#' Utility function to commit audit records 
#' 
#' @param x List of audit records
#' @param store Audit trail storage
#' 
#' @return Invisible logical of result
#' 
#' @description
#' Commits a \link[cxaudit]{cxaudit_record} `x`, or a list of records, to the 
#' specified/configured storage `store` if auditing is enabled.
#' 
#' Auditing is enabled with the app property `AUDIT` equal to `enable` or 
#' `enabled`, case insensitive.
#' 
#' If `store` is not specified, the default storage configuration using 
#' is used. See \link[cxaudit]{cxaudit_store} for further details.
#' 
#' If commit of records to the audit trail store or an error occurs, the 
#' records are cached in the audit trail fail cache, if the fail cache is 
#' enabled. See \link[cxaudit]{cxaudit_failcache} for configuration details.
#' 
#' If the audit trail fail cache is enable, not empty and the fail cache 
#' auto-commit is enabled, any records in the fail cache are pushed to the 
#' audit trail store. Note that multiple audit trail stores using with the same 
#' fail cache will use the `store` used for this specific commit.
#' 
#' Audit fail cache auto-commit is enabled with the app property 
#' `AUDIT.FAILCACHE.AUTOCOMMIT` equal to `enable` or `enabled`, case 
#' insensitive.
#' 
#' The function returns `TRUE` if the commit is successful to `store` or the 
#' audit fail cache. If auditing is enabled and commit to `store` or fail cache
#' fails, an error is returned, i.e. the intent to save audit records fails. 
#' 
#' If auditing is not enabled, `NULL` is returned.
#' 
#' @export

cxaudit_commit <- function( x, store = cxaudit::cxaudit_store() ) {

  
  if ( missing(x) || is.null(x) || ! inherits( x, c( "list", "cxaudit_record" ) ) )
    stop( "Audit records missing or invalid" )


  # -- configuration
  
  cfg <- cxapp:::.cxappconfig()


  # - audit is disabled   
  #   note: config option AUDIT is not defined or not equal to enable or enabled
  #   note: config option AUDIT equal to disable or disabled
  if ( is.na( cfg$option( "audit", unset = NA ) ) ||
       ! is.logical( cfg$option( "audit", unset = FALSE ) ) ||
       ! cfg$option( "audit", unset = FALSE ) )
    return(invisible(NULL))
    
  # - debug mode
  mode_silent <- ! cfg$option( "mode.debug", unset = FALSE )
  

  # - list of records
  
  lst <- list()
  
  if ( inherits( x, "list") ) 
    lst <- append( lst, x )
  
  if ( inherits( x, "cxaudit_record") )
    lst[[1]] <- x

  if ( length(lst) == 0 )  
    stop( "Could not determine list of records" )
  

  # -- assert all are valid records
  for ( xitem in lst ) {
    
    if ( ! inherits( xitem, "cxaudit_record") || is.null(attr(class(xitem), "package")) || ( attr( class(xitem), "package") != "cxaudit" ) )
      stop( "One or more specified records are of an invalid type" )

    if ( any(is.na( xitem$getproperties() )) )
      stop( "One or more incomplete records specified" )

    # note: all properties should be single entries
    for ( xprop in cxaudit:::.cxaudit_propertynames() )
      if ( length( xitem$getproperties()[[ xprop ]] ) != 1 )
        stop( "One or more record properties has a value ")
    
        
    if ( ! "id" %in% base::names(xitem$getproperties()) || ! uuid::UUIDvalidate(xitem$getproperties()[["id"]]) )
      stop( "One or more records has an invalid record identifier" )
    
    if ( ! "datetime" %in% base::names(xitem$getproperties()) || ! inherits( xitem$getproperties()[["datetime"]], c( "POSIXct", "POSIXlt", "POSIXt" ) ) )
      stop( "One or more records has a date/time reference of an invalid type" )

    if ( any( ! cxaudit::cxaudit_validreference( base::unlist(xitem$getproperties()[ c( "object.type", "object.class", "object.hash", "actor", "env" ) ], use.names = FALSE) )) )
      stop( "One or more property reference are invalide" )

    
    # note: a path is a / delimited string of valid references
    # note: strsplit( "/some/path", "/" ) will return an empty string triggered by the leading slash
    # note: base::substring(x, 2) is to ignore leading slash as an empty string is not a valid references
    if ( ! "object.path" %in% base::names(xitem$getproperties()) || 
         ! inherits( xitem$getproperties()[["object.path"]], "character" ) ||
         ( base::nchar(base::trimws(xitem$getproperties()[["object.path"]])) == 0 ) ||
         ( ( base::trimws(xitem$getproperties()[["object.path"]]) != "/" ) &&
           any( ! cxaudit::cxaudit_validreference( base::unlist( base::strsplit( base::substring(base::trimws(xitem$getproperties()[["object.path"]]), 2), "/", fixed = TRUE ), use.names = FALSE ) ) ) ) )
      stop( "One or more object path properties are invalid" )

    if ( any( ! cxaudit::cxaudit_validreference( base::unlist(xitem$getproperties()[ c( "object.type", "object.class", "object.hash", "actor", "env" ) ], use.names = FALSE) )) )
      stop( "One or more property reference are invalide" )

  }


  # -- fail cache
  fail_cache <- cxaudit::cxaudit_failcache()
  
    
  # -- audit commits
  commit <- try( store$commit( lst ), silent = mode_silent )

  
  # - on commit error 
  if ( inherits( commit, "try-error" ) ) {

    if ( ! fail_cache$isenabled() )
      stop( "Audit commit failed and fail cache is not enabled" )

    if ( inherits( try( fail_cache$cache( lst ), silent = mode_silent ), "try-error" ) )
      stop( "Audit commit and fail cache failed" )

    return(invisible(TRUE))
  }  #  end of if-statement for commit failure 
    
    
  # -- fail cache auto-commit
  if ( ! fail_cache$isempty() &&
       ! is.na( cfg$option( "audit.failcache.autocommit", unset = NA ) ) &&
       is.logical( cfg$option( "audit.failcache.autocommit", unset = FALSE ) ) &&
       cfg$option( "audit.failcache.autocommit", unset = FALSE ) )
    fail_cache$push( store = store )
  

  return(invisible(TRUE))
}
