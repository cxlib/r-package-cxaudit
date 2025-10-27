#' Utility class to manage the audit trail fail cache
#' 
#' @field .attr Internal storage of fail cache attributes
#' 
#' @description
#' The utility class represents the audit record fail cache. The fail cache 
#' is a disk-based storage of audit records to capture records that fail to 
#' commit to the configured audit trail storage.
#' 
#' Audit fail cache is enabled by the app property `AUDIT.FAILCACHE` equal to 
#' `enable` or `enabled`. 
#' 
#' The fail cache path is set using the app property `AUDIT.FAILCACHE.PATH`, 
#' which must exists.
#' 
#' The method `isenabled` returns `TRUE` if the fail cache is enabled and the 
#' fail cache directory path exists. Otherwise, `FALSE`.
#'
#' The method `isempty` returns `TRUE` if the fail cache is empty. A non-empty
#' fail cache contains one or more JSON files (`.json` file extension) exists 
#' in the sub-directories of fail cache directory `AUDIT.FAILCACHE.PATH`. The 
#' method does not validate that the JSON file represents a valid audit record.
#' 
#' The `cache` method stores the specified audit records `x` in the fail cache.
#' If `x` is a list of audit records, the record relationships are preserved. 
#' 
#' The method `push` commits the audit records to the audit record store 
#' as specified or returned by \link[cxaudit]{cxaudit_store}. If a commit of a
#' record is successful, the record is deleted from the fail cache. An import
#' event is added to each commit of audit records.  
#' 
#'  
#' 
#' @exportClass cxaudit_failcache
#' @export cxaudit_failcache

cxaudit_failcache <- methods::setRefClass( "cxaudit_failcache",
                                           fields = list( ".attr" = "list" ) )



cxaudit_failcache$methods( "new" = function() {
})



cxaudit_failcache$methods( "initialize" = function() {
  "Initialize audit fail cache store"
  
  # -- initialize audit trail store internal references
  .self$.attr <- list( "enabled" = FALSE )
  
  
  # -- configuration
  cfg <- cxapp:::.cxappconfig()
  
  
  # - fail cache enabled/disabled
  #   note AUDIT is not equal to enable or enabled
  #   note AUDIT.FAILCACHE is not equal to enable or enabled
  if ( ! inherits( cfg$option( "audit", unset = FALSE ), "logical" ) || ! cfg$option( "audit", unset = FALSE ) ||
       ! inherits( cfg$option( "audit.failcache", unset = FALSE ), "logical" ) || ! cfg$option( "audit.failcache", unset = FALSE ) ) 
    return()
  
  .self$.attr[["enabled"]] <- TRUE
  
  
  
  # - fail cache disk storage
  
  if ( base::trimws( cfg$option("audit.failcache.path", unset = "", as.type = FALSE ) ) == "" )
    stop( "Audit fail cache path not configured" )
  
  if ( ! dir.exists( cfg$option("audit.failcache.path", unset = NA ) ) )
    stop( "Audit fail cache directory does not exist" )
  
  # note: using head() as typed path property permits delimited list of paths
  .self$.attr[["path"]] <- utils::head( cfg$option( "audit.failcache.path" ), n = 1 )
  
})



cxaudit_failcache$methods( "isenabled" = function() {
  "Assert if audit fail cache store is enabled"
  
  if ( all( c( "enabled", "path") %in% base::names(.self$.attr)) &&
       .self$.attr[["enabled"]] &&
       ( base::nchar(base::trimws(.self$.attr[["path"]])) > 0 ) &&
       dir.exists( .self$.attr[["path"]] ) )
    return(invisible(TRUE))

  
  return(invisible(FALSE))  
})




cxaudit_failcache$methods( "isempty" = function() {
  "Assert if audit fail cache store is empty"
  
  if ( ! .self$isenabled() )
    return(TRUE)
  
  return(invisible( length(list.files( .self$.attr[["path"]], pattern = "\\.json$", recursive = TRUE, include.dirs = FALSE )) == 0 ))
})



cxaudit_failcache$methods( "cache" = function( x ) {
  "Cache audit records in the audit fail cache"
  
  if ( ! .self$isenabled() )
    stop( "The audit record fail cache is not enabled" )

  
  if ( missing(x) || is.null(x) || ! inherits( x, c( "list", "cxaudit_record") ) )
    stop( "A list of or an single audit record not specified" )
  
  
  # - futility
  if ( inherits(x, "list") && (length(x) == 0) )
    return(invisible(TRUE))
  
  
  # -- cache directory
  
  cache_path <- file.path( .self$.attr[["path"]], 
                           base::format( Sys.Date(), format = "%Y-%m-%d"), 
                           uuid::UUIDgenerate(), fsep = "/" )
  
  if ( ! dir.exists( cache_path ) && ! dir.create( cache_path, recursive = TRUE ) )
    stop( "Could not create cache transaction directory" )
  
  
  # -- process records

  # - standard input
  lst <- list()
  
  if ( inherits( x, "cxaudit_record" ) && ( as.character(attr( class(x), "package")) == "cxaudit") )
    lst[[1]] <- x
  
  if ( inherits( x, "list") )
    for (xitem in x)
      if ( inherits( xitem, "cxaudit_record" ) && ( as.character(attr( class(xitem), "package")) == "cxaudit") )
        lst[[ length(lst) + 1 ]] <- xitem

  if ( length(lst) == 0 )
    return(invisible(FALSE))
  

  # - process each record in list 
  
  recs <- list()        
  
  
  for ( xitem in lst )  {
    
    # - process properties
    xobj <- xitem$getproperties()

    # note: URL encode label
    xobj[["label"]] <- utils::URLencode( xobj[["label"]], reserved = TRUE )
    
    # note: format date/time
    xobj[["datetime"]] <- format( xobj[["datetime"]], format = "%Y-%m-%dT%H:%M:%S" )
    
    # - process attributes    
    xobj_attr <- as.list( utils::URLencode( xitem$getattributes(), reserved = TRUE ) )
    
    if ( length(xobj_attr) > 0 ) {
      base::names(xobj_attr) <-  base::names(xitem$getattributes())
      xobj[["attributes"]] <- xobj_attr
    }
      

    recs[[ xobj[["id"]]] ] <- xobj    
  }
  
  
  
  # - save records
  
  lck <- file.path( cache_path, "failcache.lck", fsep = "/" )
  
  if ( inherits(try( base::writeLines( base::basename(cache_path), con = lck ) ), "try-error") )
    stop( "Could not create cache lck file" )
  
  
  for ( xid in base::names(recs)) 
    if ( inherits( try( base::writeLines( jsonlite::toJSON( recs[[xid]], pretty = TRUE), 
                                          con = file.path( cache_path, paste0( xid, ".json" ), fsep = "/" ) ) ), 
                   "try-error" ) )
      stop( "Failed to write record to fail cache directory" )

  
  if ( file.exists(lck) )
    base::file.remove(lck)
  

  return(invisible(TRUE))
})




cxaudit_failcache$methods( "push" = function( store = cxaudit::cxaudit_store() ) {
  "Push content of audit fail cache to audit store"
  
  
  if ( ! .self$isenabled() )
    stop( "The audit record fail cache is not enabled" )
  
  
  # -- futility
  if ( .self$isempty() )
    return(TRUE)
  
  
  # -- collect records
  
  cached_recs <- list()

  # - locked caches
  lck_caches <- base::unique(base::dirname( list.files( .self$.attr[["path"]], pattern = "\\.lck$" , full.names = TRUE, recursive = TRUE, include.dirs = FALSE ) ))
  
  # - caches  
  lst_caches <- base::unique(base::dirname( list.files( .self$.attr[["path"]], pattern = "\\.json$" , full.names = TRUE, recursive = TRUE, include.dirs = FALSE ) ))
  lst_caches <- lst_caches[ ! lst_caches %in% lck_caches ]

  
  # - clear push lock (files)
  #   note: really only used in code crash
  on.exit({

    base::unlink( file.path( lst_caches, "push.lck", fsep = "/" ), recursive = TRUE, force = TRUE )    

  }, add = TRUE, after = FALSE )
  
  
  
  for ( xcachepath in lst_caches ) {
    
    xcache <- base::basename(xcachepath)
    
    # note: failcache.lck is the cache write lock
    # note: push.lck is the cache push lock
    if ( file.exists( file.path( xcachepath, "failcache.lck", fsep = "/" ) ) ||
         file.exists( file.path( xcachepath, "push.lck", fsep = "/" ) ) )
      next()
    
    
    # - set push lock
    if ( inherits(try( base::writeLines( "push lock", con = file.path( xcachepath, "push.lck", fsep = "/" ) )), "try-error" ) )
      next()
    

    # - initiate list of cache objects    
    
    if ( ! xcache %in% base::names(cached_recs) )
      cached_recs[[ xcache ]] <- list()
    
    
    transient_objs <- c( "lst", "xobj" )

    for ( xfile in list.files( xcachepath, pattern = "\\.json$", full.names = TRUE ) ) {
      
      # - clean start
      # base::rm( list = c( "lst", "xobj" ) )
      base::rm( list = transient_objs[ transient_objs %in% base::ls()] )

      
      # - read json
      lst <- try( jsonlite::fromJSON( xfile ) )
      
      if ( inherits( lst, "try-error") ) {
        cached_recs[[ xcache ]] <- list()
        break()
      }
      
      
      # - re-code typed properties

      if ( "label" %in% base::names(lst) )
        lst[["label"]] <- utils::URLdecode( lst[["label"]] )

      if ( "datetime" %in% base::names(lst) )
        lst[["datetime"]] <- as.POSIXct( lst[["datetime"]], format = "%Y-%m-%dT%H:%M:%S", tz = "UTC" )
      

      # - initialize audit record with save properties
      xobj <- try( cxaudit::cxaudit_record( lst[ cxaudit:::.cxaudit_propertynames() ] ) )
      
      if ( inherits( xobj, "try-error" ) ) {
        cached_recs[[ xcache ]] <- list()
        break()
      }
      
      
      # - add attributes
      if ( "attributes" %in% base::names(lst) ) {

        attr_names <- base::names( lst[["attributes"]] )
        
        attrs <- utils::URLdecode( base::unlist( lst[[ "attributes" ]], use.names = TRUE) )
        base::names(attrs) <- attr_names

        if ( inherits(try( xobj$setattributes( attrs )), "try-error" ) ) {
          cached_recs[[ xcache ]] <- list()
          break()
        }
        
      } # end of if-statement for attributes

      
      # - add record to list of caches
      cached_recs[[ xcache ]][[ length(cached_recs[[ xcache ]]) + 1 ]] <- xobj

    }  # end of for-loop across files in cache directory   

    
    # - no records identified 
    if ( length(cached_recs[[ xcache ]]) == 0 )
      next()
    
    
    # - add import event
    
    cache_files <- list.files( xcachepath, pattern = "\\.json$", full.names = TRUE )
    
    cache_file_hashes <- sapply( list.files( xcachepath, pattern = "\\.json$", full.names = TRUE ), function(x) {
      digest::digest( x, algo = "sha1", file = TRUE )
    })
    
    cache_hash <- digest::digest( base::sort(cache_file_hashes), algo = "sha1", file = FALSE )

    
    cfg <- cxapp:::.cxappconfig()
    
    cache_env <- utils::head( c( cfg$option( "environment", unset = NULL ), 
                                 cfg$option( "audit.environment", unset = NULL ), 
                                 as.character(Sys.info()[["nodename"]]) ), n = 1 )


    # main record
    cache_obj <- cxaudit::cxaudit_record( list( "event" = "import", 
                                                "object.type" = "audit.record", 
                                                "object.class" = "audit.record",
                                                "object.path" = .self$.attr[["path"]],
                                                "object.hash" = cache_hash,
                                                "label" = "Import of audit fail cache records",
                                                "actor" = as.character(Sys.info()[["user"]]),
                                                "env" =  cache_env ) )
    
    # add record count to attributes
    cache_obj$setattributes( c( "record.count" = as.character(length(cache_files)) ) )
    
    # cache configuration properties
    cache_obj_attr <- sapply( c( "audit", "audit.store", "audit.failcache", "audit.failcache.autocommit" ), function(x) {
      cfg$option( x, unset = "not set", as.type = FALSE )
    }, USE.NAMES = TRUE )

    # qualify attribute as a property setting
    base::names(cache_obj_attr) <- paste0( base::names(cache_obj_attr), ":property" ) 
    
    # add qualified attributes    
    cache_obj$setattributes( cache_obj_attr )
    

    cached_recs[[ xcache ]][[ length(cached_recs[[ xcache ]]) + 1 ]] <- cache_obj
   
    
  }  # end of for-loop across cache directories
  

  # -- commit records
  
  for ( xitem in base::names(cached_recs) ) {
    
    # - nothing to do
    if ( length(cached_recs[[ xitem]]) == 0 )
      next()
    
    # - attempt commit
    #   note: if commit fails ... leave as is
    if ( inherits( try( store$commit( cached_recs[[ xitem]] )), "try-error" ) )
      next()
    
    
    # - remove cache
    xpaths <- lst_caches[ grepl( paste0(".*/", xitem, "$"), lst_caches, perl = TRUE ) ]
    base::unlink( xpaths, recursive = TRUE, force = TRUE )

    
    # - remove cache parent (if empty)
    #   note: assuming fail cache block has a parent bucket/bin
    #   note: using for-loop for unexpected multiple match
    for ( xp in xpaths )
      if ( ( base::dirname(xp) != .self$.attr[["path"]] ) &&
           ( length(list.dirs( base::dirname(xp), recursive = FALSE )) == 0 ) )
        base::unlink( base::dirname(xp), recursive = TRUE, force = TRUE )

  } # end of for-loop across cached record blocks
  

  
  return(invisible(TRUE))
})



cxaudit_failcache$methods( "show" = function() {
  "Display audit fail cache information"
  
  if ( ! .self$.attr[["enabled"]] ) {
    cat( "Audit record fail cache is disabled", sep = "\n" )
    return()
  }
  
  
  info <- c( "Audit record fail cache",
             paste( base::rep_len( "-", 60), collapse = ""),
             paste( "Path ", .self$.attr[["path"]] ) )

  
  cat( c( info, "" ), sep = "\n" )
  
})


