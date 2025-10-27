#' Get an audit record
#' 
#' @param x An audit record or record ID
#' @param store Audit trail storage
#' 
#' @return A \link[cxaudit]{cxaudit_record} object
#' 
#' @description
#' Retrieve an audit record by reference, either using the audit record 
#' identifier or \link[cxaudit]{cxaudit_record}.
#' 
#' If no audit record exists with reference `x` or an error occurred when 
#' retrieving the record, invisible `NULL` is returned. An invisible `NULL` is 
#' also returned if auditing is disabled.
#' 
#' @export

cxaudit_get <- function( x, store = cxaudit::cxaudit_store() ) {
  
  

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
  
  
  # -- record identifier

  if ( missing(x) || is.null(x) || (length(x) != 1) || ! inherits( x, c( "character", "cxaudit_record" ) ) ) 
    stop( "The specified record identity or reference is missing or invalid" )

  rec_id <- character(0)
  
  if ( inherits( x, "character") )
    rec_id <- base::trimws(x)
  
  
  if ( inherits( x, "cxaudit_record") )
    if ( "id" %in% base::names(x$getproperties()) ) 
      rec_id <- x$getproperties()[["id"]]

    
  if ( ! uuid::UUIDvalidate(rec_id) )
    stop( "The record identifier is invalid" )
  
    
  
  # -- retrieve record from store
  
  rec_obj <- try( store$get( rec_id ), silent = mode_silent )
  
  if ( inherits( rec_obj, "try-error" ) )
    stop( "A storage connection error occurred when retrieving audit record" )
  
  
  
  return(invisible(rec_obj))
}