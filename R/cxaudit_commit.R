#' Utility function to commit audit records 
#' 
#' @param x List of audit records
#' 
#' @return Invisible logical of result
#' 
#' @description
#' Commits the audit records to the auditor service instance
#' 
#' The auditor service connection is configured using the app properties 
#' 
#' \itemize{
#'  \item `AUDITOR.URL` defines the URL for the service
#'  \item `AUDITOR.TOKEN` defines the access token for the auditor service
#'  \item `AUDITOR.FAILCACHE` directory path to save audit records on connection 
#'        failure
#' }
#' 
#' See \link[cxapp]{cxapp_config} for details.
#' 
#' 
#' @export

cxaudit_commit <- function( x ) {


  # -- auditor configuration
  
  cfg <- cxapp::.cxappconfig()

  # - check if auditor is enabled
  if ( ! cfg$option( "auditor", unset = FALSE ) )
    return(invisible(TRUE))
  
    
  if ( any(is.na( c( cfg$option("auditor.url", unset = NA), 
                     cfg$option("auditor.token", unset = NA)) )) )
    stop( "Auditor not configured")
  
  
  
  if ( missing(x) || is.null(x) || ! inherits( x, "list") )
    stop( "List of records invalid or missing" )
  
  
  if ( length(x) == 0 )
    return(invisible(TRUE))
  
  
  lst_recs <- list()
  
  
  for (idx in 1:length(x) ) {
    
    rec_obj <- x[[idx]]
    
    if ( ! inherits( rec_obj, "cxaudit_record") )
      stop( "Record ", idx, " not an audit record class" )
  
    # - record as list
    rec_lst <- list()
      
    # - record properties
    rec_lst <- append( rec_lst, 
                       rec_obj$getproperties() )

    # - assign record ID
    rec_lst[["id"]] <- uuid::UUIDgenerate()
    
    # - assign record timestamp 
    rec_lst[["datetime"]] <- format( as.POSIXct( Sys.time(), tz = "UTC"), format = "%Y%m%dT%H%M%S" )
 
    # - record attributes
    if ( length(rec_obj$getattributes()) > 0 ) {
      
      rec_attrs <- rec_obj$getattributes()
      
      for ( xattr in rec_attrs ) 
        for ( xattr_entry in xattr ) {
          
          for ( urlencode_key in c( "label", "value", "qualifier" ) )
            if ( urlencode_key %in% base::names(xattr_entry) )
              xattr_entry[[urlencode_key]] <- utils::URLencode( xattr_entry[[urlencode_key]], reserved = TRUE )

          rec_lst[["attributes"]][[ length(rec_lst[["attributes"]]) + 1 ]] <- xattr_entry
        }
    }
    
    # - url encode label and attribute values
    rec_lst[["label"]] <- utils::URLencode( rec_lst[["label"]], reserved = TRUE )
    
    if ( "attributes" %in% base::names(rec_lst) && (length(rec_lst[["attributes"]]) > 0) )
      for ( xattr in base::names(rec_lst[["attributes"]]) )
        rec_lst[["attributes"]][[ xattr ]] <- utils::URLencode( rec_lst[["attributes"]][[ xattr ]], reserved = TRUE )

    
    # - add to list of records
    lst_recs[[ length(lst_recs) + 1 ]] <- rec_lst    
    
  }  # end of for-statement for list of records
  

  
  
  
  # -- post records

  rslt <- try( httr2::request( cfg$option("auditor.url", unset = NA) ) |>
                 httr2::req_url_path( "/api/records") |>
                 httr2::req_method("POST") |>
                 httr2::req_auth_bearer_token( cfg$option("auditor.token", unset = NA) ) |>
                 httr2::req_body_json( lst_recs ) |>
                 httr2::req_perform(), 
               silent = FALSE )
  
  
  # - success
  if ( ! inherits(rslt, "try-error") && rslt$status_code == 201 )
    return(invisible(TRUE))
  
  
  # - failed
  
  if ( is.na(cfg$option("auditor.failcache", unset = NA )) )
    stop( "Committing audit records failed and no auditor fail cache is configured" )
  
  
  
  fail_cache <- gsub( "\\\\", "/", base::tempfile( pattern = paste0( "auditor-commit-failure-", 
                                                                     format( as.POSIXct( Sys.time(), tz = "UTC"), format = "%Y%m%dt%H%M%S" ), "-"),
                                                   tmpdir = cfg$option("auditor.failcache", unset = base::tempdir() ), 
                                                   fileext = "" ) )
  
  if ( ! dir.exists(fail_cache) && ! dir.create( fail_cache, recursive = TRUE ) )
    stop( "Could not create commit fail container directory" )
  
 
  for ( xrec in lst_recs ) {
    
    rec_file <- gsub( "\\\\", "/", base::tempfile( pattern = "audit-record-", tmpdir = fail_cache, fileext = ".json" ) )
    
    if ( inherits( try( base::writeLines( jsonlite::toJSON( xrec, pretty = TRUE, auto_unbox = TRUE ), 
                                          con = rec_file ), silent = FALSE ), "try-error" ) )
      stop( "Failed to cache audit record" )
    
    base::rm( list = "rec_file" )
  }
  
  
  # - log fail
  cxapp::cxapp_log( paste0( "Commit of audit records failed (record cache ", base::basename(fail_cache), ")") )
   

  return(invisible(FALSE))
}