#' List audit records
#' 
#' @param ... Filter conditions as named parameters or as a single list of named elements
#' @param store Audit trail storage
#' 
#' @return A list of \link[cxaudit]{cxaudit_record} entries
#' 
#' @description
#' Retrieve a list of audit records that satisfy the specified criteria from the 
#' specified/configured storage `store` if auditing is enabled. If auditing is 
#' disabled or an error occurs, an invisible `NULL` is returned.
#' 
#' The following filter criteria can be specified as named entries in a list or
#' as named arguments. Specifying filter criteria as both a list of entries and
#' named arguments are not supported. 
#' 
#' \itemize{
#'   \item `events` the audited event (see \link[cxaudit]{cxaudit_record} for valid values)
#'   \item `object.types` the type of the audited object
#'   \item `object.classes` further classifies the object `type`
#'   \item `object.parentpaths` the parent of the object path
#'   \item `object.names` the object name
#'   \item `object.hashes` the object hash
#'   \item `actors` are the services and/or users associated with an audited event
#'   \item `envs` as the environments associated with an audited event
#'   \item `from` is start of the period for selecting audit records by their corresponding date
#'   \item `to` is end of the period for selecting audit records by their corresponding date
#'   \item `limit` is the number of records to retrieve
#'   \item `offset` is the first record number to select from
#'   \item `select` is the direction from which the number of records is taken
#' }
#' 
#' The returned record selection satisfies all filter criteria analogous to the 
#' \emph{and} operator. 
#'  
#' The filter criteria `object.parentpaths` and `object.names` can be used to 
#' filter on a particular object by its path, i.e. the concatenation of object 
#' parent path and name.  
#' 
#' The date range specified by `from` and `to` are specified as a date or date 
#' and time using \link[base]{Sys.Date}, \link[base]{Sys.time} or
#' \link[base]{as.POSIXct} or \link[base]{as.POSIXlt}. The `to` and `from` 
#' values are converted to UTC timezone truncated to number of seconds. Partial
#' dates are currently not supported.
#' 
#' If `select = first`, the records selected are those in order of chronological 
#' occurrence by record date and time.
#' 
#' If `select=last` (default), records are selected in order of occurrence by
#' descending date and time (starting with the last record first).
#' 
#' The options `limit` and `offset` can be used to only return a set number of 
#' records and/or page through the records returned. The `select` option is 
#' applied before limiting and offsetting any records.
#' 
#' Note that if there are multiple records for the same date and time and the 
#' selection boundary set by `limit`, `offset` and `select` would mean that some
#' records fall within the span of `limit` and `offset` and some \emph{valid} 
#' records fall outside of that boundary, i.e. not included, then the cut-off 
#' is arbitrary. There is no indicator or notification that valid records were 
#' excluded. To ensure that all the records for the given period of interest
#' is included, increase the number of records specified by limit to ensure that
#' all the records for a given period is returned.
#' 
#' The options `from`, `to`, `limit`, `offset` and `select` can only be a single
#' value. All other options is a vector of one or more elements. 
#' 
#' All filter criteria are disabled with the last 300 records from the
#' last 30 days returned by default. 
#' 
#' The attribute `filter` of the returned list of records specifies the filter
#' criteria resulting in the selected list. See \link[base]{attr} for details on 
#' accessing `filter` attribute.
#' 
#' If this function is used in conjunction with pagination, the filter attributes
#' returned with the record selection can be used to ensure that the next page by
#' `limit` and `offset` are not affected by new records.
#' 
#' 
#' @export

cxaudit_list <- function( ..., store = cxaudit::cxaudit_store() ) {
  
  # -- supported filter conditions
  supported_filters <- c( "events", 
                          "object.types", "object.classes", "object.parentpaths", "object.names", "object.hashes", 
                          "actors", "envs", 
                          "from", "to", 
                          "limit", "offset", 
                          "select")
  
  
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
  
  
  # -- inputs
  filter_lst <- list(...)
  
  # - input as a named list of entries
  if ( ( length(filter_lst) == 1 ) && inherits( filter_lst[[1]], "list" ) )
    filter_lst <- filter_lst[[1]]

  if ( ( length(filter_lst) > 0 ) && 
       ( is.null(base::names(filter_lst)) || any( base::trimws(base::names(filter_lst)) == "" ) ) )
    stop( "All filter criteria should be named" )

  # - standardize on lower case names
  base::names(filter_lst) <- base::tolower(base::trimws(base::names(filter_lst)))
    

  # - defaults
  
  if ( ! "limit" %in% base::names(filter_lst) || is.null(filter_lst[["limit"]]) || is.na(filter_lst[["limit"]]) )
    filter_lst[["limit"]] <- 300

  if ( ! "offset" %in% base::names(filter_lst)  || is.na(filter_lst[["offset"]]) )
    filter_lst[["offset"]] <- NULL

  if ( ! "select" %in% base::names(filter_lst) )
    filter_lst[["select"]] <- "last"

  
  # - single value vs vector
  for ( xprop in c( "from", "to", "limit", "offset", "select" ) )
    if ( xprop %in% base::names(filter_lst) && (length(filter_lst[[ xprop ]]) > 1 ) )
      stop( "Filter criteria ", xprop, " can only be a single value" )
  

  # - filter criteria limit and offset is numeric 
  for ( xprop in c( "limit", "offset" ) )
    if ( xprop %in% base::names(filter_lst) && 
         ! inherits( filter_lst[[ xprop ]], "numeric" ) )
      stop( "Expecting filter criteria ", xprop, " to be a numeric vector" )

  
  # - filter criteria select is a keyword
  if ( "select" %in% base::names(filter_lst) &&
       ( ! inherits( filter_lst[["select"]], "character" ) ||
         ! base::tolower(base::trimws(as.character(filter_lst[["select"]]))) %in% c( "first", "last" ) ) )
    stop( "Select criteria is not equal to the keyword first or last" )
  
  filter_lst[["select"]] <- base::tolower(base::trimws(as.character(filter_lst[["select"]])))
  
  
  # - filter criteria on references is a character vector 
  for ( xprop in c( "events", "object.types", "object.classes", "object.names", "object.hashes", "actors", "envs" ) )
    if ( xprop %in% base::names(filter_lst) && 
         ! inherits( filter_lst[[ xprop ]], "character" ) )
      stop( "Expecting filter criteria ", xprop, " to be a character vector" )
  

  # - events are valid
  if ( "events" %in% base::names(filter_lst) && 
       ! cxaudit::cxaudit_validevent( as.character(filter_lst[["events"]]) ) )
    stop( "One or more events in filter criteria are invalid" )
  
    
  # - references 
  for ( xprop in c( "object.types", "object.classes", "object.names", "object.hashes", "actors", "envs" ) )
    if ( xprop %in% base::names(filter_lst) && 
         any( ! cxaudit::cxaudit_validreference( filter_lst[[ xprop ]] ) ) )
      stop( "One or more values in ", xprop, " filter critera are not valid references" )
  
  

  # - standardize date range
  
  for ( xprop in c( "from", "to" ) )
    if ( xprop %in% base::names(filter_lst) ) {
      
      std_rannge <- try( trunc.POSIXt( as.POSIXct( filter_lst[[xprop]], tz = "UTC" ), units = "secs" ), silent = mode_silent )
      
      if ( inherits( std_rannge, "try-error" ) )
        stop( "Could not standardize filter property ", xprop )
      
      filter_lst[[ xprop ]] <- std_range
      
      std_range <- NULL
    }
  
  
  
  # -- retrieve list of records
  
  lst <- try( store$records( filter_lst ), silent = mode_silent )

  if ( inherits( lst, "try-error" ) ) 
    stop( "A storage connection error occurred when retrieving list of records" )
 
  
   
  return(invisible(lst))
}