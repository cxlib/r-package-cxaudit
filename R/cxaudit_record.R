#' Reference class representing an audit record 
#' 
#' @field .attr Internal storage of record properties and attributes
#' 
#' @method getproperties getproperties
#' @method getproperty getproperty
#' @method show show
#' 
#' @description
#' Represents an audit record.
#' 
#' The class is initialized with a vector or list of named properties.
#' \itemize{
#'   \item `event` on the object being audited
#'   \item `type` of audited object
#'   \item `class` of the audited object type
#'   \item `reference` as a common reference to the object
#'   \item `object` as a digest referring to the object
#'   \item `label` as a human readable label to the audited event
#'   \item `actor` meaning the user or service performing or initiating the event
#'   \item `env` in which the event occurred
#' }
#' 
#' The `event` is one of the supported key words `create`, `read`, `update`, 
#' `delete`, `execute`, `commit`, `lock`, `unlock`, `sign`, `connect` and 
#' `disconnect`. The events `create`, `read`, `update`, and `delete` refer to
#' the content of the object and its state. The `lock` and `unlock` events are
#' special states that are audited separately and are synonymous  to 
#' \emph{read-only} and \emph{write enabled}, respectively.
#' 
#' The `reference` is intended as a short programmatic reference to an audited 
#' item such that `reference` represents the same audited object across the 
#' process. A simple example can be `ADAE` to represent the Adverse Event data set
#' and `T_AE` as representing a standard Adverse Event summary table. 
#' 
#' The `object` is a digest reference to the object itself and is used to track 
#' audited events across environments. As an example, a file can use the file's
#' digest, hash, checksum, etc. to effectively track the version of that file 
#' across environments and actions that are registered in the audit trail. 
#' 
#' The default `object` is the SHA-1 digest of the string 
#' `<type>:<class>:<reference>`. 
#'  
#' The special properties `id` and `datetime` are assigned when the the audit 
#' record is stored and are only available for saved audit records.
#' 
#' The `getproperties` method returns a list of the record reference named 
#' properties.
#' 
#' The `getproperty` method returns the value of specified property `x`. If `x` 
#' is not defined as a property, `NULL` is returned.
#' 
#' The `setproperty` method permits setting the record properties `object` and 
#' `label` after the audit record has been created as long as the properties
#' `id` and `datetime` are not defined (key indicators of a saved audit record).
#' 
#' One or more additional attributes can be recorded as part of the audit record.
#' An attribute is a key/value pair.
#' 
#' An attribute `x` can be associated with the audit record. If the attribute 
#' `value` is `NULL`, all entries associated with the attribute `x` is dropped. 
#' The optional attribute `label` is a human readable version of `x`. A
#' qualifying keyword `qualfier` can be associated with the attribute value, 
#' such as `new`, `old`, etc. Note that the attribute name `x` does not have to 
#' be unique.
#' 
#' The `getattributes` method returns a character vector of named elements
#' representing the record attributes.
#' 
#' The `getattribute` method returns the value of specified attribute `x`. If `x` 
#' is not defined as an attribute, `NULL` is returned.
#' 
#' If the audit record represents a saved audit record, one or more \emph{linked} 
#' audit record references represents audit records that were saved to the 
#' audit trail in conjunction with the current record. The `links` method returns
#' an un-ordered list of audit record references. A new record has no links.   
#' 
#' 
#' 
#' @exportClass cxaudit_record
#' @export cxaudit_record


cxaudit_record <- methods::setRefClass( "cxaudit_record",
                                                 fields = list( ".attr" = "list" ) )




cxaudit_record$methods( "initialize" = function( x ) {
  "Initialize record representation"
  
  # note: Initialization does not take properties id and datetime as that
  #       information represents a saved record. the properties id and datetime
  #       are back-loaded into .attr
  #
  # note: new record has id and datetime as NA
  
  supported_events <- c( "create", "read", "update", "delete", "execute",
                         "commit", "lock", "unlock", 
                         "sign", "connect", "disconnect" )
  
  
  supprted_properties <- c( "id", "event", "type", "class", "reference", "object", 
                            "label", "actor", "env", "datetime" )

  
  
  
  # -- initialize internals  
  .self$.attr <- as.list( base::rep_len( NA, length(supprted_properties)) )
  base::names( .self$.attr ) <- supprted_properties
  
  .self$.attr[["attributes"]] <- list()
  
  
  # -- process x
  x_required <- c( "event", "type", "reference", "actor", "env" )
  
  if ( missing(x) || is.null(x) || any(is.na(x)) || ! inherits( x, c( "character", "list") ) ||
       is.null(base::names(x)) || ! all(x_required %in% base::tolower(base::names(x))) )
    stop( "Required initial record properties missing or in an invalid format" )
  

  # - force lower case  
  base::names(x) <- base::tolower(base::names(x))

  

  # - validate events
  if ( ! base::tolower(x[["event"]]) %in% supported_events ) 
    stop( "The event ", base::toupper(x[["event"]]), " is not a known event" )
  
  
  # - add submitted properties
  for ( xprop in base::names(.self$.attr) )
    if ( ! xprop %in% c( "id", "datetime", "attributes" ) && xprop %in% base::names(x) )
      .self$.attr[[ xprop ]]  <- base::trimws(base::unname(x[[xprop]]))
  
  
  # - lower case keywords and references
  for ( xprop in c( "event", "type", "class", "reference", "object", "actor", "env") )
    if ( ! is.na(.self$.attr[[xprop]]) )
      .self$.attr[[xprop]] <- base::tolower(base::trimws(.self$.attr[[xprop]]))
  
  
  # - add attributes if specified in x
  if ( "attributes" %in% base::names(x) ) 
    message("deal with attributes")
  
  
  
  # -- process defaults

  if ( is.na(.self$.attr[["class"]]) )
    .self$.attr[["class"]] <- .self$.attr[["type"]]
    
  if ( is.na(.self$.attr[["label"]]) )
    .self$.attr[["label"]] <- .self$.attr[["reference"]]
  
  
  if ( is.na(.self$.attr[["object"]]) )
    .self$.attr[["object"]] <- digest::digest( paste( "object", 
                                                      .self$.attr[["type"]], 
                                                      .self$.attr[["class"]], 
                                                      .self$.attr[["reference"]], 
                                                      sep = ":" ), 
                                               algo = "sha1", file = FALSE )
  
  
})




cxaudit_record$methods( "getproperties" = function() {
  "Return list of record properties"
  
  return(invisible( .self$.attr[ ! base::names(.self$.attr) %in% c( "attributes", "links") ] ))
})




cxaudit_record$methods( "getproperty" = function(x) {
  "Return a named record property"
  
  if ( missing(x) || is.null(x) || any(is.na(x)) || ! inherits(x, "character") || 
       (length(x) != 1) || (base::trimws(x) == "") || 
       ! base::tolower(base::trimws(x)) %in% base::names(.self$.attr) ||
       base::tolower(base::trimws(x)) %in% c( "attributes", "links") )
    return(invisible(NULL))
  
  return(invisible( base::unname(.self$.attr[[ base::tolower(base::trimws(x)) ]]) ))
})



cxaudit_record$methods( "setproperty" = function(x, value ) {
  "Set named record property"
  
  permitted_properties <- c( "object", "label")
  
  if ( missing(x) || is.null(x) || any(is.na(x)) || ! inherits(x, "character") || 
       (length(x) != 1) || (base::trimws(x) == "") || 
       ! base::tolower(base::trimws(x)) %in% permitted_properties )
    stop( "Property name is invalid or not permitted to be set")
  
  
  if ( missing(value) || is.null(value) || any(is.na(value)) || ! inherits(value, "character") || 
       (length(value) != 1) || (base::trimws(value) == "") )
    stop( "Property value is missing or invalid" )
       
  
  .self$.attr[[ base::tolower(x) ]] <- base::trimws(value)
  
  
  return(invisible( base::unname(.self$.attr[[ base::tolower(x) ]]) ))
})




cxaudit_record$methods( "getattributes" = function() {
  "Return list of record attributes"
  
  return(invisible( .self$.attr[["attributes"]] ))
})




cxaudit_record$methods( "getattribute" = function(x) {
  "Return a named record attribute"
  
  if ( missing(x) || is.null(x) || any(is.na(x)) || ! inherits(x, "character") || 
       (length(x) != 1) || (base::trimws(x) == "") || 
       ! base::tolower(base::trimws(x)) %in% base::names(.self$.attr[["attributes"]]) )
    return(invisible(NULL))
  
  return(invisible( .self$.attr[["attributes"]][[ base::tolower(base::trimws(x)) ]] ))
})




cxaudit_record$methods( "setattribute" = function( x, value = NULL, label = NULL, qualifier = NULL) {
  "Set record attributes"

    
  if ( missing(x) || is.null(x) || any(is.na(x)) || ! inherits(x, "character" ) ||
       (base::trimws(x) == "") )
    stop( "Attributes missing or invalid" )
  

  if ( is.null(value) && base::tolower(x) %in% base::names(.self$.attr[["attributes"]]) ) {
    .self$.attr[["attributes"]] <- .self$.attr[["attributes"]][ base::names(.self$.attr[["attributes"]]) != base::tolower(x) ]
    return(invisible(NULL))
  }
    

  
  attr_rec <- list( "key" = base::tolower(x), 
                    "value" = base::trimws(value) )
  
  if ( ! is.null(label) && ! any(is.na(label)) && inherits(label, "character") && 
       ( length(label) == 1 ) && (base::trimws(label) != "") )
    attr_rec[["label"]] <- base::trimws(label)

    
  if ( ! is.null(qualifier) && ! any(is.na(qualifier)) && inherits(qualifier, "character") && 
       ( length(qualifier) == 1 ) && (base::trimws(qualifier) != "") )
    attr_rec[["qualifier"]] <- base::trimws(qualifier)
  

  if ( ! base::tolower(base::trimws(x)) %in% base::names(.self$.attr[["attributes"]]) )
    .self$.attr[["attributes"]][[base::tolower(base::trimws(x))]] <- list()

    
  .self$.attr[["attributes"]][[base::tolower(base::trimws(x))]] <- append( .self$.attr[["attributes"]][[base::tolower(base::trimws(x))]], 
                                                                           list( attr_rec ) )

  return(invisible( attr_rec ))
})





cxaudit_record$methods( "show" = function() {
  "Display record representation"
  
})