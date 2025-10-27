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
#'   \item `object.type` as the type of audited object
#'   \item `object.class` as the class of the audited object type
#'   \item `object.path` as a path reference to the audit object
#'   \item `object.hash` as a digest or hash referring to the object definition or content
#'   \item `label` as a human readable label to the audited event
#'   \item `actor` meaning the user or service performing or initiating the event
#'   \item `env` the environment where the audited event occurred
#' }
#' 
#' The special properties `id` and `datetime` can be assigned when the the audit 
#' record is initialized and should only be included when the audit record
#' represents saved audit records.
#' 
#' The `event` is one of the supported event types. The events `create`, `read`,
#'  `update`, `delete` and `commit` refer to the content of the audited 
#' object. Note that the `commit` event may refer to an object being created or 
#' updated. 
#' 
#' The `execute` and `fail` events are intended to represent audited processing
#' events on or with objects. The `fail` event is not expected to used to capture
#' and record errors and warnings, unless they are included as the fail record
#' attributes.
#' 
#' The events `lock`, `unlock` and `sign` are representative of common audited 
#' compliance states with the first two synonymous to \emph{read-only} and
#' \emph{write enabled}, respectively. The `sign` event by itself is not by 
#' default a verifiable sign event, but can include additional attributes to 
#' corroborate the sign event.
#' 
#' The events `connect` and `disconnect` are provided to capture audited connections, 
#' such as login's, logout's and other types and styles of connections. 
#' 
#' An audit object is referred to by a representative path `object.path` such that
#' the last level of the path is the object name and the preceding level represents 
#' a parent hierarchy, akin to a parent directory or folder. The levels of the path
#' are valid references (\link{cxaudit_validreference}).
#' 
#' The object can be further identified by its `object.type` and `object.class`
#' where the type is a generic reference to the type of object and class further
#' classifies the type. The type and class are valid references 
#' (\link{cxaudit_validreference}).
#' 
#' The `object.hash` is a representation of the object definition or content such
#' that the same object stored under different path references or across environments
#' represent the same object, i.e. enabling auditing and tracking objects across 
#' environments (think of all the file transfers in today's modern processes).
#' 
#' The `object.hash`, if not specified, is the SHA-1 digest of the string 
#' `<object.type>:<object.class>:<name>` and where \emph{name} is the last level of  
#' `object.path`. 
#' 
#' The `getproperties` method returns a list of the record reference named 
#' properties. 
#' 
#' One or more additional attributes can be recorded as part of the audit record.
#' An attribute is a value identified by a keyword. The attribute value can also
#' include additional context or scope, such as \emph{new} or \emph{old}, using
#' a qualifier.
#' 
#' The `setattributes` methods can be used to associate attributes with the 
#' audit record. The vector `x` is a named character vector where the name of 
#' the vector element is the attribute name and the vector element value is the 
#' attribute value. A qualifier can be included in the entry name using the convention 
#' `<name>:<qualifier>` where both the name and qualifier are valid references 
#' (\link{cxaudit_validreference}). 
#' 
#' The `getattributes` method returns a character vector of named elements
#' representing the record attributes. The qualifier is omitted if it is not
#' defined or equal to the default `value`.
#' 
#' 
#' 
#' @exportClass cxaudit_record
#' @export cxaudit_record


cxaudit_record <- methods::setRefClass( "cxaudit_record",
                                                 fields = list( ".attr" = "list" ) )



cxaudit_record$methods( "new" = function() {
  "New record"
})



cxaudit_record$methods( "initialize" = function( x ) {
  "Initialize record representation"

  
  # -- initiate internal storage
  
  # - properties
  .self$.attr <- as.list(base::rep_len( NA, length( cxaudit:::.cxaudit_propertynames() ) ))
  base::names(.self$.attr) <- cxaudit:::.cxaudit_propertynames()
  
  # note: record is initialized with an ID and date/time stamp assuming this is 
  #       a new audit record
  .self$.attr[["id"]] <- uuid::UUIDgenerate()
  .self$.attr[["datetime"]] <- as.POSIXct( trunc.POSIXt( Sys.time(), units = "secs" ), tz = "UTC" )
  
  
  # - attributes
  .self$.attr[["attributes"]] <- character(0)
  
  # - links
  .self$.attr[["links"]] <- list()
  
  


  # -- empty initialization
  if ( missing(x) ) 
    return()


  # -- invalid x
  if ( is.null(x) || (length(x) == 0) || all(is.na(x)) || ! inherits( x, c( "character", "list") ) )
    stop( "Required initial record properties missing or invalid" )
  
    

  # -- initialized with inputs 
  #    note: overriding ID and datetime assumes the developer is in full control of
  #          the audit record integrity

  supported_events <- cxaudit:::.cxaudit_eventnames()
  supprted_properties <- cxaudit:::.cxaudit_propertynames()


  
  if ( is.null(base::names(x)) || any( base::trimws(base::names(x)) == "" ) )
    stop( "One or more entries are not named" )
  
  

  # -- standardize names to lower case
  
  x_names <- base::trimws(base::tolower(base::names(x)))
  base::names(x) <- x_names
  

  if ( any( ! x_names %in% c( "attributes", "links", cxaudit:::.cxaudit_propertynames() ) ) )
    stop( "One or more invalid property names" )


  # -- properties
  
  # - assign ID if specified
  if ( "id" %in% x_names ) {
    
    if ( is.null(x[["id"]]) || is.na(x[["id"]]) || ! inherits(x[["id"]], "character") ||  ! uuid::UUIDvalidate(x[["id"]]) )
      stop( "Property id not in a valid format" )
    
    .self$.attr[["id"]] <- base::unname(x[["id"]])
    
  }

  
  # - assign datetime if specified
  if ( "datetime" %in% x_names ) {

    if ( ! inherits(x[["datetime"]], c( "POSIXct", "POSIXt" ) ) )
      stop( "Property datetime not in a valid format" )
    
    .self$.attr[["datetime"]] <- as.POSIXct(trunc.POSIXt(x[["datetime"]], units = "secs"), tz = "UTC")
    
  }


  # - set edit enabled properties
  .self$setproperties( base::unlist( x[ ! base::names(x) %in% c( "id", "datetime", "attributes", "links" ) ], use.names = TRUE ) )

  
})



cxaudit_record$methods( "setproperties" = function(x) {
  "Set record properties"
  
  
  if ( missing(x) || is.null(x) || any(is.na(x)) || (length(x) == 0) || ! inherits( x, c( "character", "list" ) ) ||
       is.null(base::names(x)) || any( base::trimws(base::names(x)) == "" ) )
    stop( "Properties are missing or in an invalid format" )
  
  
  # -- lower case property names
  
  x_names <- base::tolower(base::trimws(base::names(x)))
  base::names(x) <- x_names
  
  
  
  # -- valid properties
  
  if ( any( base::tolower(base::trimws(base::names( x ))) %in% c( "attributes", "links") ) )
    stop( "Record attributes and links are not record properties" )
  
  if ( any( base::tolower(base::trimws(base::names( x ))) %in% c( "id", "datetime") ) )
    stop( "Record properties id and datetime cannot be set after initialization" )
  
  
  props <- cxaudit:::.cxaudit_propertynames()[ ! cxaudit:::.cxaudit_propertynames() %in% c( "id", "datetime" ) ]
  
  if ( any( ! base::tolower(base::trimws(base::names( x ))) %in% props ) )
    stop( "One or more unknown/unsupported properties submitted" )

  
  for ( xprop in props ) {
    
    # note: use strategy to select from known properties
    
    if ( ! xprop %in% base::names(x) )
      next()

    
    if ( is.null(x[[xprop]]) || is.na(x[[xprop]]) || ( base::trimws(x[[xprop]]) == "" ) )
      stop( "Value of property ", xprop, " missing or an empty string" )
    
        
    if ( xprop == "event" && ! cxaudit::cxaudit_validevent( x[[xprop]] ) )
      stop( "Event not supported" )

    
    if ( xprop %in% c( "object.type", "object.class") && ! cxaudit::cxaudit_validreference( x[[xprop]] ) )
      stop( paste( "The property", xprop, "is not a valid reference") )
    
    
    if ( xprop == "object.path" ) {
      
      xpath <- base::unlist(base::strsplit( x[[ xprop ]], "/", fixed = TRUE ), use.names = FALSE)
      
      if ( any( ! cxaudit::cxaudit_validreference( xpath[ base::trimws(xpath) != "" ] ) ) )
        stop( "The object path is invalid" )
    }
    
    
    .self$.attr[[ xprop ]] <- base::unname(x[[ xprop ]])
    
  }  #  end of for-loop across properties
  
})



cxaudit_record$methods( "getproperties" = function() {
  "Return list of record properties"

  # - integrity check
  if ( any( ! cxaudit:::.cxaudit_propertynames() %in% base::names(.self$.attr) ) )
    stop( "Audit record properties missing")

  return(invisible( .self$.attr[ which( ! is.na(.self$.attr) & ! base::names(.self$.attr) %in% c( "attributes", "links" ) ) ] ))
})




cxaudit_record$methods( "setattributes" = function( x ) {
  "Set one or more record attributes"
  
  
  if ( missing(x) || is.null(x) || any(is.na(x)) || (length(x) == 0) || ! inherits( x, "character" ) ||
       is.null(base::names(x)) || any( base::trimws(base::names(x)) == "" ) )
    stop( "Attributes are missing or in an invalid format" )
  
  
  # -- standardise names to lower case
  base::names(x) <- base::trimws(base::tolower(base::names(x)))
  

  # -- attribute names incl qualifiers as references
  for ( xattr in base::names(x) ) {
    
    if ( ! all( cxaudit::cxaudit_validreference( base::unlist(base::strsplit(xattr, ":", fixed = TRUE))) ) )
      stop( "Attribute ", xattr, " is not a valid attribute reference" )
   
    .self$.attr[["attributes"]][[ xattr ]] <- base::unname(as.character(x[xattr]))
  }


  
  return(invisible( .self$getattributes() ))
})



cxaudit_record$methods( "getattributes" = function() {
  "Return list of record attributes"

  return(invisible( .self$.attr[["attributes"]] ))
})



cxaudit_record$methods( ".setlinks" = function(x) {
  "Internal method to link audit records"
  
  if ( missing(x) || is.null(x) || any(is.na(x)) || (length(x) == 0) || ! inherits(x, "list") )
    stop( "List of linked records missing or in an invalid format" )
  
  
  # -- assert that all are audit records
  for ( xidx in 1:length(x) ) 
    if ( ! inherits( x[[xidx]], "cxaudit_record" ) || 
         is.null( attr( class(x[[xidx]]), "package" ) ) || (attr( class(x[[xidx]]), "package" ) != "cxaudit") )
       stop( "Expecting a list entry of type cxaudit::cxaudit_record" )
    

  
  # -- store record using ID as de-duplicating information
  for ( xrec in x ) {

    # - get record ID
    x_id <- try( xrec$getproperties()[["id"]] )
      
    if ( inherits( x_id, "try-error") || is.null(x_id) )
      stop( "Records does not have an id property")

    .self$.attr[["links"]][[ x_id ]] <- xrec
    
  }

        
  return(invisible( .self$getlinks() ))
})



cxaudit_record$methods( "getlinks" = function() {
  "Return list of link audit records"
  
  return(invisible( base::unname( .self$.attr[["links"]] ) ))
})





cxaudit_record$methods( "show" = function() {
  "Display record representation"
  
  props <- .self$getproperties()
  
  rec_info <- character(0)


  # - add properties
  
  rec_info <- append( rec_info, c( " ", "Properties", "----------------------" ) )
  
  for ( xitem in base::names(props) ) 
    rec_info <- append( rec_info, 
                        paste( xitem, 
                               props[[xitem]], sep = ": " ) )
  

  
  
  
  cat( c( "Audit record",
          paste0( "(", attr( class(.self), "package" ), "::", class(.self), ")", " "), 
          rec_info ), 
       sep = "\n" )
  
})