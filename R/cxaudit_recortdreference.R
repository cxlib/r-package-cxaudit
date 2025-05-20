#' Reference class representing an audit record reference
#' 
#' @field .attr Internal storage of record properties and attributes
#' 
#' @method getproperties getproperties
#' @method getproperty getproperty
#' @method show show
#' 
#' @description
#' Represents an abbreviated reference to an audit record. The class is intended
#' for references and not for creating and for registering an audit record.
#' 
#' The `getproperties` method returns a list of the record reference named 
#' properties.
#' 
#' The `getproperty` method returns the value of specified property `x`. If `x` 
#' is not defined as a property, `NULL` is returned.
#' 
#' @exportClass cxaudit_recordreference
#' @export cxaudit_recordreference


cxaudit_recordreference <- methods::setRefClass( "cxaudit_recordreference",
                                                 fields = list( ".attr" = "list" ) )



cxaudit_recordreference$methods( "initialize" = function() {
  "Initialize record representation"
  
  # note: Initialization does not takes parameters but the information represented
  #       by the reference class is back-loaded into .attr
  
  .self$.attr <- list()
})




cxaudit_recordreference$methods( "getproperties" = function() {
  "Return list of record properties"
  
  return(invisible( .self$.attr ))
})
  



cxaudit_recordreference$methods( "getproperty" = function(x) {
  "Return a named record property"
  
  if ( missing(x) || is.null(x) || any(is.na(x)) || ! inherits(x, "character") || 
       (length(x) != 1) || (base::trimws(x) == "") || 
       ! base::tolower(base::trimws(x)) %in% base::names(.self$.attr) )
    return(invisible(NULL))
  
  return(invisible( base::unname(.self$.attr[[ base::tolower(base::trimws(x)) ]]) ))
})




cxaudit_recordreference$methods( "show" = function() {
  "Display record representation"
  
})