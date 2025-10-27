#' Internal vector represents valid property names
#' 
#' @return Character vector of property names
#' 
#' @keywords internal

.cxaudit_propertynames <- function() {
  return( c( "id", "event", "object.type", "object.class", "object.hash", "object.path", "label",  "actor", "env", "datetime" ) )
}