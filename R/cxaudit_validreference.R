#' Utility function to assert if the string is a valid reference
#' 
#' @param x A vector of references
#' 
#' @return Logical vector
#' 
#' @description
#' 
#' Returns a logical vector of the same length as `x` with `TRUE` if the item 
#' is a valid reference and `FALSE` otherwise. 
#' 
#' A reference is a case insensitive string consisting of 
#' \itemize{
#'   \item letters A-Z, digits 0-9 and punctuation dash `-`, underscore `_`
#'         and period `.`
#'   \item starts and ends with a letter or digit 
#'   \item is between 2 and 100 characters in length
#' }
#' 
#' 
#' 
#' @export


cxaudit_validreference <- function( x ) {
  
  # -- futility
  if ( missing(x) || is.null(x) || ! inherits( x, c( "character", "numeric") ) ) 
    return(FALSE)


  return( grepl( "^[a-z0-9][a-z0-9\\._\\-]{0,98}[a-z0-9]$", base::trimws(x), ignore.case = TRUE, perl = TRUE) )
}