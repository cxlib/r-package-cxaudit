#' Assert if a specified even is valid
#' 
#' @param x Event string
#' 
#' @return Logical `TRUE` if event is valid. `FALSE` otherwise
#' 
#' @description
#' An event is a case insensitive keyword representing the event. 
#' 
#' See \link{.cxaudit_eventnames} for a list of valid events.
#' 
#' 
#' 
#' @export

cxaudit_validevent <- function( x ) {

  # -- obviously 
  
  if ( missing(x) || is.null(x) || any(is.na(x)) || ! inherits( x, "character") ||
       ( length(x) != 1 ) || (base::trimws(x) == "") )
    return(invisible(FALSE))
  
  
  # -- supported events
  #    note: the use of all() is overkill as only a single event can be valid
  return(invisible( all(base::tolower(base::trimws(x)) %in% cxaudit:::.cxaudit_eventnames() ) )) 
}

