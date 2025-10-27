#' Internal vector representing valid audit events
#' 
#' @return Character vector of event names
#' 
#' @description
#' The event is represented as a case insensitive keyword, most often 
#' lower case.
#' 
#' Valid events are `create`, `read`, `write`, `update`, `delete`, `execute`, 
#' `fail`, `commit`, `lock`, `unlock`, `sign`, `connect`, `disconnect` and 
#' `import`.
#' 
#' 
#' @keywords internal

.cxaudit_eventnames <- function() {
  
  return( c( "create", "read", "write", "update", "delete", "execute", "fail", 
             "commit", "lock", "unlock", "sign", "connect", "disconnect", "import" ) )
}