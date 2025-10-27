# note: not loading library cxaudit and testthat as package should be able to be
#       used without library() or require()
# library(cxaudit)
# library(testthat)

testthat::test_check("cxaudit")