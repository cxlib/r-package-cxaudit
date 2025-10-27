#
#
#  Tests for internal function cxaudit:::.cxaudit_propertynames()
#
#
#
#

testthat::test_that( "cxaudit.internal.propertynames", {
  
  #' expected property names
  
  
  # -- test
  
  result <- cxaudit:::.cxaudit_propertynames()
  
  
  # -- expected
  
  expected_names <- c( "id", "event", "object.type", "object.class", "object.hash", "object.path", "label",  "actor", "env", "datetime" )
  
  
  # -- assertions
  
  testthat::expect_equal( base::sort(base::tolower(base::trimws(result))), base::sort(base::tolower(base::trimws(expected_names))) )

})
