#
#
#  Test for cxaudit::cxaudit_validreference()
#
#
#


#' @cx.testsfor cxaudit::cxaudit_validreference


testthat::test_that( "audit.validreference.paramMissing", {

  #' @cx.tests Missing reference returns FALSE  

  
  # -- test
  testthat::expect_false( cxaudit::cxaudit_validreference() )
  
})



testthat::test_that( "audit.validreference.paramNull", {
  
  #' @cx.tests Reference equal to NULL returns FALSE
  

  # -- test
  testthat::expect_false( cxaudit::cxaudit_validreference( NULL ) )
  
})


testthat::test_that( "audit.validreference.paramInvalidType", {

  #' @cx.tests Invalid type reference returns FALSE 

      
  # -- test
  testthat::expect_false( cxaudit::cxaudit_validreference( TRUE ) )
  
})



testthat::test_that( "audit.validreference.paramEmptyString", {
  
  #' @cx.tests Reference equal to an empty string returns FALSE
  #' @cx.tests Reference equal to string of spaces returns FALSE
  
  # -- test
  testthat::expect_false( cxaudit::cxaudit_validreference( "   " ) )
  
})



testthat::test_that( "audit.validreference.paramInvalidReferences", {
  
  #' reference when input parameter is a vector of invalid references 
  
  
  # -- stage
  
  #' @cx.tests A reference of valid characters and length 1 is invalid and returns FALSE 
  #' @cx.tests A reference starting with an underscore is invalid and returns FALSE 
  #' @cx.tests A reference starting with an period is invalid and returns FALSE 
  #' @cx.tests A reference starting with an dash is invalid and returns FALSE 
  #' @cx.tests A reference ending with an underscore is invalid and returns FALSE 
  #' @cx.tests A reference ending with an period is invalid and returns FALSE 
  #' @cx.tests A reference ending with an dash is invalid and returns FALSE 
  #' @cx.tests A reference of valid characters and length larger than 100 characters is invalid and returns FALSE

  test_references <- c( "a", "1", 
                        paste0( c("_", ".", "-"), "ab" ),
                        paste0( c("_", ".", "-"), "12" ), 
                        paste0( "ab", c("_", ".", "-") ),
                        paste0( "12", c("_", ".", "-") ),
                        paste0( "a", paste( sample( c( base::letters, as.character(0:9) ), 101, replace = TRUE), collapse = "" ), "b" )
                        )

  

  

  # -- test
  
  result <- cxaudit::cxaudit_validreference( test_references ) 
  
  
  # -- expected 
  
  expected_results <- base::rep_len( FALSE, length(test_references))
  
  
  
  # -- assertions
  
  testthat::expect_equal( result, expected_results )
  
})




testthat::test_that( "audit.validreference.paramValidReferences", {
  
  #' valid reference when input parameter is a vector of valid references 
  
  
  # -- stage
  
  #' @cx.tests A reference of length 2 containing letters a-z is valid and returns TRUE 
  #' @cx.tests A reference of length 2 containing digits 0-9 is valid and returns TRUE 
  #' @cx.tests A reference starting and ending in a character a-z with an embedded underscore is valid and returns TRUE 
  #' @cx.tests A reference starting and ending in a character a-z with an embedded period is valid and returns TRUE 
  #' @cx.tests A reference starting and ending in a character a-z with an embedded dash is valid and returns TRUE 
  #' @cx.tests A reference starting and ending in a digit 0-9 with an embedded underscore is valid and returns TRUE 
  #' @cx.tests A reference starting and ending in a digit 0-9 with an embedded period is valid and returns TRUE 
  #' @cx.tests A reference starting and ending in a digit 0-9 with an embedded dash is valid and returns TRUE 
  #' @cx.tests A reference ending with an underscore is invalid and returns FALSE 
  #' @cx.tests A reference ending with an period is invalid and returns FALSE 
  #' @cx.tests A reference ending with an dash is invalid and returns FALSE 
  
  
  test_references <- c( "ab", "12", 
                        paste0( "a", c("_", ".", "-"), "b" ), 
                        paste0( "1", c("_", ".", "-"), "2" ), 
                        paste0( "a", paste( sample( c( base::letters, as.character(0:9), ".", "_", "-"), 40, replace = TRUE), collapse = "" ), "b" ), 
                        paste0( "a", paste( sample( c( base::letters, as.character(0:9), ".", "_", "-"), 98, replace = TRUE), collapse = "" ), "b" ) )
  

  # -- test
  
  result <- cxaudit::cxaudit_validreference( test_references ) 
  
  
  # -- expected
  
  expected_results <- base::rep_len( TRUE, length(test_references) )
  
  
  # -- assertions
  
  testthat::expect_equal( result, expected_results)
  
})
