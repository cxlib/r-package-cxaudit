#
#
#
#
#

testthat::test_that( "cxaudit.internal.eventnames", {

  # -- test
  
  result <- cxaudit:::.cxaudit_eventnames()
  
  
  # -- expected  
  
  expect_events <- c( "create", "read", "write", "update", "delete", "execute", "fail", 
                      "commit", "lock", "unlock", "sign", "connect", "disconnect", "import" )
  
  
  # -- assertions
  
  testthat::expect_equal( base::tolower(base::sort( result )), base::tolower(base::sort( expect_events )) )

})

