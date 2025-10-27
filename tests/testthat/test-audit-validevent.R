#
#
#  Testing valid events
#
#  
#

#' @cx.testsfor cxaudit::cxaudit_validevent


testthat::test_that( "audit.validevents.paramMissing", {
  
  #' @cx.tests Missing event returns FALSE
  
  # -- test
  testthat::expect_false( cxaudit::cxaudit_validevent() )

})



testthat::test_that( "audit.validevents.paramNull", {
  
  #' @cx.tests Event equal to NULL returns FALSE
  
  
  # -- test
  testthat::expect_false( cxaudit::cxaudit_validevent(NULL) )
  
})



testthat::test_that( "audit.validevents.paramNA", {
  
  #' @cx.tests Event equal to NA returns FALSE
  
  
  # -- test
  testthat::expect_false( cxaudit::cxaudit_validevent(NA) )
  
})



testthat::test_that( "audit.validevents.emptyString", {
  
  #' @cx.tests Event equal to an empty string returns FALSE
  #' @cx.tests Event equal to string of spaces returns FALSE
  
  
  # -- test
  testthat::expect_false( cxaudit::cxaudit_validevent("         ") )
  
})







testthat::test_that( "audit.validevents.invalidString", {

  #' @cx.tests Invalid event reference returns FALSE
  

  # -- stage
  
  #    note: event not plausible as it is too long and includes digits
  test_event <- paste( sample( c(base::LETTERS, base::letters, as.character(1:9) ), 40,  replace = TRUE), collapse = "")
  
  
  if ( test_event %in% cxaudit:::.cxaudit_eventnames() )
    testthat::fail( "Randomness gave us a valid event" )
  
  
  # -- test
  testthat::expect_false( cxaudit::cxaudit_validevent( test_event  ) )
  
})




testthat::test_that( "audit.validevents.expectedValidEvents", {

  #' @cx.tests Valid event reference returns TRUE
    

  # -- stage
  
  test_events <- cxaudit:::.cxaudit_eventnames()

  
  # -- test
  
  for ( xitem in test_events )
    testthat::expect_true( cxaudit::cxaudit_validevent( xitem ), label = paste( "Returned value for event", base::dQuote( xitem, q = FALSE)) )
  
})




testthat::test_that( "audit.validevents.expectedMultiEvents", {

  #' @cx.tests Only a single event can be specified
  #' @cx.tests Multiple valid event references return FALSE

  
  # -- stage
  
  test_events <- sample( cxaudit:::.cxaudit_eventnames(), 2 )
  
  if ( length(base::unique(test_events)) == 1 )
    testthat::fail( "Should be at least two non-equal events")
  
  
  # -- test and assertion

  testthat::expect_false( cxaudit_validevent( test_events ) )

})







