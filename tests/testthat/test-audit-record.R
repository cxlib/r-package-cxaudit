#
#
#  Tests for internal function cxaudit::cxaudit_record()
#
#
#
#


#' @cx.testsfor cxaudit::cxaudit_record


testthat::test_that( "cxaudit.record.new", {

  #' @cx.tests Initialize as an empty record
  #' @cx.tests Default record date/time truncated to seconds

  
  # -- stage 
  prior_dt <- base::trunc.POSIXt( as.POSIXct( Sys.time(), tz = "UTC" ), units = "secs" )

  
  # -- test
  result <- cxaudit::cxaudit_record()


  
  # -- expected
  
  # - date/time after test 
  post_dt <- base::trunc.POSIXt( as.POSIXct( Sys.time(), tz = "UTC" ), units = "secs" )
  
  # - expected internal attributes
  expected_attr <- c( cxaudit:::.cxaudit_propertynames(), "attributes", "links" )

  
    
  
  # -- assertions
  
  testthat::expect_true( inherits( result$.attr, "list" ) )
  testthat::expect_equal( base::sort(base::names(result$.attr)), base::sort(expected_attr) )

  #' @cx.test Record is initialized with a default record ID
  testthat::expect_true( uuid::UUIDvalidate(result$.attr[["id"]]) )


  #' @cx.test Record is initialized with the current date time truncated to the second
  testthat::expect_true( inherits( result$.attr[["datetime"]], c( "POSIXct", "POSIXt" ) ) )
  testthat::expect_true( prior_dt <= result$.attr[["datetime"]])
  testthat::expect_true( result$.attr[["datetime"]] <= post_dt ) 
  
    
  #' @cx.test Default properties except id and datetime are initialized as NA
  testthat::expect_equal( base::unique(base::unlist( result$.attr[ ! base::names(result$.attr) %in% c( "id", "datetime", "attributes", "links") ], use.names = FALSE)), NA)

  #' @cx.test List of attributes is empty when initializing an empty record
  testthat::expect_length( result$.attr[["attributes"]], 0)
  
  #' @cx.test List of linked records is empty when initializing an empty record
  testthat::expect_length( result$.attr[["links"]], 0)

})



testthat::test_that( "cxaudit.record.initAllPropsValid", {
 
  #' simple test for complete valid record

  #' @cx.tests Initialize complete record
    
  
  # -- stage
  
  test_rec <- list()
  
  # - add ID
  #' @cx.tests Assign new record ID during record initialization
  test_rec[["id"]] <- uuid::UUIDgenerate()
  
  # - add event
  #' @cx.tests Assign valid event during record initialization
  test_rec[["event"]] <- sample( cxaudit:::.cxaudit_eventnames(), 1 )
  
  # - add type and class
  #' @cx.tests Assign valid object type during record initialization
  #' @cx.tests Assign valid object class during record initialization
  test_rec[ c( "object.type", "object.class" ) ] <- replicate( 2, 
                                                               paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE ), collapse = "" ), 
                                                               simplify = TRUE )

  # - add path
  #' @cx.tests Assign valid object path during record initialization
  test_rec[["object.path"]] <- paste0( "/", paste( replicate( 10, 
                                                              paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE ), collapse = "" ), 
                                                              simplify = TRUE ), collapse = "/" ) )
                                
  
  # - add hash
  #' @cx.tests Assign valid object hash during record initialization
  test_rec[["object.hash"]] <- digest::digest( paste( sample( c( base::letters, as.character(0:9) ), 100, replace = TRUE ), collapse = "" ), algo = "sha1", file = FALSE )
  
  
  # - add label
  #' @cx.tests Assign valid record label during record initialization
  test_rec[["label"]] <- paste( replicate( sample( c(5:15), 1), 
                                           paste( sample( c( base::letters, as.character(0:9) ), 
                                                          sample( c(5:25), 1),
                                                          replace = TRUE ), collapse = "" ), 
                                           simplify = TRUE ), collapse = " " )
  
  #' @cx.tests Assign valid actor during record initialization
  #' @cx.tests Assign valid environment reference type during record initialization
  test_rec[ c( "actor", "env" ) ] <- replicate( 2, 
                                                 paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE ), collapse = "" ), 
                                                 simplify = TRUE )
  
  #' @cx.test Record is initialized with the current date time truncated to the second
  test_rec[["datetime"]] <- base::trunc.POSIXt( as.POSIXct( Sys.time(), tz = "UTC" ), units = "secs" )
  
  
  
  # -- test
  result <- cxaudit::cxaudit_record( test_rec )
  
  
  # -- assertions

  # - property names
  result_props <- result$.attr[ ! base::names(result$.attr) %in% c( "attributes", "links") ]  
  testthat::expect_equal( base::sort(base::names(result_props)), base::sort(base::names(test_rec)) )
  
  # - property values
  testthat::expect_equal( result$.attr[ base::sort(base::names(test_rec)) ], test_rec[ base::sort(base::names(test_rec)) ] )

})





testthat::test_that( "cxaudit.record.initAutomaticProps", {
  

  #' @cx.tests Initialize complete record excluding ID and date/time
  
  
  # -- stage

  # note: not including ID and datetime 
  
  test_rec <- list()
  
  # - add event
  #' @cx.tests Assign valid event during record initialization
  test_rec[["event"]] <- sample( cxaudit:::.cxaudit_eventnames(), 1 )
  
  # - add type and class  
  #' @cx.tests Assign valid object type during record initialization
  #' @cx.tests Assign valid object class during record initialization
  test_rec[ c( "object.type", "object.class" ) ] <- replicate( 2, 
                                                               paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE ), collapse = "" ), 
                                                               simplify = TRUE )
  
  # - add path
  #' @cx.tests Assign valid object path during record initialization
  test_rec[["object.path"]] <- paste0( "/", paste( replicate( 10, 
                                                              paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE ), collapse = "" ), 
                                                              simplify = TRUE ), collapse = "/" ) )
  
  
  # - add hash
  #' @cx.tests Assign valid object hash during record initialization
  test_rec[["object.hash"]] <- digest::digest( paste( sample( c( base::letters, as.character(0:9) ), 100, replace = TRUE ), collapse = "" ), algo = "sha1", file = FALSE )
  
  
  # - add label
  #' @cx.tests Assign valid record label during record initialization
  test_rec[["label"]] <- paste( replicate( sample( c(5:15), 1), 
                                           paste( sample( c( base::letters, as.character(0:9) ), 
                                                          sample( c(5:25), 1),
                                                          replace = TRUE ), collapse = "" ), 
                                           simplify = TRUE ), collapse = " " )
  
  #' @cx.tests Assign valid actor during record initialization
  #' @cx.tests Assign valid environment reference type during record initialization
  test_rec[ c( "actor", "env" ) ] <- replicate( 2, 
                                                paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE ), collapse = "" ), 
                                                simplify = TRUE )
  
  
  if ( any( c( "id", "datetime") %in% base::names(test_rec)) )
    testthat::fail( "Could not stage test record" )

  
  # - generate reference date/time
  test_datetime <- base::trunc.POSIXt( as.POSIXct( Sys.time(), tz = "UTC" ), units = "secs" )
  
  
  
  # -- test
  result <- cxaudit::cxaudit_record( test_rec )
  

  # -- expected
  
  expected_props <- c( "id", "datetime", base::names(test_rec) )
  
    
  # -- assertions

  # - property names
  result_props <- result$.attr[ ! base::names(result$.attr) %in% c( "attributes", "links") ]  
  testthat::expect_equal( base::sort(base::names(result_props)), base::sort(expected_props) )
  
  
  # - property values
  testthat::expect_equal( result$.attr[ base::sort(base::names(test_rec)) ], test_rec[ base::sort(base::names(test_rec)) ] )
  
  
  # - auto-generated property id
  #' @cx.test Record is initialized with a valid ID
  testthat::expect_true( uuid::UUIDvalidate(result$.attr[["id"]]) )
  
  
  # - auto-generated property datetime
  #' @cx.test Record is initialized with the current date time truncated to seconds
  testthat::expect_true( test_datetime <= result$.attr[["datetime"]] )
  testthat::expect_true( result$.attr[["datetime"]] <= base::trunc.POSIXt( as.POSIXct( Sys.time(), tz = "UTC" ), units = "secs" ) )

})




testthat::test_that( "cxaudit.record.initPropNull", {
  
  
  # -- test
  #' @cx.test Initializing a record with list of properties equal to NULL results in an error
  testthat::expect_error( cxaudit::cxaudit_record( NULL ), regexp =  "^Required initial record properties missing or invalid$" )

})



testthat::test_that( "cxaudit.record.initPropNA", {
  
  # -- test
  #' @cx.test Initializing a record with list of properties equal to NA results in an error
  testthat::expect_error( cxaudit::cxaudit_record( NA ), regexp =  "^Required initial record properties missing or invalid$" )

})


testthat::test_that( "cxaudit.record.initPropEmptyList", {

  # -- test
  #' @cx.test Initializing a record with an empty list of properties results in an error
  testthat::expect_error( cxaudit::cxaudit_record( list() ), regexp =  "^Required initial record properties missing or invalid$" )

})


testthat::test_that( "cxaudit.record.initPropEmptyVector", {
  
  # -- test
  #' @cx.test Initializing a record with an empty character vector of properties results in an error
  testthat::expect_error( cxaudit::cxaudit_record( character(0) ), regexp =  "^Required initial record properties missing or invalid$" )
  
})


testthat::test_that( "cxaudit.record.initPropEmptyInvalidType", {
  
  # -- test
  #' @cx.test Initializing a record with vector not of type character or a list results in an error
  testthat::expect_error( cxaudit::cxaudit_record( as.numeric(c(1))  ), regexp =  "^Required initial record properties missing or invalid$" )
  
})



testthat::test_that( "cxaudit.record.initPropIDNA", {

  # -- test
  #' @cx.test Initializing a record with property ID equal to NA results in an error
  testthat::expect_error( cxaudit::cxaudit_record( list( "id" = NA) ), regexp =  "^Required initial record properties missing or invalid$" )
  
})





testthat::test_that( "cxaudit.record.initPropIDInvalidValue", {
  

  # -- stage 
  #' @cx.test Initializing a record with property ID equal to NULL results in an error
  #' @cx.test Initializing a record with property ID equal to an empty string results in an error
  #' @cx.test Initializing a record with property ID equal to a character string not in an UUID format results in an error

  test_ids <- list( "NULL" = NULL, 
                    "Empty string" = "", 
                    "String not in a UUID format" = paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") )
  
  
  if ( uuid::UUIDvalidate( test_ids[["String not in a UUID format"]] ) )
    testthat::fail( "Random string is unepxected in an UUID format" )
    

  # -- test
  
  for ( xscenario in base::names(test_ids) )
    testthat::expect_error( cxaudit::cxaudit_record( list( "id" = test_ids[[xscenario]] )), 
                            regexp =  "^Property id not in a valid format$", 
                            label = paste( "The id property failed for", xscenario ) )
  
})






testthat::test_that( "cxaudit.record.initPropDateTimeNA", {
  

  # -- test
  #' @cx.test Initializing a record with property datetime equal to NA results in an error
  testthat::expect_error( cxaudit::cxaudit_record( list( "datetime" = NA) ), regexp =  "^Required initial record properties missing or invalid$" )
  
})





testthat::test_that( "cxaudit.record.initPropDateTimeInvalidValue", {
  

  # -- stage 
  #' @cx.test Initializing a record with property datetime equal to NULL results in an error
  #' @cx.test Initializing a record with property datetime equal to an empty string results in an error
  #' @cx.test Initializing a record with property datetime equal to a character string not in an UUID format results in an error  
   
  test_datetimes <- list( "NULL" = NULL, 
                          "Empty string" = "", 
                          "String not in a date/time format" = paste( sample( c( base::LETTERS, base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") )
  

  # -- test
  
  for ( xscenario in base::names(test_datetimes) )
    testthat::expect_error( cxaudit::cxaudit_record( list( "datetime" = test_datetimes[[xscenario]] )), 
                            regexp =  "^Property datetime not in a valid format$", 
                            label = paste( "The datetime property failed for", xscenario ) )
  
})



testthat::test_that( "cxaudit.record.setPropNull", {
  

  # -- stage

  # - test audit record
  test_obj <- cxaudit::cxaudit_record()

  
  # -- test
  #' @cx.test Set record properties with list of properties equal to NULL results in an error
  testthat::expect_error( test_obj$setproperties( NULL ), regexp = "^Properties are missing or in an invalid format$" )
  
  
})






testthat::test_that( "cxaudit.record.setPropNotPermitted", {
  

  # -- stage
  
  # - blocked properties
  #' @cx.test Set record properties with valid ID property results in an error
  #' @cx.test Set record properties with valid datetime property results in an error
  test_recs <- list( "id" = uuid::UUIDgenerate(), 
                     "datetime" = as.POSIXct( Sys.time(), tz = "UTC" ) )
  
  
  # - test audit record
  test_obj <- cxaudit::cxaudit_record()
  
  
  # -- test
  for ( xprop in base::names(test_recs) )
    testthat::expect_error( test_obj$setproperties( test_recs[ xprop ] ), 
                            regexp = "^Record properties id and datetime cannot be set after initialization$",
                            label = paste( "Property", xprop, "failed" ) )
  
})  






testthat::test_that( "cxaudit.record.setPropInvalidInput", {
  

  # -- stage
  
  # - test records
  #' @cx.test Set record properties with property list equal to NULL results in an error
  #' @cx.test Set record properties with property list equal to NA results in an error
  #' @cx.test Set record properties with an empty property list results in an error
  #' @cx.test Set record properties with a property vector of valid unnamed elements results in an error
  #' @cx.test Set record properties with a property list of valid unnamed elements results in an error
  
  test_recs <- list( "NULL" = NULL, 
                     "NA" = NA, 
                     "Empty list" = list(), 
                     "Unnamed vector" = c( utils::head( cxaudit:::.cxaudit_eventnames(), n = 1), paste( sample( base::letters, 15, replace = TRUE), collapse = "") ), 
                     "Element missing name" = c( utils::head( cxaudit:::.cxaudit_eventnames(), n = 1), "event" =  utils::head( cxaudit:::.cxaudit_eventnames(), n = 1) ) )
  
  
  
  # - test audit record
  test_obj <- cxaudit::cxaudit_record()
  
  
  # -- test
  for ( xscenario in base::names(test_recs) )
    testthat::expect_error( test_obj$setproperties( test_recs[[ xscenario ]] ), 
                            regexp = "^Properties are missing or in an invalid format$",
                            label = xscenario )
  
  
})




testthat::test_that( "cxaudit.record.setPropInvalidPropName", {
  

  # -- stage
  
  # - test record
  test_rec <- list( "event" = utils::head( cxaudit:::.cxaudit_eventnames(), n = 1) )
  
  #' @cx.test Set record properties with a named property that has an invalid property name results in an error
  test_rec[[ paste( sample( base::letters, 15, replace = TRUE), collapse = "") ]] <- paste( sample( base::letters, 40, replace = TRUE), collapse = "")

  
  # - test audit record object
  test_obj <- cxaudit::cxaudit_record()
  
  
  # -- test
  testthat::expect_error( test_obj$setproperties( test_rec ), 
                          regexp = "^One or more unknown/unsupported properties submitted$" )

})




testthat::test_that( "cxaudit.record.setPropPropNameAttrLink", {
  
  # -- stage
  
  # - test audit record object
  test_obj <- cxaudit::cxaudit_record()
  
  
  # -- test
  
  #' @cx.test The property name attributes is reserved when using set properties and results in an error
  #' @cx.test The property name links is reserved when using set properties and results in an error

  for ( xprop in c( "attributes", "links") ) {
    
    test_rec <- list( paste( sample( base::letters, 15, replace = TRUE), collapse = "") )
    base::names(test_rec) <- xprop
    
    testthat::expect_error( test_obj$setproperties( test_rec ), 
                            regexp = "^Record attributes and links are not record properties$" )
    
    base::rm( list = "test_rec" )
  }

})





testthat::test_that( "cxaudit.record.setPropInvalidValue", {
  

  # -- stage
  
  # - test value
  
  #' @cx.tests Set properties with event property equal to NULL results in an error
  #' @cx.tests Set properties with event property equal to an empty string results in an error
  
  test_values <- list( NULL, "" )
  base::names(test_values) <- rep_len( utils::head( cxaudit:::.cxaudit_propertynames()[ ! cxaudit:::.cxaudit_propertynames() %in%  c( "id", "datetime") ], n = 1), length(test_values) )


  # - test audit record object
  test_obj <- cxaudit::cxaudit_record()
  
  
  # -- test
  for ( xidx in 1:length(test_values) )
    testthat::expect_error( test_obj$setproperties( test_values[ xidx ] ), 
                            regexp = "^Value of property event missing or an empty string$" )
  
})





testthat::test_that( "cxaudit.record.setPropInvalidEvent", {
  

  # -- stage
  
  # - test event
  #' @cx.tests Set properties with an invalid event property reference value results in an error
  test_rec <- list( "event" = paste( sample( base::letters, 40, replace = TRUE), collapse = "") )
  
  if ( cxaudit::cxaudit_validevent( test_rec[["event"]] ) )
    testthat::fail( "Unexpected random string matches a valid event name" )
  
  
  # - test audit record object
  test_obj <- cxaudit::cxaudit_record()
  
  
  # -- test
  testthat::expect_error( test_obj$setproperties( test_rec ), 
                          regexp = "^Event not supported$" )
  
})




testthat::test_that( "cxaudit.record.setPropInvalidObjectRefProp", {

  #' @cx.tests Set properties with an invalid reference value for object type results in an error
  #' @cx.tests Set properties with an invalid reference value for object class results in an error

  # -- stage
  
  # - test reference value
  #   note: a reference value cannot start or end with a period
  #   note: align with cxaudit::cxaudit_validreference

  test_rec <- list( "_reference_" = paste( c( ".", sample( base::letters, 40, replace = TRUE), "."), collapse = "") )
  
  if ( cxaudit::cxaudit_validreference( test_rec[["_reference_"]] ) )
    testthat::fail( "Unexpected random string matches a valid reference" )
  
  
  # - test audit record object
  test_obj <- cxaudit::cxaudit_record()
  
  
  # -- test
  for ( xprop in c( "object.type", "object.class" ) ) {

    base::names(test_rec) <- xprop
    
    testthat::expect_error( test_obj$setproperties( test_rec ), 
                            regexp = paste("^The property", xprop, "is not a valid reference$") )
    
    base::names(test_rec) <- "_reference_"
        
  }
    
})




testthat::test_that( "cxaudit.record.setPropInvalidObjectPath", {

  #' @cx.tests Set properties with an invalid value for object path results in an error
  
    
  # -- stage
  
  # - test path value
  #   note: a path is a delimited set of reference values that cannot start or end with a period
  #   note: align with cxaudit::cxaudit_validreference
  
  test_refs <- c( replicate( 10, 
                             paste( sample( base::letters, 40, replace = TRUE), collapse = ""), 
                             simplify = TRUE ), 
                  paste( c( ".", sample( base::letters, 40, replace = TRUE), "."), collapse = "") )


  if ( all(cxaudit::cxaudit_validreference( test_refs )) )
    testthat::fail( "Unexpected random string matches a valid reference" )

    
  test_rec <- list( "object.path" = paste0( "/", paste( test_refs, collapse = "/" ) ) ) 
  

  # - test audit record object
  test_obj <- cxaudit::cxaudit_record()
  
  
  # -- test
  testthat::expect_error( test_obj$setproperties( test_rec ), 
                          regexp = "^The object path is invalid$" )
  
})





testthat::test_that( "cxaudit.record.setPropInvalidObjectPath", {
  
  #' @cx.tests Set properties with an object path where one level is defined by an invalid reference value results in an error
  
  
  # -- stage
  
  # - test path value
  #   note: a path is a delimited set of reference values that cannot start or end with a period
  #   note: align with cxaudit::cxaudit_validreference
  
  test_refs <- c( replicate( 10, 
                             paste( sample( base::letters, 40, replace = TRUE), collapse = ""), 
                             simplify = TRUE ), 
                  paste( c( ".", sample( base::letters, 40, replace = TRUE), "."), collapse = "") )
  
  
  if ( all(cxaudit::cxaudit_validreference( test_refs )) )
    testthat::fail( "Unexpected random string matches a valid reference" )
  
  
  test_rec <- list( "object.path" = paste0( "/", paste( test_refs, collapse = "/" ) ) ) 
  
  
  # - test audit record object
  test_obj <- cxaudit::cxaudit_record()
  
  
  # -- test
  testthat::expect_error( test_obj$setproperties( test_rec ), 
                          regexp = "^The object path is invalid$" )
  
})



testthat::test_that( "cxaudit.record.setPropAllValid", {
  
  #' @cx.tests set complete set of record properties

  
  # -- stage
  
  test_rec <- list()
  

  # - add event
  #' @cx.tests Assign valid event property
  test_rec[["event"]] <- sample( cxaudit:::.cxaudit_eventnames(), 1 )
  
  # - add type and class
  #' @cx.tests Assign valid object type property
  #' @cx.tests Assign valid object class property
  test_rec[ c( "object.type", "object.class" ) ] <- replicate( 2, 
                                                               paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE ), collapse = "" ), 
                                                               simplify = TRUE )
  
  # - add path
  #' @cx.tests Assign valid object path property
  test_rec[["object.path"]] <- paste0( "/", paste( replicate( 10, 
                                                              paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE ), collapse = "" ), 
                                                              simplify = TRUE ), collapse = "/" ) )
  
  
  # - add hash
  #' @cx.tests Assign valid object hash property
  test_rec[["object.hash"]] <- digest::digest( paste( sample( c( base::letters, as.character(0:9) ), 100, replace = TRUE ), collapse = "" ), algo = "sha1", file = FALSE )
  
  
  # - add label
  #' @cx.tests Assign valid record label property
  test_rec[["label"]] <- paste( replicate( sample( c(5:15), 1), 
                                           paste( sample( c( base::letters, as.character(0:9) ), 
                                                          sample( c(5:25), 1),
                                                          replace = TRUE ), collapse = "" ), 
                                           simplify = TRUE ), collapse = " " )
  
  #' @cx.tests Assign valid actor property
  #' @cx.tests Assign valid environment property
  test_rec[ c( "actor", "env" ) ] <- replicate( 2, 
                                                paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE ), collapse = "" ), 
                                                simplify = TRUE )
  


  # - test audit record object
  test_obj <- cxaudit::cxaudit_record()
  
  
  
  # -- test
  test_obj$setproperties( test_rec )



  # -- assertions

  # - has properties
  testthat::expect_true( all( base::sort(base::names(test_rec)) %in% base::sort(base::names( test_obj$.attr[ ! is.na(test_obj$.attr) ] )) ) )
  
  # - has property values
  testthat::expect_equal( test_obj$.attr[ base::sort(base::names(test_rec)) ], test_rec[ base::sort(base::names(test_rec)) ] )

})






testthat::test_that( "cxaudit.record.getPropAllSet", {
  

  #' @cx.tests Get properties when all properties are defined  
  
  
  # -- stage
  
  test_rec <- list()
  
  
  # - add event
  #' @cx.tests Assign valid event property
  test_rec[["event"]] <- sample( cxaudit:::.cxaudit_eventnames(), 1 )
  
  # - add type and class
  #' @cx.tests Assign valid object type property
  #' @cx.tests Assign valid object class property
  test_rec[ c( "object.type", "object.class" ) ] <- replicate( 2, 
                                                               paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE ), collapse = "" ), 
                                                               simplify = TRUE )
  
  # - add path
  #' @cx.tests Assign valid object path property
  test_rec[["object.path"]] <- paste0( "/", paste( replicate( 10, 
                                                              paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE ), collapse = "" ), 
                                                              simplify = TRUE ), collapse = "/" ) )
  
  
  # - add hash
  #' @cx.tests Assign valid object hash property
  test_rec[["object.hash"]] <- digest::digest( paste( sample( c( base::letters, as.character(0:9) ), 100, replace = TRUE ), collapse = "" ), algo = "sha1", file = FALSE )
  
  
  # - add label
  #' @cx.tests Assign valid record label property
  test_rec[["label"]] <- paste( replicate( sample( c(5:15), 1), 
                                           paste( sample( c( base::letters, as.character(0:9) ), 
                                                          sample( c(5:25), 1),
                                                          replace = TRUE ), collapse = "" ), 
                                           simplify = TRUE ), collapse = " " )
  
  
  #' @cx.tests Assign valid actor property
  #' @cx.tests Assign valid environment property
  test_rec[ c( "actor", "env" ) ] <- replicate( 2, 
                                                paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE ), collapse = "" ), 
                                                simplify = TRUE )
  
  
  
  # - test audit record object
  test_obj <- cxaudit::cxaudit_record()

  # - assign props  
  test_obj$setproperties( test_rec )
  
  
  # -- test
  result <- test_obj$getproperties()
  

  # -- assertions
  
  # - has properties  
  testthat::expect_true( all( base::sort(base::names(result)) %in% cxaudit:::.cxaudit_propertynames() ) )
  testthat::expect_true( all( cxaudit:::.cxaudit_propertynames() %in% base::sort(base::names(result)) ) )
  
  # - has property values
  testthat::expect_equal( result[ base::sort(base::names(test_rec)) ], test_rec[ base::sort(base::names(test_rec)) ] )
  
  #' @cx.tests Properties returned does not include a value equal to NA
  testthat::expect_false( any(is.na( result )) )
  
})





testthat::test_that( "cxaudit.record.getPropOnlySet", {
  
  
  #' @cx.tests Get properties when a subset of properties are defined  
  
  
  # -- stage
  
  test_rec <- list()

  
  # - exclude object.path and object.hash  
  
  # - add event
  #' @cx.test Assign a valid event property
  test_rec[["event"]] <- sample( cxaudit:::.cxaudit_eventnames(), 1 )
  
  # - add type and class
  #' @cx.test Assign a valid object type property
  #' @cx.test Assign a valid object class property
  test_rec[ c( "object.type", "object.class" ) ] <- replicate( 2, 
                                                               paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE ), collapse = "" ), 
                                                               simplify = TRUE )
  

  # - add label
  #' @cx.test Assign a valid record label
  test_rec[["label"]] <- paste( replicate( sample( c(5:15), 1), 
                                           paste( sample( c( base::letters, as.character(0:9) ), 
                                                          sample( c(5:25), 1),
                                                          replace = TRUE ), collapse = "" ), 
                                           simplify = TRUE ), collapse = " " )
  
  #' @cx.test Assign a valid actor property
  #' @cx.test Assign a valid environment property
  test_rec[ c( "actor", "env" ) ] <- replicate( 2, 
                                                paste( sample( c( base::letters, as.character(0:9) ), 15, replace = TRUE ), collapse = "" ), 
                                                simplify = TRUE )
  
  
  
  # - test audit record object
  test_obj <- cxaudit::cxaudit_record()
  
  # - assign props  
  test_obj$setproperties( test_rec )
  
  
  # -- test
  result <- test_obj$getproperties()
  
  
  # -- expected
  
  # - expect missing properties
  expected_missing_prop <- c( "object.path", "object.hash" )
  
  
  # -- assertions
  
  # - has properties  
  
  #' @cx.tests Known valid properties returned
  testthat::expect_true( all( base::sort(base::names(result)) %in% cxaudit:::.cxaudit_propertynames() ) )
  testthat::expect_false( all( cxaudit:::.cxaudit_propertynames() %in% base::sort(base::names(result)) ) )
  
  #' @cx.tests Unassigned and valid known properties not returned
  testthat::expect_false( any( expected_missing_prop %in% base::names(result) ) )
  

  # - has property values
  testthat::expect_equal( result[ base::sort(base::names(test_rec)) ], test_rec[ base::sort(base::names(test_rec)) ] )
  
  
})




testthat::test_that( "cxaudit.record.setAttribNull", {

  # -- stage
  test_obj <- cxaudit::cxaudit_record()
  
  
  
  # -- test
  #' @cx.tests Set attributes equal to NULL results in an error
  testthat::expect_error( test_obj$setattributes(NULL), regexp = "Attributes are missing or in an invalid format" )

})
  


testthat::test_that( "cxaudit.record.setAttribNA", {
  
  # -- stage
  test_obj <- cxaudit::cxaudit_record()
  
  
  
  # -- test
  #' @cx.tests Set attributes equal to NA results in an error
  testthat::expect_error( test_obj$setattributes(NA), regexp = "Attributes are missing or in an invalid format" )
  
})




testthat::test_that( "cxaudit.record.setAttribEmptyVector", {
  
  # -- stage
  test_obj <- cxaudit::cxaudit_record()
  
  
  
  # -- test
  #' @cx.tests Set attributes equal to a vector of invalid type results in an error
  testthat::expect_error( test_obj$setattributes(character(0)), regexp = "Attributes are missing or in an invalid format" )
  
})



testthat::test_that( "cxaudit.record.setAttribInvalidType", {
  
  # -- stage
  
  # - test attributes
  test_attr <- 1
  base::names(test_attr) <- "label"
   

  if ( inherits( test_attr, "character") )
    testthat::fail( "Failed to create an invalid vector" )
  
  
    
  # - test record
  test_obj <- cxaudit::cxaudit_record()
  
  
  
  # -- test
  #' @cx.tests Set attributes equal to a vector of invalid type results in an error
  testthat::expect_error( test_obj$setattributes( test_attr ), regexp = "Attributes are missing or in an invalid format" )
  
})



testthat::test_that( "cxaudit.record.setAttribUnnamed", {
  
  # -- stage
  
  # - test attributes
  test_attr <- paste( replicate( 10, 
                                 paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), "."), sample( c(5:15), 1 ), replace = TRUE), collapse = "" ),
                                 simplify = TRUE ), 
                      collapse = " " )
  
  
  if ( ! inherits( test_attr, "character") || ! is.null( base::names(test_attr)) )
    testthat::fail( "Failed to create an invalid vector" )
  
  
  
  # - test record
  test_obj <- cxaudit::cxaudit_record()
  
  
  
  # -- test
  #' @cx.tests Set attributes equal to a vector of unnamed elements results in an error
  testthat::expect_error( test_obj$setattributes( test_attr ), regexp = "Attributes are missing or in an invalid format" )
  
})



testthat::test_that( "cxaudit.record.setAttribUnnamedElement", {
  
  # -- stage
  
  # - test attributes
  test_attr <- paste( replicate( 10, 
                                 paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), "."), sample( c(5:15), 1 ), replace = TRUE), collapse = "" ),
                                 simplify = TRUE ), 
                      collapse = " " )
  
  base::names(test_attr) <- "test"
  

  test_attr <- append( test_attr,
                       paste( replicate( 10, 
                                         paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), "."), sample( c(5:15), 1 ), replace = TRUE), collapse = "" ),
                                         simplify = TRUE ),
                              collapse = " " ) )
  

  if ( ! inherits( test_attr, "character") || is.null(base::names(test_attr)) || all( base::names(test_attr) != "" ) )
    testthat::fail( "Failed to create an invalid vector" )
  
  
  
  # - test record
  test_obj <- cxaudit::cxaudit_record()
  
  
  
  # -- test
  #' @cx.tests Set attributes equal to a vector with one element unnamed results in an error
  testthat::expect_error( test_obj$setattributes( test_attr ), regexp = "Attributes are missing or in an invalid format" )
  
})



testthat::test_that( "cxaudit.record.setAttribInvalidAttrName", {
  
  # -- stage
  
  # - test attributes
  test_attr <- paste( replicate( 10, 
                                 paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), "."), sample( c(5:15), 1 ), replace = TRUE), collapse = "" ),
                                 simplify = TRUE ), 
                      collapse = " " )
  
  
  #   note: attribute names are references
  #   note: align with cxaudit::cxaudit_validreference
  base::names(test_attr) <- paste( c( "_", base::sample( c( base::LETTERS, base::letters, as.character(0:9), "_"), 20, replace = TRUE), "_"), collapse = "" )
  


  if ( ! inherits( test_attr, "character") || is.null(base::names(test_attr)) || any( base::names(test_attr) == "" ) || cxaudit::cxaudit_validreference(base::names(test_attr)) )
    testthat::fail( "Failed to create an invalid vector" )
  
  
  
  # - test record
  test_obj <- cxaudit::cxaudit_record()
  
  
  
  # -- test
  #' @cx.tests Set attributes containing an element with an invalid attribute name results in an error
  testthat::expect_error( test_obj$setattributes( test_attr ), regexp = 
                            paste( "Attribute", base::tolower(base::names(test_attr)), "is not a valid attribute reference" ) )
  
})


testthat::test_that( "cxaudit.record.setAttribInvalidAttrQualifier", {
  
  # -- stage
  
  # - test attributes
  test_attr <- paste( replicate( 10, 
                                 paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), "."), sample( c(5:15), 1 ), replace = TRUE), collapse = "" ),
                                 simplify = TRUE ), 
                      collapse = " " )
  
  
  #   note: attribute names are references
  #   note: align with cxaudit::cxaudit_validreference
  
  test_attr_name <- paste( paste( c( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), sample( 1:5, 1), replace = TRUE)), collapse = "" ),
                           paste( c( base::sample( c( base::LETTERS, base::letters, as.character(0:9), "_"), 20, replace = TRUE)), collapse = "" ),
                           paste( c( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), sample( 1:5, 1), replace = TRUE)), collapse = "" ),
                           sep = "")

  if ( ! cxaudit::cxaudit_validreference( test_attr_name ) )
    testthat::fail( "Failed to create an valid attribute name" )
  
  
  test_attr_qual <- paste( c(  "_", base::sample( c( base::LETTERS, base::letters, as.character(0:9), "_"), 20, replace = TRUE), "_"), collapse = "" )
  
  if ( cxaudit::cxaudit_validreference( test_attr_qual ) )
    testthat::fail( "Failed to create an invalid attribute qualifier" )
  
  
  
  base::names(test_attr) <- paste( test_attr_name, test_attr_qual, sep = ":" )
  

  if ( ! inherits( test_attr, "character") || is.null(base::names(test_attr)) || any( base::names(test_attr) == "" ) )
    testthat::fail( "Failed to create an invalid vector" )
  
  
  
  # - test record
  test_obj <- cxaudit::cxaudit_record()
  
  
  
  # -- test
  #' @cx.tests Set attributes containing an element with an invalid attribute qualifier results in an error
  testthat::expect_error( test_obj$setattributes( test_attr ), regexp = 
                            paste( "Attribute", base::tolower(base::names(test_attr)), "is not a valid attribute reference" ) )
  
})



testthat::test_that( "cxaudit.record.setAttribSingleElement", {
  
  # -- stage
  
  
  # - attribute
  
  test_attr <- paste( replicate( 10, 
                                 paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), "."), sample( c(5:15), 1 ), replace = TRUE), collapse = "" ),
                                 simplify = TRUE ), 
                      collapse = " " )
  
  base::names(test_attr) <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9) ), 20, replace = TRUE), collapse = "" )
  
  if ( ! inherits( test_attr, "character") || ! cxaudit::cxaudit_validreference( base::names(test_attr) ) )
    testthat::fail( "Unexpectedly generated an invalid vector" )
  
  
  # - test record
  test_obj <- cxaudit::cxaudit_record()
  

  # -- test
  result <- test_obj$setattributes( test_attr )
  
  
  # -- expected
  expected_attr <- test_attr
  base::names(expected_attr) <- base::tolower(base::names(test_attr))
  
  # -- assertions

  #' @cx.test Attribute assigned
  testthat::expect_equal( result, expected_attr )

})


testthat::test_that( "cxaudit.record.setAttribMultiElement", {
  
  # -- stage
  
  
  # - attribute
  
  test_attr <- replicate( 5, 
                          paste( replicate( 10,
                                            paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), "."), sample( c(5:15), 1 ), replace = TRUE), collapse = "" ),
                                            simplify = TRUE ),
                                 collapse = " " ),
                          simplify = TRUE )
  
  base::names(test_attr) <- replicate( 5, 
                                       paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 20, replace = TRUE), collapse = "" ), 
                                       simplify = TRUE )
  
  if ( ! inherits( test_attr, "character") || any( ! cxaudit::cxaudit_validreference( base::names(test_attr) ) ) )
    testthat::fail( "Unexpectedly generated an invalid vector" )

  
  
  test_attr_qual <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 10, replace = TRUE), collapse = "" )
  
  if ( ! inherits( test_attr, "character") || any( ! cxaudit::cxaudit_validreference( test_attr_qual ) ) )
    testthat::fail( "Unexpectedly generated an invalid attribute qualifier" )
  

  # - add qualifier to last attribute  
  test_attr_names <- base::names(test_attr)
  test_attr_names[ length(test_attr_names) ] <- paste( utils::head( test_attr_names, n = 1 ), test_attr_qual, sep = ":" )

  base::names( test_attr ) <- test_attr_names
  
  
  # - test record
  test_obj <- cxaudit::cxaudit_record()
  
  
  # -- test
  result <- test_obj$setattributes( test_attr )
  
  
  # -- expected
  expected_attr <- test_attr
  base::names(expected_attr) <- base::tolower(base::names(test_attr))
  
  # -- assertions
  
  #' @cx.test Multiple attributes assigned with at least one containing a qualifier
  testthat::expect_equal( base::sort(base::names(result)), base::sort(base::names(expected_attr)) )
  testthat::expect_equal( result[ base::sort(base::names(result)) ], expected_attr[ base::sort(base::names(result)) ] )

})

  

testthat::test_that( "cxaudit.record.setAttribMultiElementMultiPart", {
  
  # -- stage
  
  
  # - attribute
  
  test_attr <- replicate( 5, 
                          paste( replicate( 10,
                                            paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), "."), sample( c(5:15), 1 ), replace = TRUE), collapse = "" ),
                                            simplify = TRUE ),
                                 collapse = " " ),
                          simplify = TRUE )
  
  base::names(test_attr) <- replicate( 5, 
                                       paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 20, replace = TRUE), collapse = "" ), 
                                       simplify = TRUE )
  
  if ( ! inherits( test_attr, "character") || any( ! cxaudit::cxaudit_validreference( base::names(test_attr) ) ) )
    testthat::fail( "Unexpectedly generated an invalid vector" )
  
  
  
  test_attr_qual <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 10, replace = TRUE), collapse = "" )
  
  if ( ! inherits( test_attr, "character") || any( ! cxaudit::cxaudit_validreference( test_attr_qual ) ) )
    testthat::fail( "Unexpectedly generated an invalid attribute qualifier" )
  
  
  # - add qualifier to last attribute  
  test_attr_names <- base::names(test_attr)
  test_attr_names[ length(test_attr_names) ] <- paste( utils::head( test_attr_names, n = 1 ), test_attr_qual, sep = ":" )
  
  base::names( test_attr ) <- base::tolower(test_attr_names)
  
  
  # - test record
  test_obj <- cxaudit::cxaudit_record()
  
  
  # - stage initial set of attributes
  test_obj_result <- test_obj$setattributes( test_attr )


  if ( ( length(test_obj_result) != length(test_attr) ) ||
       ! all( base::sort(base::names(test_obj_result)) == base::sort(base::names(test_attr) ) ) ||
       ! all( test_obj_result[ base::sort(base::names(test_obj_result)) ] == test_attr[ base::sort(base::names(test_obj_result)) ] ) )
    testthat::fail( "Could not stage test record with attributes")

      
  
  # - update first attribute
  
  test_attr_updt_name <- utils::head( base::names(test_attr), n = 1 )
  
  
  test_attr_updt_value <- paste( replicate( 10,
                                            paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), "."), sample( c(5:15), 1 ), replace = TRUE), collapse = "" ),
                                            simplify = TRUE ),
                                 collapse = " " )
  
  if ( test_attr[ test_attr_updt_name ] == test_attr_updt_value )
    testthat::fail( "Could not generate a new unique update value" )

  
  test_attr_updt <- test_attr_updt_value
  base::names(test_attr_updt) <-test_attr_updt_name
  

  # -- test
  result <- test_obj$setattributes( test_attr_updt )
  

  
  # -- expected

  # - oritingal set of attributes
  expected_attr <- test_attr
  
  # - apply update
  expected_attr[ test_attr_updt_name ] <- test_attr_updt_value

  
  # -- assertions

  #' @cx.tests Update existing attribute
  testthat::expect_equal( base::sort(base::names(result)), base::sort(base::names(expected_attr)) )
  testthat::expect_equal( result[ base::sort(base::names(result)) ], expected_attr[ base::sort(base::names(result)) ] )
  
})



testthat::test_that( "cxaudit.record.getAttribNoneAssigned", {
  
  # -- stage 
  
  test_obj <- cxaudit::cxaudit_record()
  
  
  # -- test
  result <- test_obj$getattributes()

  
  # -- assertions
  #' @cx.tests Empty character vector of attributes is returned when no attributes are assigned
  testthat::expect_false( is.null(result))
  testthat::expect_true( inherits( result, "character") )
  testthat::expect_length( result, 0 )

})




testthat::test_that( "cxaudit.record.getAttribMultiAssigned", {
  
  # -- stage
  
  
  # - attribute
  
  test_attr <- replicate( 5, 
                          paste( replicate( 10,
                                            paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), "."), sample( c(5:15), 1 ), replace = TRUE), collapse = "" ),
                                            simplify = TRUE ),
                                 collapse = " " ),
                          simplify = TRUE )
  
  base::names(test_attr) <- replicate( 5, 
                                       paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 20, replace = TRUE), collapse = "" ), 
                                       simplify = TRUE )
  
  if ( ! inherits( test_attr, "character") || any( ! cxaudit::cxaudit_validreference( base::names(test_attr) ) ) )
    testthat::fail( "Unexpectedly generated an invalid vector" )
  
  
  
  test_attr_qual <- paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9)), 10, replace = TRUE), collapse = "" )
  
  if ( ! inherits( test_attr, "character") || any( ! cxaudit::cxaudit_validreference( test_attr_qual ) ) )
    testthat::fail( "Unexpectedly generated an invalid attribute qualifier" )

  
  # - add qualifier to last attribute  
  test_attr_names <- base::names(test_attr)
  test_attr_names[ length(test_attr_names) ] <- paste( utils::head( test_attr_names, n = 1 ), test_attr_qual, sep = ":" )
  
  base::names( test_attr ) <- base::tolower(test_attr_names)
  
  
  
  
  # - test record
  test_obj <- cxaudit::cxaudit_record()


  # - stage initial set of attributes
  test_obj_result <- test_obj$setattributes( test_attr )


  if ( ( length(test_obj_result) != length(test_attr) ) ||
       ! all( base::sort(base::names(test_obj_result)) == base::sort(base::names(test_attr) ) ) ||
       ! all( test_obj_result[ base::sort(base::names(test_obj_result)) ] == test_attr[ base::sort(base::names(test_obj_result)) ] ) )
    testthat::fail( "Could not stage test record with attributes")


  # - update first attribute

  test_attr_updt_name <- utils::head( base::names(test_attr), n = 1 )

  test_attr_updt_value <- paste( replicate( 10,
                                            paste( base::sample( c( base::LETTERS, base::letters, as.character(0:9), "."), sample( c(5:15), 1 ), replace = TRUE), collapse = "" ),
                                            simplify = TRUE ),
                                 collapse = " " )

  if ( test_attr[ test_attr_updt_name ] == test_attr_updt_value )
    testthat::fail( "Could not generate a new unique update value" )

  
  test_attr_updt <- test_attr_updt_value
  base::names( test_attr_updt ) <- test_attr_updt_name
  

  test_obj_updt <- test_obj$setattributes( test_attr_updt )



  # -- test
  result <- test_obj$getattributes()



  # -- expected

  # - oritingal set of attributes
  expected_attr <- test_attr

  # - apply update
  expected_attr[ test_attr_updt_name ] <- test_attr_updt_value


  # -- assertions

  #' @cx.test Update existing attribute
  testthat::expect_equal( base::sort(base::names(result)), base::sort(base::names(expected_attr)) )
  testthat::expect_equal( result[ base::sort(base::names(result)) ], expected_attr[ base::sort(base::names(result)) ] )
  
})



testthat::test_that( "cxaudit.record.internalSetLinksNull", {
  
  
  # -- stage
  
  test_obj <- cxaudit::cxaudit_record()
  
  
  # -- test
  #' @cx.tests Internal function set links returns error when list of links is equal to NULL
  testthat::expect_error( test_obj$.setlinks(NULL), regexp = "^List of linked records missing or in an invalid format$" )
  
})




testthat::test_that( "cxaudit.record.internalSetLinksNA", {

  # -- stage
  
  test_obj <- cxaudit::cxaudit_record()
  
  
  # -- test
  #' @cx.tests Internal function set links returns error when list of links is equal to NA
  testthat::expect_error( test_obj$.setlinks(NA), regexp = "^List of linked records missing or in an invalid format$" )
  
})



testthat::test_that( "cxaudit.record.internalSetLinksInvalidType", {
  
  # -- stage
  
  test_obj <- cxaudit::cxaudit_record()
  
  
  # -- test
  #' @cx.tests Internal function set links returns error when list of links is not a list
  testthat::expect_error( test_obj$.setlinks( as.character("lksjd") ), regexp = "^List of linked records missing or in an invalid format$" )
  
})



testthat::test_that( "cxaudit.record.internalSetLinksEmptyList", {
  
  # -- stage
  
  test_obj <- cxaudit::cxaudit_record()
  
  
  # -- test
  #' @cx.tests Internal function set links returns error when list is empty
  testthat::expect_error( test_obj$.setlinks( list() ), regexp = "^List of linked records missing or in an invalid format$" )
  
})




testthat::test_that( "cxaudit.record.internalSetLinksListInvalidEntryType", {
  
  # -- stage
  
  test_obj <- cxaudit::cxaudit_record()
  
  
  # -- test
  #' @cx.tests Internal function set links returns error when list entry is not an audit record
  testthat::expect_error( test_obj$.setlinks( list( "lkjdslkjdsflk") ), regexp = "^Expecting a list entry of type cxaudit::cxaudit_record$" )
  
})




testthat::test_that( "cxaudit.record.internalSetLinks", {
  
  # -- stage

  test_links <- as.list(replicate( 5, cxaudit::cxaudit_record() ) )
  

  test_obj <- cxaudit::cxaudit_record()
  
  
  # -- test
  
  result <- test_obj$.setlinks( test_links )
  
  
  # -- expected
  
  expected_links <- list()
  
  for ( xitem in test_links )
    expected_links[[ xitem$getproperties()[["id"]] ]] <- xitem
  
  
  # -- assertions
  
  actual_links <- list()
  
  for ( xresult in result )
    actual_links[[ xresult$getproperties()[["id"]] ]] <- xresult
  

  # - expected records 
  #   note: using IDs as surrogates
  #' @cx.tests Internal function set links 
  testthat::expect_true( all( base::sort(base::names(expected_links)) %in% base::sort(base::names(actual_links)) ) )

})



testthat::test_that( "cxaudit.record.internalSetLinksAmendLinks", {
  
  # -- stage
  
  test_links <- as.list(replicate( 5, cxaudit::cxaudit_record() ) )
  
  # - audit record
  test_obj <- cxaudit::cxaudit_record()
  
  
  # - set initial links
  test_obj_result <- test_obj$.setlinks( utils::head( test_links, n = length(test_links) - 2 ) )
  
  
  # -- test
  result <- test_obj$.setlinks( utils::tail( test_links, n = 2 ) )
  
  
  
  # -- expected
  
  expected_links <- list()
  
  for ( xitem in test_links )
    expected_links[[ xitem$getproperties()[["id"]] ]] <- xitem
  
  
  # -- assertions
  
  actual_links <- list()
  
  for ( xresult in result )
    actual_links[[ xresult$getproperties()[["id"]] ]] <- xresult
  
  
  # - expected records 
  #   note: using IDs as surrogates
  #' @cx.tests Internal function amend links 
  testthat::expect_true( all( base::sort(base::names(expected_links)) %in% base::sort(base::names(actual_links)) ) )

})




testthat::test_that( "cxaudit.record.getLinks", {
  
  # -- stage
  
  test_links <- as.list(replicate( 5, cxaudit::cxaudit_record() ) )
  
  # - test record
  test_obj <- cxaudit::cxaudit_record()

  # - add test links
  test_obj_result <- test_obj$.setlinks( test_links )
  
  
    
  # -- test
  result <- test_obj$getlinks()
  
  
  
  # -- expected
  
  expected_links <- list()
  
  for ( xitem in test_links )
    expected_links[[ xitem$getproperties()[["id"]] ]] <- xitem
  
  
  # -- assertions
  
  actual_links <- list()
  
  for ( xresult in result )
    actual_links[[ xresult$getproperties()[["id"]] ]] <- xresult
  
  
  # - expected records 
  #   note: using IDs as surrogates
  #' @cx.tests Get set links 
  testthat::expect_true( all( base::sort(base::names(expected_links)) %in% base::sort(base::names(actual_links)) ) )
  
})






testthat::test_that( "cxaudit.record.getPropIntegtityCheck", {
  
  # -- stage
  
  test_links <- as.list(replicate( 5, cxaudit::cxaudit_record() ) )
  
  # - test record
  test_obj <- cxaudit::cxaudit_record()
  
  # - add test links
  test_obj_result <- test_obj$.setlinks( test_links )
  
  

  # - test record
  test_obj <- cxaudit::cxaudit_record()
  

  # - inject integrity issue
  #   note: all properties are required
  #   note: drop one random property
  #   note: sample() of length(props) - 1 and no replace means all properties except 1 is sampled
  keep_props <- c( sample( cxaudit:::.cxaudit_propertynames(), length(cxaudit:::.cxaudit_propertynames()) - 1, replace = FALSE ), 
                   "attributes", "links" )

  test_obj$.attr <- test_obj$.attr[ base::names(test_obj$.attr) %in% keep_props ]
  
  # -- test
  #' @cx.tests Missing property results in record integrity check failing
  testthat::expect_error( test_obj$getproperties(), regexp = "^Audit record properties missing$" )

})

