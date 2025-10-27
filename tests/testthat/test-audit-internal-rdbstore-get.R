#
#
# Tests for relational database store
#
# Get audit record
#

#' @cx.testsfor cxaudit:::.cxaudit_rdbstore()



testthat::test_that( "audit.rdbstore.getMissingParam", {
  
  # -- stage
  
  # - move global in-memory cached config
  #   note: align with cxapp:::.cxappconfig()
  
  prev_config <- NA
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  on.exit( {
    
    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
    
    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )
    
  }, add = TRUE )
  
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )  
  
  
  
  # - database connection
  dbcon <- base::get( "testdbcon", envir = cxaudit_test_env )
  
  
  # - store
  dbstore <- cxaudit:::.cxaudit_rdbstore( dbcon )
  
  
  # -- test
  #' @cx.tests Get audit record with no record identifier specified
  testthat::expect_error( dbstore$get(), regexp = "^Audit record identifier missing or not in a valid format$" )
  
})




testthat::test_that( "audit.rdbstore.getParamNull", {
  
  # -- stage
  
  # - move global in-memory cached config
  #   note: align with cxapp:::.cxappconfig()
  
  prev_config <- NA
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  on.exit( {
    
    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
    
    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )
    
  }, add = TRUE )
  
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )  
  
  
  
  # - database connection
  dbcon <- base::get( "testdbcon", envir = cxaudit_test_env )
  
  
  # - store
  dbstore <- cxaudit:::.cxaudit_rdbstore( dbcon )
  
  
  # -- test
  #' @cx.tests Get audit record with record identifier equal to NULL
  testthat::expect_error( dbstore$get(NULL), regexp = "^Audit record identifier missing or not in a valid format$" )
  
})



testthat::test_that( "audit.rdbstore.getParamNA", {
  
  # -- stage
  
  # - move global in-memory cached config
  #   note: align with cxapp:::.cxappconfig()
  
  prev_config <- NA
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  on.exit( {
    
    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
    
    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )
    
  }, add = TRUE )
  
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )  
  
  
  
  # - database connection
  dbcon <- base::get( "testdbcon", envir = cxaudit_test_env )
  
  
  # - store
  dbstore <- cxaudit:::.cxaudit_rdbstore( dbcon )
  
  
  # -- test
  #' @cx.tests Get audit record with record identifier equal to NA
  testthat::expect_error( dbstore$get(NA), regexp = "^Audit record identifier missing or not in a valid format$" )
  
})



testthat::test_that( "audit.rdbstore.getParamInvalidType", {
  
  # -- stage
  
  # - move global in-memory cached config
  #   note: align with cxapp:::.cxappconfig()
  
  prev_config <- NA
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  on.exit( {
    
    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
    
    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )
    
  }, add = TRUE )
  
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )  
  
  
  
  # - database connection
  dbcon <- base::get( "testdbcon", envir = cxaudit_test_env )
  
  
  # - store
  dbstore <- cxaudit:::.cxaudit_rdbstore( dbcon )
  
  
  # -- test
  #' @cx.tests Get audit record with record identifier of an invalid type
  testthat::expect_error( dbstore$get(as.numeric(0)), regexp = "^Audit record identifier missing or not in a valid format$" )
  
})





testthat::test_that( "audit.rdbstore.getParamZeroLength", {
  
  # -- stage
  
  # - move global in-memory cached config
  #   note: align with cxapp:::.cxappconfig()
  
  prev_config <- NA
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  on.exit( {
    
    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
    
    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )
    
  }, add = TRUE )
  
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )  
  
  
  
  # - database connection
  dbcon <- base::get( "testdbcon", envir = cxaudit_test_env )
  
  
  # - store
  dbstore <- cxaudit:::.cxaudit_rdbstore( dbcon )
  
  
  # -- test
  #' @cx.tests Get audit record with record identifier of an invalid type
  testthat::expect_error( dbstore$get(character(0)), regexp = "^Audit record identifier missing or not in a valid format$" )
  
})



testthat::test_that( "audit.rdbstore.getParamNotUUIDFormat", {
  
  # -- stage
  
  # - move global in-memory cached config
  #   note: align with cxapp:::.cxappconfig()
  
  prev_config <- NA
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  on.exit( {
    
    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
    
    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )
    
  }, add = TRUE )
  
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )  
  
  
  
  # - database connection
  dbcon <- base::get( "testdbcon", envir = cxaudit_test_env )
  
  
  # - store
  dbstore <- cxaudit:::.cxaudit_rdbstore( dbcon )
  
  # - reference ID
  test_id <- paste( sample( base::letters, 40, replace = TRUE ), collapse = "" )
  
  if ( uuid::UUIDvalidate( test_id ) )
    testthat::fail( "Unexpected test ID is a valid UUID format" )
  
  
  # -- test
  #' @cx.tests Get audit record with record identifier of an invalid type
  testthat::expect_error( dbstore$get( test_id  ), regexp = "^Audit record identifier missing or not in a valid format$" )
  
})



testthat::test_that( "audit.rdbstore.getNoRecords", {
  
  # -- stage
  
  # - move global in-memory cached config
  #   note: align with cxapp:::.cxappconfig()
  
  prev_config <- NA
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  on.exit( {
    
    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
    
    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )
    
  }, add = TRUE )
  
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )  
  
  
  
  # - database connection
  dbcon <- base::get( "testdbcon", envir = cxaudit_test_env )

  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) > 0 )
    testthat::fail( "Collection of records already exists" )


  # - reference ID
  test_id <- uuid::UUIDgenerate()
  
  if ( ! uuid::UUIDvalidate( test_id ) )
    testthat::fail( "Unexpected test ID is not in a valid UUID format" )

  
  test_recs <- DBI::dbGetQuery( dbcon, paste( "select cast(uid as varchar(128)) as uid from tbl_adt_records where ( uid =", base::sQuote( test_id, q = FALSE), ");" ) )
  
  if ( base::nrow(test_recs) > 0 )
    testthat::fail( "A record with test id already exists" )

  
  # - store
  dbstore <- cxaudit:::.cxaudit_rdbstore( dbcon )
 
  
  # -- test
  result <- dbstore$get( test_id )

  
  # -- assertions
  #' @cx.tests Get audit record when a record ID does not represent a valid database record results in Null
  testthat::expect_null( result )
  
})




testthat::test_that( "audit.rdbstore.getSingleRecordExistsNoAttr", {
  
  # -- stage
  
  # - move global in-memory cached config
  #   note: align with cxapp:::.cxappconfig()
  
  prev_config <- NA
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  on.exit( {
    
    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
    
    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )
    
  }, add = TRUE )
  
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )  
  
  
  
  # - database connection
  dbcon <- base::get( "testdbcon", envir = cxaudit_test_env )
  
  # - reference ID
  test_id <- uuid::UUIDgenerate()
  
  if ( ! uuid::UUIDvalidate( test_id ) )
    testthat::fail( "Unexpected test ID is not in a valid UUID format" )
  
  
  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast(uid as varchar(128)) as uid from tbl_adt_records where ( uid =", base::sQuote( test_id, q = FALSE), ");" ) ) ) > 0 )
    testthat::fail( "A record with test id already exists" )
  

  # - store
  dbstore <- cxaudit:::.cxaudit_rdbstore( dbcon )

  
  # - test record
  
  test_rec_obj_props <- list( "id" = test_id, 
                              "event" = base::sample( cxaudit:::.cxaudit_eventnames(), 1),
                              "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                              "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                              "object.path" = paste( c( "", replicate( 5, 
                                                                       paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                       simplify = TRUE)), collapse = "/"),
                              "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), 
                                                              algo = "sha1", 
                                                              file = FALSE ),
                              "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")), 
                              "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                              "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") )
  
  test_rec_obj <- cxaudit::cxaudit_record( test_rec_obj_props )
  
  testthat::expect_true( dbstore$commit( test_rec_obj ) )

  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast(uid as varchar(128)) as uid from tbl_adt_records where ( uid =", base::sQuote( test_id, q = FALSE), ");" ) ) ) != 1 )
    testthat::fail( "Failed to stage record" )
  

  # - record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon, 
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  
  # -- test
  result <- dbstore$get( test_id )
  
  
  # -- expected
  expected_props <- test_rec_obj$getproperties()
  

  # -- assertions
  
  # - record
  #' @cx.tests Get audit record returns an audit record with record details
  testthat::expect_equal( as.character(class(result)), "cxaudit_record" )
  testthat::expect_equal( attr( class(result), "package"), "cxaudit" )
  
  
  # - record properties
  actual_props <- result$getproperties()
  
  testthat::expect_equal( base::sort(base::names(actual_props)), base::sort(base::names(expected_props)) )

  # - assert all but datetime    
  prop_names <- union( base::sort(base::names(actual_props)), base::sort(base::names(expected_props)) )
  testthat::expect_equal( actual_props[ prop_names[ prop_names != "datetime" ] ], expected_props[ prop_names[ prop_names != "datetime" ] ])  
  
  # - assert datetime    
  testthat::expect_equal( trunc.POSIXt( actual_props[["datetime"]], units = "secs"), trunc.POSIXt( expected_props[["datetime"]], units = "secs" )  )
  
  
  # - assert attributes
  testthat::expect_length( result$getattributes(), 0 )
  
  # - assert links
  testthat::expect_length( result$getlinks(), 0 )
  

})







testthat::test_that( "audit.rdbstore.getSingleRecordExistsWithAttr", {
  
  # -- stage
  
  # - move global in-memory cached config
  #   note: align with cxapp:::.cxappconfig()
  
  prev_config <- NA
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  on.exit( {
    
    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
    
    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )
    
  }, add = TRUE )
  
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )  
  
  
  
  # - database connection
  dbcon <- base::get( "testdbcon", envir = cxaudit_test_env )
  
  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) > 0 )
    testthat::fail( "Collection of records already exists" )
  
  
  # - reference ID
  test_id <- uuid::UUIDgenerate()
  
  if ( ! uuid::UUIDvalidate( test_id ) )
    testthat::fail( "Unexpected test ID is not in a valid UUID format" )
  
  
  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast(uid as varchar(128)) as uid from tbl_adt_records where ( uid =", base::sQuote( test_id, q = FALSE), ");" ) ) ) > 0 )
    testthat::fail( "A record with test id already exists" )
  
  
  # - store
  dbstore <- cxaudit:::.cxaudit_rdbstore( dbcon )
  
  
  # - static test records
  #   note: first use of some volume  
  test_obj_recs <- replicate( 100, 
                              cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(), 
                                                             "event" = base::sample( cxaudit:::.cxaudit_eventnames(), 1),
                                                             "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                             "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                             "object.path" = paste( c( "", replicate( 5, 
                                                                                                      paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                                                      simplify = TRUE)), collapse = "/"),
                                                             "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), 
                                                                                             algo = "sha1", 
                                                                                             file = FALSE ),
                                                             "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")), 
                                                             "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                             "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") ) ), 
                              simplify = TRUE )
  
  

  # - commit in blocks of 10
  for ( xblock in 0:9 )
    testthat::expect_true( dbstore$commit( test_obj_recs[ (10*xblock + 1):(10*(xblock + 1)) ] ) )
  
  
  
  # - test record
  
  test_rec <- cxaudit::cxaudit_record( list( "id" = test_id, 
                                             "event" = base::sample( cxaudit:::.cxaudit_eventnames(), 1),
                                             "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                             "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                             "object.path" = paste( c( "", replicate( 5, 
                                                                                      paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                                      simplify = TRUE)), collapse = "/"),
                                             "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), 
                                                                             algo = "sha1", 
                                                                             file = FALSE ),
                                             "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")), 
                                             "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                             "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") ) )
  
  #   attributes
  
  test_rec_attrs <- replicate( 5, 
                               paste( sample( base::letters, 30, replace = TRUE), collapse = "" ),
                               simplify = TRUE )
  
  base::names( test_rec_attrs ) <- replicate( 5, 
                                              paste( sample( base::letters, 10, replace = TRUE), collapse = "" ),
                                              simplify = TRUE ) 
  
  
  test_rec$setattributes( test_rec_attrs )
  
  
  #   commit test record
  testthat::expect_true( dbstore$commit( test_rec ) )
  
  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select distinct cast( uid as varchar(128)) as uid from tbl_adt_records;" ) ) ) != length(test_obj_recs) + 1 )
    testthat::fail( "Failed to stage record" )
  
  
  # - record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon, 
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  
  # -- test
  result <- dbstore$get( test_id )
  

  # -- expected
  
  # - properties
  expected_props <- test_rec$getproperties()

  # - attributes
  expected_attrs <- test_rec_attrs


  # -- assertions

  # - record
  #' @cx.tests Get audit record returns an audit record with record details
  testthat::expect_equal( as.character(class(result)), "cxaudit_record" )
  testthat::expect_equal( attr( class(result), "package"), "cxaudit" )


  # - record properties
  actual_props <- result$getproperties()

  testthat::expect_equal( base::sort(base::names(actual_props)), base::sort(base::names(expected_props)) )

  # - assert all but datetime
  prop_names <- union( base::sort(base::names(actual_props)), base::sort(base::names(expected_props)) )
  testthat::expect_equal( actual_props[ prop_names[ prop_names != "datetime" ] ], expected_props[ prop_names[ prop_names != "datetime" ] ])

  # - assert datetime
  testthat::expect_equal( trunc.POSIXt( actual_props[["datetime"]], units = "secs"), trunc.POSIXt( expected_props[["datetime"]], units = "secs" )  )


  # - assert attributes
  testthat::expect_length( result$getattributes(), length(expected_attrs) )

  actual_attrs <- result$getattributes()  
  
  testthat::expect_equal( base::sort(base::names(actual_attrs)), base::sort(base::names(expected_attrs))  )
  testthat::expect_equal( actual_attrs[ base::sort(base::names(actual_attrs)) ], expected_attrs[ base::sort(base::names(expected_attrs)) ]  )
  
  # - assert links
  testthat::expect_length( result$getlinks(), 0 )

  
})





testthat::test_that( "audit.rdbstore.getSingleRecordLinks", {
  
  # -- stage
  
  
  # - move global in-memory cached config
  #   note: align with cxapp:::.cxappconfig()
  
  prev_config <- NA
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    prev_config <- get( ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  on.exit( {
    
    if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
      base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
    
    if ( inherits( prev_config, "cxapp_config" ) )
      base::assign( ".cxapp.wrkcache.config", prev_config, envir = .GlobalEnv )
    
  }, add = TRUE )
  
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )  
  
  
  
  # - database connection
  dbcon <- base::get( "testdbcon", envir = cxaudit_test_env )

  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) > 0 )
    testthat::fail( "Collection of records already exists" )
  
  
  # - store
  dbstore <- cxaudit:::.cxaudit_rdbstore( dbcon )
  
  
  # - test records
  #   note: first use of some volume  
  test_obj_recs <- replicate( 100, 
                              cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(), 
                                                             "event" = base::sample( cxaudit:::.cxaudit_eventnames(), 1),
                                                             "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                             "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                             "object.path" = paste( c( "", replicate( 5, 
                                                                                                      paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                                                      simplify = TRUE)), collapse = "/"),
                                                             "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), 
                                                                                             algo = "sha1", 
                                                                                             file = FALSE ),
                                                             "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")), 
                                                             "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                             "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") ) ), 
                              simplify = TRUE )
  

  
  # - stage in blocks of 10
  #   note: first block to verify linked records
  for ( xblock in 1:9 )
    testthat::expect_true( dbstore$commit( test_obj_recs[ (10*xblock + 1):(10*(xblock + 1)) ] ) )


  # - stage block to assert links
  testthat::expect_true( dbstore$commit( test_obj_recs[ 1:10 ] ) )

  # - record id to query for links
  test_refobj <- test_obj_recs[[ 1 ]]
  test_id <- test_refobj$getproperties()[["id"]]
  
  

  # - record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon, 
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  
  # # -- test
  result <- dbstore$get( test_id )


  # -- expected 
  
  # - expected main record
  expected_obj <- test_refobj

  # - expected properties
  expected_props <- expected_obj$getproperties()
  
  # - links
  expected_links <- list()

  #   note: test reference object is index 1
  for ( xidx in 2:10 ) {
    xobj <- test_obj_recs[[ xidx ]]
    expected_links[[ xobj$getproperties()[["id"]] ]] <- xobj
  }


  
  # -- assertions
  
  # - record
  #' @cx.tests Get audit record returns an audit record with record links
  testthat::expect_equal( as.character(class(result)), "cxaudit_record" )
  testthat::expect_equal( attr( class(result), "package"), "cxaudit" )
  
  
  # - record properties
  actual_props <- result$getproperties()

  testthat::expect_equal( base::sort(base::names(actual_props)), base::sort(base::names(expected_props)) )

  # - assert all but datetime
  prop_names <- union( base::sort(base::names(actual_props)), base::sort(base::names(expected_props)) )
  testthat::expect_equal( actual_props[ prop_names[ prop_names != "datetime" ] ], expected_props[ prop_names[ prop_names != "datetime" ] ])

  # - assert datetime
  testthat::expect_equal( trunc.POSIXt( actual_props[["datetime"]], units = "secs"), trunc.POSIXt( expected_props[["datetime"]], units = "secs" )  )


  # - assert attributes
  testthat::expect_length( result$getattributes(), 0 )

  
  # - assert links
  testthat::expect_length( result$getlinks(), length(expected_links) )
  
  actual_links <- result$getlinks()
  
  actual_link_recs <- list()

  for ( xidx in 1:length(actual_links) ) {
    lnkobj <- actual_links[[ xidx ]]
    actual_link_recs[[ lnkobj$getproperties()[["id"]] ]] <- lnkobj
  }  

  testthat::expect_equal( base::sort(base::names(actual_link_recs)), base::sort(base::names(expected_links)) )  
  
  for ( xid in base::union( base::names(actual_link_recs), base::names(expected_links) ) ) 
    for ( xprop in base::union( base::names( actual_link_recs[[xid]]$getproperties() ), base::names( expected_links[[xid]]$getproperties() ) ) ) 
      testthat::expect_equal( actual_link_recs[[xid]]$getproperties()[[xprop]], expected_links[[xid]]$getproperties()[[xprop]] )


})




