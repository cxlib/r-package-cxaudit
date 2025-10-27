#
#
# Tests for relational database store
#
# List audit records

#' @cx.testsfor cxaudit:::.cxaudit_rdbstore()


#
#  Note: the number of assertions will vary due to randomness in number of 
#        records used by tests. The randomness is an integral part of testing
#        underlying and foundational data integrity
#



testthat::test_that( "audit.rdbstore.listAnyDefaults", {
  
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
  
  
  # -- test
  result <- dbstore$records()
  
  
  # -- assertions

  #' @cx.tests List audit records when no records exist in the database returns a list of length 0
  testthat::expect_equal( class(result), "list" )
  testthat::expect_length( result, 0 )
  
})







testthat::test_that( "audit.rdbstore.listAnyDefaults", {
  
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
  test_obj_recs <- replicate( 10, 
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
  

  
  # - stage 
  testthat::expect_true( dbstore$commit( test_obj_recs ) )
  

  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) != length(test_obj_recs) )
    testthat::fail( "Could not stage expected test records" )
  
  
  
  # - record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon, 
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  # -- test
  #' @cx.tests Return a collection of records using filter defaults
  result <- dbstore$records()
  
  
  # -- expected 
  
  expected_recs <- list()
  
  for ( xobj in test_obj_recs )
    expected_recs[[ xobj$getproperties()[["id"]] ]] <- xobj
  
  
  
  
  # -- assertions
  
  actual_ids <- character(0)
  
  for ( xresult in result ) {
    
    testthat::expect_true( inherits( xresult, "cxaudit_record" ) )
    testthat::expect_equal( attr(class(xresult), "package"), "cxaudit" )
   
    testthat::expect_true( xresult$getproperties()[["id"]] %in% base::names(expected_recs) )

    actual_ids <- append( actual_ids, xresult$getproperties()[["id"]] )
  }
    

  testthat::expect_equal( base::sort(actual_ids), base::sort(base::names(expected_recs)) )  

})





testthat::test_that( "audit.rdbstore.listFilterEvents", {
  
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
  
  
  # - target events
  test_events <- utils::head( cxaudit:::.cxaudit_eventnames(), n = 2 )

  
  # - test records

  #   target records
  test_obj_recs <- lapply( test_events, function(x) {
    
    cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(), 
                                   "event" = x,
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
  })
  
  
  

  #   noise records  
  test_misc_events <- cxaudit:::.cxaudit_eventnames()[ ! cxaudit:::.cxaudit_eventnames() %in% test_events ]

  test_obj_recs <- append( test_obj_recs,
                           replicate( 10, 
                                      cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(), 
                                                                     "event" = base::sample( test_misc_events, 1),
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
                                      simplify = TRUE ) )
  

  # - stage records 
  testthat::expect_true( dbstore$commit( test_obj_recs ) )
  
  
  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) != length(test_obj_recs) )
    testthat::fail( "Could not stage expected test records" )
  
  
  
  # - record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon, 
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  # -- test
  #' @cx.tests Return a collection of records for a specified selected list of events 
  result <- dbstore$records( list( "events" = test_events ) )
  
  
  # -- expected 
  
  expected_events <- test_events
  
  
  expected_recs <- list()
  
  for ( xobj in test_obj_recs ) 
    if ( xobj$getproperties()[["event"]] %in% expected_events )
      expected_recs[[ xobj$getproperties()[["id"]] ]] <- xobj

  
  
  
  
  # -- assertions
  
  # - records
  actual_ids <- character(0)
  
  for ( xresult in result ) {
    
    testthat::expect_true( inherits( xresult, "cxaudit_record" ) )
    testthat::expect_equal( attr(class(xresult), "package"), "cxaudit" )
    
    testthat::expect_true( xresult$getproperties()[["id"]] %in% base::names(expected_recs) )
    
    actual_ids <- append( actual_ids, xresult$getproperties()[["id"]] )
  }

  testthat::expect_equal( base::sort(actual_ids), base::sort(base::names(expected_recs)) )  

  
  # - filter attr
  
  actual_filter <- attr(result, "filter") 
  
  testthat::expect_false( is.null(actual_filter) )
  testthat::expect_true( "events" %in% base::names(actual_filter) )

  testthat::expect_equal( base::sort(actual_filter[["events"]]), base::sort(expected_events) )  

})






testthat::test_that( "audit.rdbstore.listFilterObjTypes", {
  
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
  
  
  # - target object types
  test_objtypes <- replicate( 4, paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), simplify = TRUE )
  
  
  # - test records

  #   target records
  test_obj_recs <- lapply( test_objtypes, function(x) {
    
    cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(), 
                                   "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                   "object.type" = x, 
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
  })
  
  
  
  
  #   noise records  
  
  #   note: noise object type is 5 characters longer than test type to force uniqueness between collections
  test_misc_types <- replicate( 100, paste( base::sample( c( base::letters, as.character(0:9)), 35, replace = TRUE), collapse = ""), simplify = TRUE )
  
  if ( any( test_misc_types %in% test_objtypes ) )
    testthat::fail( "Unexpected random miscallaneous object type equals a random generated test type" )
   
  
  test_obj_recs <- append( test_obj_recs,
                           replicate( 10, 
                                      cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(), 
                                                                     "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                                                     "object.type" = sample( test_misc_types, 1), 
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
                                      simplify = TRUE ) )
  
  
  # - stage records 
  testthat::expect_true( dbstore$commit( test_obj_recs ) )
  
  
  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) != length(test_obj_recs) )
    testthat::fail( "Could not stage expected test records" )
  
  
  
  # - record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon, 
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  # -- test
  #' @cx.tests Return a collection of records for a specified selected list of object types 
  result <- dbstore$records( list( "object.types" = test_objtypes ) )
  
  
  # -- expected 
  
  expected_objtypes <- test_objtypes
  
  
  expected_recs <- list()
  
  for ( xobj in test_obj_recs ) 
    if ( xobj$getproperties()[["object.type"]] %in% expected_objtypes )
      expected_recs[[ xobj$getproperties()[["id"]] ]] <- xobj
  
  
  # -- assertions
  
  actual_ids <- character(0)
  
  for ( xresult in result ) {
    
    testthat::expect_true( inherits( xresult, "cxaudit_record" ) )
    testthat::expect_equal( attr(class(xresult), "package"), "cxaudit" )
    
    testthat::expect_true( xresult$getproperties()[["id"]] %in% base::names(expected_recs) )
    
    actual_ids <- append( actual_ids, xresult$getproperties()[["id"]] )
  }

  testthat::expect_equal( base::sort(actual_ids), base::sort(base::names(expected_recs)) )  


  
  # - filter attr
  
  actual_filter <- attr(result, "filter") 
  
  testthat::expect_false( is.null(actual_filter) )
  testthat::expect_true( "object.types" %in% base::names(actual_filter) )
  
  testthat::expect_equal( base::sort(actual_filter[["object.types"]]), base::sort(expected_objtypes) )  
  
    
})





testthat::test_that( "audit.rdbstore.listFilterObjClass", {
  
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
  
  
  # - target object types
  test_objclass <- replicate( 4, paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), simplify = TRUE )
  
  
  # - test records

  #   target records
  test_obj_recs <- lapply( test_objclass, function(x) {
    
    cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(), 
                                   "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                   "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                   "object.class" = x, 
                                   "object.path" = paste( c( "", replicate( 5, 
                                                                            paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                            simplify = TRUE)), collapse = "/"),
                                   "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), 
                                                                   algo = "sha1", 
                                                                   file = FALSE ),
                                   "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")), 
                                   "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") ) )
  })
  
  
  
  
  #   noise records  
  
  #   note: noise object class is 5 characters longer than test class to force uniqueness between collections
  test_misc_classes <- replicate( 100, paste( base::sample( c( base::letters, as.character(0:9)), 35, replace = TRUE), collapse = ""), simplify = TRUE )
  
  if ( any( test_misc_classes %in% test_objclass ) )
    testthat::fail( "Unexpected random miscallaneous object class equals a random generated test class" )
  
  
  test_obj_recs <- append( test_obj_recs,
                           replicate( 10, 
                                      cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(), 
                                                                     "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                                                     "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                                     "object.class" = sample( test_misc_classes, 1), 
                                                                     "object.path" = paste( c( "", replicate( 5, 
                                                                                                              paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                                                              simplify = TRUE)), collapse = "/"),
                                                                     "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), 
                                                                                                     algo = "sha1", 
                                                                                                     file = FALSE ),
                                                                     "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")), 
                                                                     "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                     "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") ) ), 
                                      simplify = TRUE ) )
  
  
  # - stage records 
  testthat::expect_true( dbstore$commit( test_obj_recs ) )
  
  
  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) != length(test_obj_recs) )
    testthat::fail( "Could not stage expected test records" )
  
  
  
  # - record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon, 
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  # -- test
  #' @cx.tests Return a collection of records for a specified selected list of object classes 
  result <- dbstore$records( list( "object.classes" = test_objclass ) )
  
  
  # -- expected 
  
  expected_objclasses <- test_objclass
  
  
  expected_recs <- list()
  
  for ( xobj in test_obj_recs ) 
    if ( xobj$getproperties()[["object.class"]] %in% expected_objclasses )
      expected_recs[[ xobj$getproperties()[["id"]] ]] <- xobj
  
  
  # -- assertions
  
  actual_ids <- character(0)
  
  for ( xresult in result ) {
    
    testthat::expect_true( inherits( xresult, "cxaudit_record" ) )
    testthat::expect_equal( attr(class(xresult), "package"), "cxaudit" )
    
    testthat::expect_true( xresult$getproperties()[["id"]] %in% base::names(expected_recs) )
    
    actual_ids <- append( actual_ids, xresult$getproperties()[["id"]] )
  }
  
  testthat::expect_equal( base::sort(actual_ids), base::sort(base::names(expected_recs)) )  
  
  
  
  # - filter attr
  
  actual_filter <- attr(result, "filter") 
  
  testthat::expect_false( is.null(actual_filter) )
  testthat::expect_true( "object.classes" %in% base::names(actual_filter) )
  
  testthat::expect_equal( base::sort(actual_filter[["object.classes"]]), base::sort(expected_objclasses) )  
  
  
})




testthat::test_that( "audit.rdbstore.listFilterObjPathAsObject", {
  
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
  
  
  # - target object paths
  
  test_parentpath <- paste( c( "", replicate( 3, paste( base::sample( c( base::letters, as.character(0:9)), 20, replace = TRUE), collapse = ""), simplify = TRUE )), collapse = "/" )
  
  test_objpaths <- paste( test_parentpath, 
                          replicate( 4, paste( base::sample( c( base::letters, as.character(0:9)), 15, replace = TRUE), collapse = ""), simplify = TRUE ), 
                          sep = "/" )
  
  
  # - test records
  
  #   target records
  test_obj_recs <- lapply( test_objpaths, function(x) {
    
    cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(), 
                                   "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                   "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                   "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                   "object.path" = x,
                                   "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), 
                                                                   algo = "sha1", 
                                                                   file = FALSE ),
                                   "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")), 
                                   "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") ) )
  })
  
  
  
  
  #   noise records  
  
  #   note: noise object path elements are 5 characters longer than test paths to force uniqueness between collections
  #   note: using sampling to add some randomness to the paths
  test_misc_paths <- replicate( 100, 
                                paste( c( "", replicate( sample( 3:10, 1), 
                                                         paste( base::sample( c( base::letters, as.character(0:9)), sample( 10:25, 1), replace = TRUE), collapse = ""),
                                                         simplify = TRUE)), collapse = "/"),
                                simplify = TRUE )
  
  
  if ( any( test_misc_paths %in% test_objpaths ) )
    testthat::fail( "Unexpected random miscallaneous object path equals a random generated test path" )
  
  
  test_obj_recs <- append( test_obj_recs,
                           replicate( 10, 
                                      cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(), 
                                                                     "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                                                     "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                                     "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                                     "object.path" = sample( test_misc_paths, 1),
                                                                     "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), 
                                                                                                     algo = "sha1", 
                                                                                                     file = FALSE ),
                                                                     "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")), 
                                                                     "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                     "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") ) ), 
                                      simplify = TRUE ) )
  
  
  # - stage records 
  testthat::expect_true( dbstore$commit( test_obj_recs ) )
  
  
  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) != length(test_obj_recs) )
    testthat::fail( "Could not stage expected test records" )
  
  
  
  # - record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon, 
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  # -- test
  #' @cx.tests Return a collection of records for a specified selected list of objects 
  result <- dbstore$records( list( "object.paths" = test_objpaths ) )
  
  
  # -- expected 
  
  expected_objpaths <- test_objpaths
  
  
  expected_recs <- list()
  
  for ( xobj in test_obj_recs ) 
    if ( xobj$getproperties()[["object.path"]] %in% expected_objpaths )
      expected_recs[[ xobj$getproperties()[["id"]] ]] <- xobj
  
  
  # -- assertions
  
  actual_ids <- character(0)
  
  for ( xresult in result ) {
    
    testthat::expect_true( inherits( xresult, "cxaudit_record" ) )
    testthat::expect_equal( attr(class(xresult), "package"), "cxaudit" )
    
    testthat::expect_true( xresult$getproperties()[["id"]] %in% base::names(expected_recs) )
    
    actual_ids <- append( actual_ids, xresult$getproperties()[["id"]] )
  }
  
  testthat::expect_equal( base::sort(actual_ids), base::sort(base::names(expected_recs)) )  
  
  
  
  # - filter attr
  
  actual_filter <- attr(result, "filter") 
  
  testthat::expect_false( is.null(actual_filter) )
  testthat::expect_true( "object.paths" %in% base::names(actual_filter) )
  
  testthat::expect_equal( base::sort(actual_filter[["object.paths"]]), base::sort(expected_objpaths) )  
  
  
})





testthat::test_that( "audit.rdbstore.listFilterObjPathAsObjectParent", {
  
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
  
  
  # - target object paths
  
  test_parentpath <- paste( c( "", replicate( 3, paste( base::sample( c( base::letters, as.character(0:9)), 20, replace = TRUE), collapse = ""), simplify = TRUE )), collapse = "/" )
  
  test_objpaths <- paste( test_parentpath, 
                          replicate( 4, paste( base::sample( c( base::letters, as.character(0:9)), 15, replace = TRUE), collapse = ""), simplify = TRUE ), 
                          sep = "/" )
  
  
  # - test records
  
  #   target records
  test_obj_recs <- lapply( test_objpaths, function(x) {
    
    cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(), 
                                   "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                   "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                   "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                   "object.path" = x,
                                   "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), 
                                                                   algo = "sha1", 
                                                                   file = FALSE ),
                                   "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")), 
                                   "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") ) )
  })
  
  
  
  
  #   noise records  
  
  #   note: noise object path elements are 5 characters longer than test paths to force uniqueness between collections
  #   note: using sampling to add some randomness to the paths
  test_misc_paths <- replicate( 100, 
                                paste( c( "", replicate( sample( 3:10, 1), 
                                                         paste( base::sample( c( base::letters, as.character(0:9)), sample( 10:25, 1), replace = TRUE), collapse = ""),
                                                         simplify = TRUE)), collapse = "/"),
                                simplify = TRUE )
  
  
  if ( any( test_misc_paths %in% test_parentpath ) )
    testthat::fail( "Unexpected random miscallaneous object path equals a random generated test parent path" )
  
  
  test_obj_recs <- append( test_obj_recs,
                           replicate( 10, 
                                      cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(), 
                                                                     "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                                                     "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                                     "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                                     "object.path" = sample( test_misc_paths, 1),
                                                                     "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), 
                                                                                                     algo = "sha1", 
                                                                                                     file = FALSE ),
                                                                     "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")), 
                                                                     "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                     "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") ) ), 
                                      simplify = TRUE ) )
  
  
  # - stage records 
  testthat::expect_true( dbstore$commit( test_obj_recs ) )
  
  
  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) != length(test_obj_recs) )
    testthat::fail( "Could not stage expected test records" )
  
  
  
  # - record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon, 
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  # -- test
  #' @cx.tests Return a collection of records for a specified selected list of objects parent paths 
  result <- dbstore$records( list( "object.parentpaths" = test_parentpath ) )
  
  
  # -- expected 

  expected_parentpath <- test_parentpath  
  
  
  expected_objpaths <- test_objpaths

  expected_recs <- list()
  
  for ( xobj in test_obj_recs ) 
    if ( xobj$getproperties()[["object.path"]] %in% expected_objpaths )
      expected_recs[[ xobj$getproperties()[["id"]] ]] <- xobj
  
  
  # -- assertions
  
  actual_ids <- character(0)
  
  for ( xresult in result ) {
    
    testthat::expect_true( inherits( xresult, "cxaudit_record" ) )
    testthat::expect_equal( attr(class(xresult), "package"), "cxaudit" )
    
    testthat::expect_true( xresult$getproperties()[["id"]] %in% base::names(expected_recs) )
    
    actual_ids <- append( actual_ids, xresult$getproperties()[["id"]] )
  }
  
  testthat::expect_equal( base::sort(actual_ids), base::sort(base::names(expected_recs)) )  
  
  
  
  # - filter attr
  
  actual_filter <- attr(result, "filter") 
  
  testthat::expect_false( is.null(actual_filter) )
  testthat::expect_true( "object.parentpaths" %in% base::names(actual_filter) )
  
  testthat::expect_equal( base::sort(actual_filter[["object.parentpaths"]]), base::sort(expected_parentpath) )  
  
  
})





testthat::test_that( "audit.rdbstore.listFilterNamedObject", {
  
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
  
  
  # - target object paths
  
  test_parentpath <- paste( c( "", replicate( 3, paste( base::sample( c( base::letters, as.character(0:9)), 20, replace = TRUE), collapse = ""), simplify = TRUE )), collapse = "/" )
  
  test_objpaths <- paste( test_parentpath, 
                          replicate( 4, paste( base::sample( c( base::letters, as.character(0:9)), 15, replace = TRUE), collapse = ""), simplify = TRUE ), 
                          sep = "/" )
  
  
  # - test records
  
  #   target records
  test_obj_recs <- lapply( test_objpaths, function(x) {
    
    cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(), 
                                   "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                   "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                   "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                   "object.path" = x,
                                   "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), 
                                                                   algo = "sha1", 
                                                                   file = FALSE ),
                                   "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")), 
                                   "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") ) )
  })
  
  
  
  
  #   noise records  
  
  #   note: noise object path elements are 5 characters longer than test paths to force uniqueness between collections
  #   note: using sampling to add some randomness to the paths
  test_misc_paths <- replicate( 100, 
                                paste( c( "", replicate( sample( 3:10, 1), 
                                                         paste( base::sample( c( base::letters, as.character(0:9)), sample( 10:25, 1), replace = TRUE), collapse = ""),
                                                         simplify = TRUE)), collapse = "/"),
                                simplify = TRUE )
  
  
  if ( any( test_misc_paths %in% test_parentpath ) )
    testthat::fail( "Unexpected random miscallaneous object path equals a random generated test parent path" )
  
  
  test_obj_recs <- append( test_obj_recs,
                           replicate( 10, 
                                      cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(), 
                                                                     "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                                                     "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                                     "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                                     "object.path" = sample( test_misc_paths, 1),
                                                                     "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), 
                                                                                                     algo = "sha1", 
                                                                                                     file = FALSE ),
                                                                     "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")), 
                                                                     "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                     "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") ) ), 
                                      simplify = TRUE ) )
  
  
  # - stage records 
  testthat::expect_true( dbstore$commit( test_obj_recs ) )
  
  
  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) != length(test_obj_recs) )
    testthat::fail( "Could not stage expected test records" )
  
  
  
  # - record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon, 
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  # -- test
  #' @cx.tests Return a collection of records for a specified selected list of objects names
  result <- dbstore$records( list( "object.names" = base::basename(test_objpaths) ) )
  
  
  # -- expected 
  
  expected_parentpath <- test_parentpath  
  
  
  expected_objpaths <- test_objpaths
  
  expected_recs <- list()
  
  for ( xobj in test_obj_recs ) 
    if ( xobj$getproperties()[["object.path"]] %in% expected_objpaths )
      expected_recs[[ xobj$getproperties()[["id"]] ]] <- xobj
  
  
  expected_names <- base::basename( test_objpaths )
  
  
  # -- assertions
  
  actual_ids <- character(0)
  
  for ( xresult in result ) {
    
    testthat::expect_true( inherits( xresult, "cxaudit_record" ) )
    testthat::expect_equal( attr(class(xresult), "package"), "cxaudit" )
    
    testthat::expect_true( xresult$getproperties()[["id"]] %in% base::names(expected_recs) )
    
    actual_ids <- append( actual_ids, xresult$getproperties()[["id"]] )
  }
  
  testthat::expect_equal( base::sort(actual_ids), base::sort(base::names(expected_recs)) )  
  
  
  
  # - filter attr
  
  actual_filter <- attr(result, "filter") 
  
  testthat::expect_false( is.null(actual_filter) )
  testthat::expect_true( "object.names" %in% base::names(actual_filter) )
  
  testthat::expect_equal( base::sort(actual_filter[["object.names"]]), base::sort(expected_names) )  
  
  
})








testthat::test_that( "audit.rdbstore.listFilterNamedObjectInPath", {
  
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
  
  
  # - target object paths
  
  test_parentpath <- paste( c( "", replicate( 3, paste( base::sample( c( base::letters, as.character(0:9)), 20, replace = TRUE), collapse = ""), simplify = TRUE )), collapse = "/" )
  
  test_objpaths <- paste( test_parentpath, 
                          replicate( 4, paste( base::sample( c( base::letters, as.character(0:9)), 15, replace = TRUE), collapse = ""), simplify = TRUE ), 
                          sep = "/" )
  
  
  # - test records
  
  #   target records
  test_obj_recs <- lapply( test_objpaths, function(x) {
    
    cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(), 
                                   "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                   "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                   "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                   "object.path" = x,
                                   "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), 
                                                                   algo = "sha1", 
                                                                   file = FALSE ),
                                   "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")), 
                                   "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") ) )
  })
  
  
  
  
  #   noise records  
  
  #   note: noise object path elements are 5 characters longer than test paths to force uniqueness between collections
  #   note: using sampling to add some randomness to the paths
  test_misc_paths <- replicate( 100, 
                                paste( c( "", replicate( sample( 3:10, 1), 
                                                         paste( base::sample( c( base::letters, as.character(0:9)), sample( 10:25, 1), replace = TRUE), collapse = ""),
                                                         simplify = TRUE)), collapse = "/"),
                                simplify = TRUE )
  
  
  if ( any( test_misc_paths %in% test_parentpath ) )
    testthat::fail( "Unexpected random miscallaneous object path equals a random generated test parent path" )
  
  
  test_obj_recs <- append( test_obj_recs,
                           replicate( 10, 
                                      cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(), 
                                                                     "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                                                     "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                                     "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                                     "object.path" = sample( test_misc_paths, 1),
                                                                     "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), 
                                                                                                     algo = "sha1", 
                                                                                                     file = FALSE ),
                                                                     "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")), 
                                                                     "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                     "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") ) ), 
                                      simplify = TRUE ) )
  
  
  test_obj_recs <- append( test_obj_recs,
                           replicate( 10, 
                                      cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(), 
                                                                     "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                                                     "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                                     "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                                     "object.path" = file.path( base::dirname(sample( test_misc_paths, 1)), base::basename(sample( test_objpaths, 1)), fsep = "/"),
                                                                     "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), 
                                                                                                     algo = "sha1", 
                                                                                                     file = FALSE ),
                                                                     "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")), 
                                                                     "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                     "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") ) ), 
                                      simplify = TRUE ) )
  
  
  # - stage records 
  testthat::expect_true( dbstore$commit( test_obj_recs ) )
  
  
  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) != length(test_obj_recs) )
    testthat::fail( "Could not stage expected test records" )
  
  
  
  # - record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon, 
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  # -- test
  #' @cx.tests Return a collection of records for a specified selected list of named objects in one or parent paths
  result <- dbstore$records( list( "object.parentpaths" = test_parentpath, "object.names" = base::basename(test_objpaths) ) )
  
  
  # -- expected 
  
  expected_parentpath <- test_parentpath  
  
  
  expected_objpaths <- test_objpaths
  
  expected_recs <- list()
  
  for ( xobj in test_obj_recs ) 
    if ( xobj$getproperties()[["object.path"]] %in% expected_objpaths )
      expected_recs[[ xobj$getproperties()[["id"]] ]] <- xobj
  
  
  
  expected_names <- base::basename( test_objpaths )
  
  
  # -- assertions
  
  actual_ids <- character(0)
  
  for ( xresult in result ) {
    
    testthat::expect_true( inherits( xresult, "cxaudit_record" ) )
    testthat::expect_equal( attr(class(xresult), "package"), "cxaudit" )
    
    testthat::expect_true( xresult$getproperties()[["id"]] %in% base::names(expected_recs) )
    
    actual_ids <- append( actual_ids, xresult$getproperties()[["id"]] )
  }
  
  testthat::expect_equal( base::sort(actual_ids), base::sort(base::names(expected_recs)) )  
  
  
  
  # - filter attr
  
  actual_filter <- attr(result, "filter") 
  
  testthat::expect_false( is.null(actual_filter) )
  testthat::expect_true( all( c( "object.parentpaths", "object.names" ) %in% base::names(actual_filter) ) )
  
  testthat::expect_equal( base::sort(actual_filter[["object.parentpaths"]]), base::sort(expected_parentpath) )  
  testthat::expect_equal( base::sort(actual_filter[["object.names"]]), base::sort(expected_names) )  
  
  
})






testthat::test_that( "audit.rdbstore.listFilterObjHash", {
  
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
  
  
  # - target object hash
  test_objhash <- digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), algo = "sha1", file = FALSE )
  
  
  # - test records
  
  #   target records
  
  test_obj_recs <- replicate( 10, 
                              cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(), 
                                                             "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                                             "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                             "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                             "object.path" = paste( c( "", replicate( 5, 
                                                                                                      paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                                                      simplify = TRUE)), collapse = "/"),
                                                             "object.hash" = test_objhash,
                                                             "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")), 
                                                             "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                             "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") ) ), 
                              simplify = TRUE )
  
  

  
  #   noise records  
  
  #   note: noise object class is 5 characters longer than test class to force uniqueness between collections
  test_misc_hashes <- replicate( 100, 
                                 digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), algo = "sha1", file = FALSE ),
                                 simplify = TRUE )
  
  if ( any( test_misc_hashes %in% test_objhash ) )
    testthat::fail( "Unexpected random miscallaneous object hash equals a random generated test hash" )
  
  
  test_obj_recs <- append( test_obj_recs,
                           replicate( 10, 
                                      cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(), 
                                                                     "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                                                     "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                                     "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                                     "object.path" = paste( c( "", replicate( 5, 
                                                                                                              paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                                                              simplify = TRUE)), collapse = "/"),
                                                                     "object.hash" = sample( test_misc_hashes, 1),
                                                                     "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")), 
                                                                     "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                     "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") ) ), 
                                      simplify = TRUE ) )
  
  
  # - stage records 
  testthat::expect_true( dbstore$commit( test_obj_recs ) )
  
  
  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) != length(test_obj_recs) )
    testthat::fail( "Could not stage expected test records" )
  
  
  
  # - record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon, 
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  # -- test
  #' @cx.tests Return a collection of records for a specified selected list of object hashes
  result <- dbstore$records( list( "object.hashes" = test_objhash ) )
  
  
  # -- expected 
  
  expected_objhash <- test_objhash
  
  
  expected_recs <- list()
  
  for ( xobj in test_obj_recs ) 
    if ( xobj$getproperties()[["object.hash"]] %in% expected_objhash )
      expected_recs[[ xobj$getproperties()[["id"]] ]] <- xobj
  
  
  # -- assertions
  
  actual_ids <- character(0)
  
  for ( xresult in result ) {
    
    testthat::expect_true( inherits( xresult, "cxaudit_record" ) )
    testthat::expect_equal( attr(class(xresult), "package"), "cxaudit" )
    
    testthat::expect_true( xresult$getproperties()[["id"]] %in% base::names(expected_recs) )
    
    actual_ids <- append( actual_ids, xresult$getproperties()[["id"]] )
  }
  
  testthat::expect_equal( base::sort(actual_ids), base::sort(base::names(expected_recs)) )  
  
  
  
  # - filter attr
  
  actual_filter <- attr(result, "filter") 
  
  testthat::expect_false( is.null(actual_filter) )
  testthat::expect_true( "object.hashes" %in% base::names(actual_filter) )
  
  testthat::expect_equal( base::sort(actual_filter[["object.hashes"]]), base::sort(expected_objhash) )  
  
  
})






testthat::test_that( "audit.rdbstore.listFilterActors", {
  
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
  
  
  # - target actor
  test_actor <- paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "")
  
  
  # - test records
  
  #   target records
  
  test_obj_recs <- replicate( 30, 
                              cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(), 
                                                             "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                                             "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                             "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                             "object.path" = paste( c( "", replicate( 5, 
                                                                                                      paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                                                      simplify = TRUE)), collapse = "/"),
                                                             "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), algo = "sha1", file = FALSE ),
                                                             "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")), 
                                                             "actor" = test_actor,
                                                             "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") ) ), 
                              simplify = TRUE )
  
  
  
  
  #   noise records  
  
  #   note: noise actor is 20 characters longer than test actor to force uniqueness between collections
  test_misc_actors <- replicate( 100, 
                                 paste( base::sample( c( base::letters, as.character(0:9)), 50, replace = TRUE), collapse = ""),
                                 simplify = TRUE )
  
  if ( any( test_misc_actors %in% test_actor ) )
    testthat::fail( "Unexpected random miscallaneous actors equals a random generated test actor" )
  
    
  test_obj_recs <- append( test_obj_recs,
                           replicate( 100, 
                                      cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(), 
                                                                     "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                                                     "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                                     "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                                     "object.path" = paste( c( "", replicate( 5, 
                                                                                                              paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                                                              simplify = TRUE)), collapse = "/"),
                                                                     "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), algo = "sha1", file = FALSE ),
                                                                     "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")), 
                                                                     "actor" = sample( test_misc_actors, 1),
                                                                     "env" =  paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") ) ), 
                                      simplify = TRUE ) )
  
  
  # - stage records 
  testthat::expect_true( dbstore$commit( test_obj_recs ) )
  
  
  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) != length(test_obj_recs) )
    testthat::fail( "Could not stage expected test records" )
  
  
  
  # - record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon, 
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  # -- test
  #' @cx.tests Return a collection of records for a specified selected actor 
  result <- dbstore$records( list( "actors" = test_actor ) ) 
  
  
  # -- expected 
  
  expected_actor <- test_actor
  
  
  expected_recs <- list()
  
  for ( xobj in test_obj_recs ) 
    if ( xobj$getproperties()[["actor"]] %in% expected_actor )
      expected_recs[[ xobj$getproperties()[["id"]] ]] <- xobj
  
  
  # -- assertions
  
  actual_ids <- character(0)
  
  for ( xresult in result ) {
    
    testthat::expect_true( inherits( xresult, "cxaudit_record" ) )
    testthat::expect_equal( attr(class(xresult), "package"), "cxaudit" )
    
    testthat::expect_true( xresult$getproperties()[["id"]] %in% base::names(expected_recs) )
    
    actual_ids <- append( actual_ids, xresult$getproperties()[["id"]] )
  }
  
  testthat::expect_equal( base::sort(actual_ids), base::sort(base::names(expected_recs)) )  
  
  
  
  # - filter attr
  
  actual_filter <- attr(result, "filter") 
  
  testthat::expect_false( is.null(actual_filter) )
  testthat::expect_true( "actors" %in% base::names(actual_filter) )
  
  testthat::expect_equal( base::sort(actual_filter[["actors"]]), base::sort(expected_actor) )  
  
  
})






testthat::test_that( "audit.rdbstore.listFilterEnv", {
  
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
  
  
  # - target environment
  test_environs <- replicate( 10,
                              paste( base::sample( c( base::letters, as.character(0:9)), 50, replace = TRUE), collapse = ""), 
                              simplify = TRUE )
  
  
  # - test records
  
  #   target records
  
  test_obj_recs <- replicate( 30, 
                              cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(), 
                                                             "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                                             "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                             "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                             "object.path" = paste( c( "", replicate( 5, 
                                                                                                      paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                                                      simplify = TRUE)), collapse = "/"),
                                                             "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), algo = "sha1", file = FALSE ),
                                                             "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")), 
                                                             "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                             "env" = sample( test_environs, 1 ) ) ), 
                              simplify = TRUE )
  
  
  
  
  #   noise records  
  
  #   note: noise environment is 20 characters longer than test environment to force uniqueness between collections
  test_misc_environs <- replicate( 100, 
                                   paste( base::sample( c( base::letters, as.character(0:9)), 70, replace = TRUE), collapse = ""),
                                   simplify = TRUE )
  
  if ( any( test_misc_environs %in% test_environs ) )
    testthat::fail( "Unexpected random miscallaneous environment equals a random generated test environment" )
  
  
  test_obj_recs <- append( test_obj_recs,
                           replicate( 100, 
                                      cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(), 
                                                                     "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                                                     "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                                     "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                                                     "object.path" = paste( c( "", replicate( 5, 
                                                                                                              paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                                                              simplify = TRUE)), collapse = "/"),
                                                                     "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), algo = "sha1", file = FALSE ),
                                                                     "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")), 
                                                                     "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                     "env" =  sample( test_misc_environs, 1) ) ), 
                                      simplify = TRUE ) )
  
  
  # - stage records 
  testthat::expect_true( dbstore$commit( test_obj_recs ) )
  
  
  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) != length(test_obj_recs) )
    testthat::fail( "Could not stage expected test records" )
  
  
  
  # - record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon, 
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  # -- test
  #' @cx.tests Return a collection of records for a specified selected list of environments 
  result <- dbstore$records( list( "envs" = c( utils::head( test_environs, n = 2 ), utils::tail( test_misc_environs, n = 1 ) ) ) )
  
  
  # -- expected 
  
  expected_env <- c( utils::head( test_environs, n = 2 ), utils::tail( test_misc_environs, n = 1 ) )
  
  
  expected_recs <- list()
  
  for ( xobj in test_obj_recs ) 
    if ( xobj$getproperties()[["env"]] %in% expected_env )
      expected_recs[[ xobj$getproperties()[["id"]] ]] <- xobj
  
  
  # -- assertions
  
  actual_ids <- character(0)
  
  for ( xresult in result ) {
    
    testthat::expect_true( inherits( xresult, "cxaudit_record" ) )
    testthat::expect_equal( attr(class(xresult), "package"), "cxaudit" )
    
    testthat::expect_true( xresult$getproperties()[["id"]] %in% base::names(expected_recs) )
    
    actual_ids <- append( actual_ids, xresult$getproperties()[["id"]] )
  }
  
  testthat::expect_equal( base::sort(actual_ids), base::sort(base::names(expected_recs)) )  
  
  
  
  # - filter attr
  
  actual_filter <- attr(result, "filter") 
  
  testthat::expect_false( is.null(actual_filter) )
  testthat::expect_true( "envs" %in% base::names(actual_filter) )
  
  testthat::expect_equal( base::sort(actual_filter[["envs"]]), base::sort(expected_env) )  
  
  
})





testthat::test_that( "audit.rdbstore.listFilterDateRangeFrom", {
  
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
  
  
  # - date range
  #   note: using a 1000 day span for reference
  lst_datetime <- list()
  
  for ( xshift in 1:1000 )
    lst_datetime[[ xshift ]] <- Sys.time() - xshift*24*60*60
  
  
  # - test records
  
  test_obj_recs <- lapply( lst_datetime, function(x) {
    
    cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(),
                                   "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                   "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "object.path" = paste( c( "", replicate( 5,
                                                                            paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                            simplify = TRUE)), collapse = "/"),
                                   "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), algo = "sha1", file = FALSE ),
                                   "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")),
                                   "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                   "datetime" = as.POSIXct( x, tz = "UTC") ) ) 

  } )
  
  

  # - stage records
  testthat::expect_true( dbstore$commit( test_obj_recs ) )


  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) != length(test_obj_recs) )
    testthat::fail( "Could not stage expected test records" )


  # - record clean up
  on.exit( {

    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon,
                      paste( "delete from", xtbl, ";") )

  }, add = TRUE )

  
  # - record date/time
  #   note: using some random cut-off between 10 and 75 days
  test_dtcutoff <- as.POSIXct( Sys.Date() - sample( 10:75, 1 ), tz = "UTC" )
  

  # -- test
  #' @cx.tests Return a collection of records with a date/time after or equal to specified cutoff
  result <- dbstore$records( list( "from" = test_dtcutoff ) )

  
  # -- expected

  # - cutoff
  expected_cutoff <- test_dtcutoff
  
  # - records
  
  expected_recs <- list()

  for ( xobj in test_obj_recs )
    if ( xobj$getproperties()[["datetime"]] >= expected_cutoff )
      expected_recs[[ xobj$getproperties()[["id"]] ]] <- xobj
    

  # -- assertions

  actual_ids <- character(0)

  for ( xresult in result ) {

    testthat::expect_true( inherits( xresult, "cxaudit_record" ) )
    testthat::expect_equal( attr(class(xresult), "package"), "cxaudit" )

    testthat::expect_true( xresult$getproperties()[["id"]] %in% base::names(expected_recs) )

    actual_ids <- append( actual_ids, xresult$getproperties()[["id"]] )
  }

  testthat::expect_equal( base::sort(actual_ids), base::sort(base::names(expected_recs)) )



  # - filter attr

  actual_filter <- attr(result, "filter")

  testthat::expect_false( is.null(actual_filter) )
  

  # make sure limit does not impose test condition
  if ( "limit" %in% base::names(actual_filter) && (actual_filter[["limit"]] < length(expected_recs)) )
    testthat::fail( "The number of expected records exceeds the limit filter" )
  
  testthat::expect_true( "from" %in% base::names(actual_filter) )
  testthat::expect_equal( as.POSIXct( actual_filter[["from"]], tz = "UTC"), expected_cutoff )

})







testthat::test_that( "audit.rdbstore.listFilterDateRangeFromTo", {
  
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
  
  
  # - date range
  #   note: using a 1000 day span for reference
  lst_datetime <- list()
  
  for ( xshift in 1:1000 )
    lst_datetime[[ xshift ]] <- Sys.time() - xshift*24*60*60
  
  
  # - test records
  
  test_obj_recs <- lapply( lst_datetime, function(x) {
    
    cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(),
                                   "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                   "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "object.path" = paste( c( "", replicate( 5,
                                                                            paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                            simplify = TRUE)), collapse = "/"),
                                   "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), algo = "sha1", file = FALSE ),
                                   "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")),
                                   "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                   "datetime" = as.POSIXct( x, tz = "UTC") ) ) 
    
  } )
  
  
  
  # - stage records
  testthat::expect_true( dbstore$commit( test_obj_recs ) )
  
  
  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) != length(test_obj_recs) )
    testthat::fail( "Could not stage expected test records" )
  
  
  # - record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon,
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  # - record date/time
  #   note: sample with replace = FALSE will select two different dates at random
  #   note: sort to ensure the first entry is before the second entry
  test_dtcutoffs <- as.list( base::sort( as.POSIXct( Sys.Date() - sample( 10:75, 2, replace = FALSE ), tz = "UTC" ) ) )
  base::names(test_dtcutoffs) <- c( "from", "to" )
  
  
  # -- test
  #' @cx.tests Return a collection of records with a date/time after or equal to specified cutoff
  #' @cx.tests Return a collection of records with a date/time on or equal to specified cutoff
  result <- dbstore$records( test_dtcutoffs )
  
  
  # -- expected
  
  # - cutoff
  expected_cutoffs <- test_dtcutoffs
  
  # - records
  
  expected_recs <- list()
  
  for ( xobj in test_obj_recs )
    if ( (xobj$getproperties()[["datetime"]] >= expected_cutoffs[["from"]]) && (xobj$getproperties()[["datetime"]] <= expected_cutoffs[["to"]]) )
      expected_recs[[ xobj$getproperties()[["id"]] ]] <- xobj
  
  
  # -- assertions
  
  actual_ids <- character(0)
  
  for ( xresult in result ) {
    
    testthat::expect_true( inherits( xresult, "cxaudit_record" ) )
    testthat::expect_equal( attr(class(xresult), "package"), "cxaudit" )
    
    testthat::expect_true( xresult$getproperties()[["id"]] %in% base::names(expected_recs) )
    
    actual_ids <- append( actual_ids, xresult$getproperties()[["id"]] )
  }
  
  testthat::expect_equal( base::sort(actual_ids), base::sort(base::names(expected_recs)) )
  
  
  
  # - filter attr
  
  actual_filter <- attr(result, "filter")

  testthat::expect_false( is.null(actual_filter) )
  

  # make sure limit does not impose test condition
  if ( "limit" %in% base::names(actual_filter) && (actual_filter[["limit"]] < length(expected_recs)) )
    testthat::fail( "The number of expected records exceeds the limit filter" )
  
  testthat::expect_true( all( c( "from", "to" ) %in% base::names(actual_filter) ) )
  testthat::expect_equal( as.POSIXct( actual_filter[["from"]], tz = "UTC"), expected_cutoffs[["from"]] )
  testthat::expect_equal( as.POSIXct( actual_filter[["to"]], tz = "UTC"), expected_cutoffs[["to"]] )
  
})





testthat::test_that( "audit.rdbstore.listFilterLimit", {
  
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
  
  
  # - date range
  #   note: using a 1000 day span for reference
  lst_datetime <-  trunc.POSIXt( as.POSIXct( sapply( 1:1000, function(x) Sys.time() - x*24*60*60 ) , tz = "UTC" ), units = "secs" )
    
  
  
  # - test records
  
  test_obj_recs <- lapply( lst_datetime, function(x) {
    
    cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(),
                                   "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                   "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "object.path" = paste( c( "", replicate( 5,
                                                                            paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                            simplify = TRUE)), collapse = "/"),
                                   "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), algo = "sha1", file = FALSE ),
                                   "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")),
                                   "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                   "datetime" = as.POSIXct( x, tz = "UTC") ) ) 
    
  } )
  
  
  
  # - stage records
  testthat::expect_true( dbstore$commit( test_obj_recs ) )
  
  
  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) != length(test_obj_recs) )
    testthat::fail( "Could not stage expected test records" )
  
  
  # - record clean up
  on.exit( {

    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon,
                      paste( "delete from", xtbl, ";") )

  }, add = TRUE )



  # - filter options
  #   note: the ends of the date range to eliminate date filtering
  #   note: end of limit range should be less than number of elements in lst_datetime
  test_filter <- list( "from" = utils::head( base::sort(lst_datetime), n = 1),
                       "to" = utils::tail( base::sort(lst_datetime), n = 1),
                       "limit" = sample( 30:500, 1) )


  # test_filter <- list( "from" = utils::head( base::sort(lst_datetime), n = 1),
  #                      "to" = utils::tail( base::sort(lst_datetime), n = 1), 
  #                      "limit" = 10 )
  
    
  # -- test
  #' @cx.tests Limit the number of records when sorted in chronological descending order   
  result <- dbstore$records( test_filter )
  

  # -- expected

  # - expected limit  
  expected_limit <- test_filter[["limit"]]
  
  
  # - expected cutoffs
  expected_cutoffs <- test_filter

  # - records
  
  expected_recs <- utils::head( test_obj_recs, n = expected_limit )
  base::names(expected_recs) <- unlist( lapply( expected_recs, function(x) x$getproperties()[["id"]] ), use.names = FALSE )  


  # -- assertions
  
  testthat::expect_length( result, expected_limit )
  
  
  actual_ids <- unlist( lapply( result, function(x) x$getproperties()[["id"]] ), use.names = FALSE )  
  testthat::expect_equal( base::sort(actual_ids), base::sort(base::names(expected_recs)) )
  
  
  
  # - filter attr
  
  actual_filter <- attr(result, "filter")

  testthat::expect_false( is.null(actual_filter) )


  # make sure limit does not impose test condition
  if ( "limit" %in% base::names(actual_filter) && (actual_filter[["limit"]] < length(expected_recs)) )
    testthat::fail( "The number of expected records exceeds the limit filter" )

  testthat::expect_true( all( c( "from", "to", "limit" ) %in% base::names(actual_filter) ) )
  testthat::expect_equal( as.POSIXct( actual_filter[["from"]], tz = "UTC"), expected_cutoffs[["from"]] )
  testthat::expect_equal( as.POSIXct( actual_filter[["to"]], tz = "UTC"), expected_cutoffs[["to"]] )
  testthat::expect_equal( as.numeric(actual_filter[["limit"]]), expected_limit )
    
})





testthat::test_that( "audit.rdbstore.listFilterLimitInvalid", {
  
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
  
  
  # - date range
  #   note: using a 1000 day span for reference
  lst_datetime <-  trunc.POSIXt( as.POSIXct( sapply( 1:10, function(x) Sys.time() - x*24*60*60 ) , tz = "UTC" ), units = "secs" )
  
  
  
  # - test records
  
  test_obj_recs <- lapply( lst_datetime, function(x) {
    
    cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(),
                                   "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                   "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "object.path" = paste( c( "", replicate( 5,
                                                                            paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                            simplify = TRUE)), collapse = "/"),
                                   "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), algo = "sha1", file = FALSE ),
                                   "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")),
                                   "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                   "datetime" = as.POSIXct( x, tz = "UTC") ) ) 
    
  } )
  
  
  
  # - stage records
  testthat::expect_true( dbstore$commit( test_obj_recs ) )
  
  
  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) != length(test_obj_recs) )
    testthat::fail( "Could not stage expected test records" )
  
  
  # - record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon,
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  
  # - filter options
  #   note: the ends of the date range to eliminate date filtering
  #   note: end of limit range should be less than number of elements in lst_datetime
  test_filter <- list( "from" = utils::head( base::sort(lst_datetime), n = 1),
                       "to" = utils::tail( base::sort(lst_datetime), n = 1),
                       "limit" = paste( sample( base::letters, 20, replace = TRUE ), collapse = "" ) )
  
  

  # -- test
  #' @cx.tests Limit for number of records invalid
  testthat::expect_error( dbstore$records( test_filter ), regexp = "^Record limit or offset not a number$" )
  
  
})






testthat::test_that( "audit.rdbstore.listFilterLimitSelectFirst", {
  
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
  
  
  # - date range
  #   note: using a 1000 day span for reference
  lst_datetime <-  trunc.POSIXt( as.POSIXct( sapply( 1:1000, function(x) Sys.time() - x*24*60*60 ) , tz = "UTC" ), units = "secs" )
  
  
  
  # - test records
  
  test_obj_recs <- lapply( lst_datetime, function(x) {
    
    cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(),
                                   "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                   "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "object.path" = paste( c( "", replicate( 5,
                                                                            paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                            simplify = TRUE)), collapse = "/"),
                                   "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), algo = "sha1", file = FALSE ),
                                   "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")),
                                   "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                   "datetime" = as.POSIXct( x, tz = "UTC") ) ) 
    
  } )
  
  
  
  # - stage records
  testthat::expect_true( dbstore$commit( test_obj_recs ) )
  
  
  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) != length(test_obj_recs) )
    testthat::fail( "Could not stage expected test records" )
  
  
  # - record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon,
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  
  # - filter options
  #   note: the ends of the date range to eliminate date filtering
  #   note: end of limit range should be less than number of elements in lst_datetime
  test_filter <- list( "from" = utils::head( base::sort(lst_datetime), n = 1),
                       "to" = utils::tail( base::sort(lst_datetime), n = 1),
                       "limit" = sample( 30:500, 1), 
                       "select" = "FIRST" )
  

  # -- test
  #' @cx.tests Limit the number of records when sorted in chronological ascending order   
  result <- dbstore$records( test_filter )
  
  
  # -- expected
  
  # - expected limit  
  expected_limit <- test_filter[["limit"]]
  
  # - expected sort selection
  expected_select <- base::tolower(test_filter[["select"]])
  
  # - expected cutoffs
  expected_cutoffs <- test_filter
  
  # - records
  
  expected_recs <- utils::tail( test_obj_recs, n = expected_limit )
  base::names(expected_recs) <- unlist( lapply( expected_recs, function(x) x$getproperties()[["id"]] ), use.names = FALSE )  
  
  
  # -- assertions
  
  testthat::expect_length( result, expected_limit )
  
  
  actual_ids <- unlist( lapply( result, function(x) x$getproperties()[["id"]] ), use.names = FALSE )  
  testthat::expect_equal( base::sort(actual_ids), base::sort(base::names(expected_recs)) )
  
  
  
  # - filter attr
  
  actual_filter <- attr(result, "filter")
  
  testthat::expect_false( is.null(actual_filter) )
  
  
  # make sure limit does not impose test condition
  if ( "limit" %in% base::names(actual_filter) && (actual_filter[["limit"]] < length(expected_recs)) )
    testthat::fail( "The number of expected records exceeds the limit filter" )
  
  testthat::expect_true( all( c( "from", "to", "limit", "select" ) %in% base::names(actual_filter) ) )
  testthat::expect_equal( as.POSIXct( actual_filter[["from"]], tz = "UTC"), expected_cutoffs[["from"]] )
  testthat::expect_equal( as.POSIXct( actual_filter[["to"]], tz = "UTC"), expected_cutoffs[["to"]] )
  testthat::expect_equal( as.numeric(actual_filter[["limit"]]), expected_limit )
  testthat::expect_equal( as.character(actual_filter[["select"]]), expected_select )
  
})





testthat::test_that( "audit.rdbstore.listFilterLimitSelectInvalid", {
  
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
  
  
  # - date range
  #   note: using a 1000 day span for reference
  lst_datetime <-  trunc.POSIXt( as.POSIXct( sapply( 1:10, function(x) Sys.time() - x*24*60*60 ) , tz = "UTC" ), units = "secs" )
  
  
  
  # - test records
  
  test_obj_recs <- lapply( lst_datetime, function(x) {
    
    cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(),
                                   "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                   "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "object.path" = paste( c( "", replicate( 5,
                                                                            paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                            simplify = TRUE)), collapse = "/"),
                                   "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), algo = "sha1", file = FALSE ),
                                   "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")),
                                   "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                   "datetime" = as.POSIXct( x, tz = "UTC") ) ) 
    
  } )
  
  
  
  # - stage records
  testthat::expect_true( dbstore$commit( test_obj_recs ) )
  
  
  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) != length(test_obj_recs) )
    testthat::fail( "Could not stage expected test records" )
  
  
  # - record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon,
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  
  # - filter options
  #   note: the ends of the date range to eliminate date filtering
  #   note: end of limit range should be less than number of elements in lst_datetime
  test_filter <- list( "from" = utils::head( base::sort(lst_datetime), n = 1),
                       "to" = utils::tail( base::sort(lst_datetime), n = 1),
                       "limit" = sample( 30:500, 1), 
                       "select" = paste( sample( c( base::letters, base::LETTERS, as.character(0:9) ), 20, replace = TRUE ), collapse = "" ) )
  
  
  if ( base::tolower(test_filter[["select"]]) %in% c( "first", "last" ) )
    testthat::fail( "Unexpected random select is valid" )
  
  

  
  # -- test
  #' @cx.tests Limit the number of records when sorted in invalid descending order   
  testthat::expect_error( dbstore$records( test_filter ), regexp = "^Record block selection invalid$" )
  

})







testthat::test_that( "audit.rdbstore.listFilterLimitOffset", {
  
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
  
  
  # - date range
  #   note: using a 1000 day span for reference
  lst_datetime <-  trunc.POSIXt( as.POSIXct( sapply( 1:1000, function(x) Sys.time() - x*24*60*60 ) , tz = "UTC" ), units = "secs" )
  
  
  
  # - test records
  
  test_obj_recs <- lapply( lst_datetime, function(x) {
    
    cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(),
                                   "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                   "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "object.path" = paste( c( "", replicate( 5,
                                                                            paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                            simplify = TRUE)), collapse = "/"),
                                   "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), algo = "sha1", file = FALSE ),
                                   "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")),
                                   "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                   "datetime" = as.POSIXct( x, tz = "UTC") ) ) 
    
  } )
  
  
  
  # - stage records
  testthat::expect_true( dbstore$commit( test_obj_recs ) )
  
  
  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) != length(test_obj_recs) )
    testthat::fail( "Could not stage expected test records" )
  
  
  # - record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon,
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  
  # - filter options
  #   note: the ends of the date range to eliminate date filtering
  #   note: end of limit range should be less than number of elements in lst_datetime
  test_filter <- list( "from" = utils::head( base::sort(lst_datetime), n = 1),
                       "to" = utils::tail( base::sort(lst_datetime), n = 1),
                       "limit" = sample( 30:500, 1), 
                       "offset" = sample( 30:50, 1) )
  
  
  # -- test
  #' @cx.tests Offset limited selection of number of records when sorted in chronological descending order   
  result <- dbstore$records( test_filter )
  
  
  # -- expected
  
  # - expected limit  
  expected_limit <- test_filter[["limit"]]
  
  # - expected offset (first record)
  expected_offset <- test_filter[["offset"]]

  # - expected cutoffs
  expected_cutoffs <- test_filter

    
  # - records
  
  expected_recs <- utils::head( utils::tail( test_obj_recs, n = length(test_obj_recs) - expected_offset ), n = expected_limit )
  base::names(expected_recs) <- unlist( lapply( expected_recs, function(x) x$getproperties()[["id"]] ), use.names = FALSE )  
  
  
  # -- assertions
  
  testthat::expect_length( result, expected_limit )
  
  
  actual_ids <- unlist( lapply( result, function(x) x$getproperties()[["id"]] ), use.names = FALSE )  
  testthat::expect_equal( base::sort(actual_ids), base::sort(base::names(expected_recs)) )
  
  
  
  # - filter attr
  
  actual_filter <- attr(result, "filter")
  
  testthat::expect_false( is.null(actual_filter) )
  
  
  # make sure limit does not impose test condition
  if ( "limit" %in% base::names(actual_filter) && (actual_filter[["limit"]] < length(expected_recs)) )
    testthat::fail( "The number of expected records exceeds the limit filter" )
  
  testthat::expect_true( all( c( "from", "to", "limit", "select" ) %in% base::names(actual_filter) ) )
  testthat::expect_equal( as.POSIXct( actual_filter[["from"]], tz = "UTC"), expected_cutoffs[["from"]] )
  testthat::expect_equal( as.POSIXct( actual_filter[["to"]], tz = "UTC"), expected_cutoffs[["to"]] )
  testthat::expect_equal( as.numeric(actual_filter[["limit"]]), expected_limit )
  testthat::expect_equal( as.numeric(actual_filter[["offset"]]), expected_offset )
  
})






testthat::test_that( "audit.rdbstore.listFilterOffsetInvalid", {
  
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
  
  
  # - date range
  #   note: using a 1000 day span for reference
  lst_datetime <-  trunc.POSIXt( as.POSIXct( sapply( 1:10, function(x) Sys.time() - x*24*60*60 ) , tz = "UTC" ), units = "secs" )
  
  
  
  # - test records
  
  test_obj_recs <- lapply( lst_datetime, function(x) {
    
    cxaudit::cxaudit_record( list( "id" = uuid::UUIDgenerate(),
                                   "event" = sample( cxaudit:::.cxaudit_eventnames(), 1),
                                   "object.type" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "object.class" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "object.path" = paste( c( "", replicate( 5,
                                                                            paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                                                            simplify = TRUE)), collapse = "/"),
                                   "object.hash" = digest::digest( paste( base::sample( c( base::letters, as.character(0:9)), 200, replace = TRUE), collapse = ""), algo = "sha1", file = FALSE ),
                                   "label" = base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")),
                                   "actor" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""),
                                   "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                   "datetime" = as.POSIXct( x, tz = "UTC") ) ) 
    
  } )
  
  
  
  # - stage records
  testthat::expect_true( dbstore$commit( test_obj_recs ) )
  
  
  if ( base::nrow( DBI::dbGetQuery( dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) != length(test_obj_recs) )
    testthat::fail( "Could not stage expected test records" )
  
  
  # - record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon,
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  
  # - filter options
  #   note: the ends of the date range to eliminate date filtering
  #   note: end of limit range should be less than number of elements in lst_datetime
  test_filter <- list( "from" = utils::head( base::sort(lst_datetime), n = 1),
                       "to" = utils::tail( base::sort(lst_datetime), n = 1),
                       "offset" = paste( sample( base::letters, 20, replace = TRUE ), collapse = "" ) )
  
  
  
  # -- test
  #' @cx.tests Offset for record selection invalid
  testthat::expect_error( dbstore$records( test_filter ), regexp = "^Record limit or offset not a number$" )
  
  
})

