#
#
# Tests for relational database store
#
# Commit audit records
#

#' @cx.testsfor cxaudit:::.cxaudit_rdbstore()


testthat::test_that( "audit.rdbstore.commitMissingParam", {
  
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
  #' @cx.tests Commit with no records specified results in an error
  testthat::expect_error( dbstore$commit(), regexp = "^Audit records missing or invalid$" )
  
})



testthat::test_that( "audit.rdbstore.commitNull", {
  
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
  #' @cx.tests Commit with records equal to NULL results in an error
  testthat::expect_error( dbstore$commit( NULL ), regexp = "^Audit records missing or invalid$" )
  
})


testthat::test_that( "audit.rdbstore.commitNA", {
  
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
  #' @cx.tests Commit with records equal to NA results in an error
  testthat::expect_error( dbstore$commit( NA ), regexp = "^Audit records missing or invalid$" )
  
})



testthat::test_that( "audit.rdbstore.commitInvalidValue", {
  
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
  #' @cx.tests Commit with records equal to invalid value in error
  testthat::expect_error( dbstore$commit( "lksdjlkjdslkdjf" ), regexp = "^Audit records missing or invalid$" )
  
})



testthat::test_that( "audit.rdbstore.commitListInvalidValue", {
  
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
  
  
  # - clean test record
  
  test_rec_obj <- cxaudit::cxaudit_record( list( "event" = utils::head( cxaudit:::.cxaudit_eventnames(), n = 1),
                                                 "object.type" = "test.item", 
                                                 "object.class" = "test.item", 
                                                 "object.path" = "/some/path/here/to/item",
                                                 "object.hash" = digest::digest( paste( "test.item", "test.item", "item", sep = ":" ), algo = "sha1", file = FALSE ),
                                                 "label" = "Simple test item", 
                                                 "actor" = "a.user",
                                                 "env" = "an.env" )  )
  
  test_recs <- list( test_rec_obj, "lksdlkdjlkjf" )
  
  
  # - store
  dbstore <- cxaudit:::.cxaudit_rdbstore( dbcon )
  
  
  # -- test
  #' @cx.tests Commit of an invalid list item results in error
  testthat::expect_error( dbstore$commit( test_recs ), regexp = "^One or more committed items is not an audit record$" )
  
})





testthat::test_that( "audit.rdbstore.commitIncompleteRecord", {
  
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
  
  # - test records
  test_recs <- cxaudit::cxaudit_record()
  
  
  # - store
  dbstore <- cxaudit:::.cxaudit_rdbstore( dbcon )
  
  
  # -- test
  #' @cx.tests Commit of an invalid list item results in error
  testthat::expect_error( dbstore$commit( test_recs ), regexp = "^Audit record is incomplete and missing required properties$" )
  
})





testthat::test_that( "audit.rdbstore.commitSingleRecordNoAttr", {
  
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
  
  
  
  # - clean test record
  #   note: sample any valid event name
  #   note: random reference string for object.type, object.class, object.path, actor and env
  #   note: SHA-1 digest of random string for object.hash
  #   note: random phrase for label with spaces 
  test_rec_obj <- cxaudit::cxaudit_record( list( "event" = base::sample( cxaudit:::.cxaudit_eventnames(), 1),
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
                                                 "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") )  )
  

  
  # - store
  dbstore <- cxaudit:::.cxaudit_rdbstore( dbcon )
  
  
  # -- test
  #' @cx.tests Commit of single record with no attributes 
  testthat::expect_true( dbstore$commit( test_rec_obj ) )
  
  
  # -- record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon, 
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  
  
  # -- assertions
  
  actual_recs <- DBI::dbGetQuery( dbcon, 
                                  paste( "select cast(uid as varchar(128)) as uid, str_event, str_objtype, str_objclass, str_objname, str_objbin, str_objhash, str_label, str_actor, str_env, dt_date, ts_datetime from tbl_adt_records", 
                                         "where ( uid = ", base::sQuote( test_rec_obj$getproperties()[["id"]], q = FALSE), ");") )

  # - one record returned
  testthat::expect_equal( base::nrow(actual_recs), 1)
  
  # - record values
  testthat::expect_equal( actual_recs[1, "str_event"], test_rec_obj$getproperties()[["event"]] )
  
  testthat::expect_equal( actual_recs[1, "str_objtype"], test_rec_obj$getproperties()[["object.type"]] )
  testthat::expect_equal( actual_recs[1, "str_objclass"], test_rec_obj$getproperties()[["object.class"]] )
  testthat::expect_equal( paste( actual_recs[1, "str_objbin"], actual_recs[1, "str_objname"], sep = "/"), test_rec_obj$getproperties()[["object.path"]] )
  testthat::expect_equal( actual_recs[1, "str_objhash"], test_rec_obj$getproperties()[["object.hash"]] )
  
  testthat::expect_equal( utils::URLdecode(actual_recs[1, "str_label"]), test_rec_obj$getproperties()[["label"]] )
  
  testthat::expect_equal( actual_recs[1, "str_actor"], test_rec_obj$getproperties()[["actor"]] )
  testthat::expect_equal( actual_recs[1, "str_env"], test_rec_obj$getproperties()[["env"]] )
  
  testthat::expect_equal( as.POSIXct( actual_recs[ 1, "dt_date"], tz = "UTC", format = "%Y-%m-%d" ), 
                          as.POSIXct( format( test_rec_obj$getproperties()[["datetime"]], format = "%Y-%m-%d"), tz = "UTC", format = "%Y-%m-%d" ) ) 
  
  testthat::expect_equal( as.POSIXct( actual_recs[1, "ts_datetime"], tz = "UTC", format = "%Y-%m-%d %H:%M:%S" ), test_rec_obj$getproperties()[["datetime"]] )
  

  # - record attributes

  actual_rec_attrs <- DBI::dbGetQuery( dbcon,
                                       paste( "select cast(uid_rec as varchar(128)) as uid_rec from tbl_adt_record_attrs",
                                              "where ( uid_rec = ", base::sQuote( test_rec_obj$getproperties()[["id"]], q = FALSE), ");") )

  testthat::expect_equal( base::nrow(actual_rec_attrs), 0 )


  # - record links
  #' @cx.tests Commit of single record results in a commit link of one
  actual_rec_links <- DBI::dbGetQuery( dbcon,
                                       paste( c( "select cast(uid as varchar(128)), cast(uid_rec as varchar(128)), ts_datetime from tbl_adt_commits",
                                                 "where ( uid in (select uid from tbl_adt_commits where ( uid_rec = ",
                                                 base::sQuote( test_rec_obj$getproperties()[["id"]], q = FALSE),
                                                 ") ) );" ), collapse = " ")  )

  testthat::expect_equal( base::nrow(actual_rec_links), 1 )


  # - work records and attributes

  actual_wrkrecs <- DBI::dbGetQuery( dbcon,
                                     paste( "select cast(uid as varchar(128)) as uid from tbl_adt_wrkrecords",
                                            "where ( uid = ", base::sQuote( test_rec_obj$getproperties()[["id"]], q = FALSE), ");", collapse = " ") )

  testthat::expect_equal( base::nrow(actual_wrkrecs), 0 )


  actual_wrkrec_attrs <- DBI::dbGetQuery( dbcon,
                                          paste( "select cast(uid_rec as varchar(128)) as uid_rec from tbl_adt_wrkrecord_attrs",
                                                 "where ( uid_rec = ", base::sQuote( test_rec_obj$getproperties()[["id"]], q = FALSE), ");", collapse = " ") )

  testthat::expect_equal( base::nrow(actual_wrkrec_attrs), 0 )
  

})



testthat::test_that( "audit.rdbstore.commitSingleRecordWithAttr", {
  
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
  
  
  # - clean test record
  #   note: sample any valid event name
  #   note: random reference string for object.type, object.class, object.path, actor and env
  #   note: SHA-1 digest of random string for object.hash
  #   note: random phrase for label with spaces 
  test_rec_obj <- cxaudit::cxaudit_record( list( "event" = base::sample( cxaudit:::.cxaudit_eventnames(), 1),
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
                                                 "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") )  )
  
  
  # - generate random attributes with random values, names and qualifiers
  
  test_rec_objattr <- replicate( 10, 
                                 base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")),
                                 simplify = TRUE )
  
  test_rec_objattr_names <- replicate( 7, 
                                       paste( base::sample( c( base::letters, as.character(0:9)), 20, replace = TRUE), collapse = ""),
                                       simplify = TRUE )
  
  test_rec_objattr_names <- append( test_rec_objattr_names, 
                                    paste( utils::head( test_rec_objattr_names, n = 3 ), paste( base::sample( c( base::letters, as.character(0:9)), 10, replace = TRUE), collapse = ""), sep = ":" ) )
  
  base::names( test_rec_objattr ) <- test_rec_objattr_names
  
  
  test_rec_obj$setattributes( test_rec_objattr )
  

 
  # - store
  dbstore <- cxaudit:::.cxaudit_rdbstore( dbcon )


  # -- test
  #' @cx.tests Commit of single record with attributes
  testthat::expect_true( dbstore$commit( test_rec_obj ) )


  # -- record clean up
  on.exit( {

    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon,
                      paste( "delete from", xtbl, ";") )

  }, add = TRUE )



  # -- expectations
  
  expected_attrs <- test_rec_objattr 
  

  # -- assertions

  actual_recs <- DBI::dbGetQuery( dbcon,
                                  paste( "select cast(uid as varchar(128)), str_event, str_objtype, str_objclass, str_objname, str_objbin, str_objhash, str_label, str_actor, str_env, dt_date, ts_datetime from tbl_adt_records",
                                         "where ( uid = ", base::sQuote( test_rec_obj$getproperties()[["id"]], q = FALSE), ");") )

  # - one record returned
  testthat::expect_equal( base::nrow(actual_recs), 1)

  # - record values
  testthat::expect_equal( actual_recs[1, "str_event"], test_rec_obj$getproperties()[["event"]] )

  testthat::expect_equal( actual_recs[1, "str_objtype"], test_rec_obj$getproperties()[["object.type"]] )
  testthat::expect_equal( actual_recs[1, "str_objclass"], test_rec_obj$getproperties()[["object.class"]] )
  testthat::expect_equal( paste( actual_recs[1, "str_objbin"], actual_recs[1, "str_objname"], sep = "/"), test_rec_obj$getproperties()[["object.path"]] )
  testthat::expect_equal( actual_recs[1, "str_objhash"], test_rec_obj$getproperties()[["object.hash"]] )

  testthat::expect_equal( utils::URLdecode(actual_recs[1, "str_label"]), test_rec_obj$getproperties()[["label"]] )

  testthat::expect_equal( actual_recs[1, "str_actor"], test_rec_obj$getproperties()[["actor"]] )
  testthat::expect_equal( actual_recs[1, "str_env"], test_rec_obj$getproperties()[["env"]] )

  testthat::expect_equal( as.POSIXct( actual_recs[ 1, "dt_date"], tz = "UTC", format = "%Y-%m-%d" ), 
                          as.POSIXct( format( test_rec_obj$getproperties()[["datetime"]], format = "%Y-%m-%d"), tz = "UTC", format = "%Y-%m-%d" ) ) 
  
  testthat::expect_equal( as.POSIXct( actual_recs[1, "ts_datetime"], tz = "UTC", format = "%Y-%m-%d %H:%M:%S" ), test_rec_obj$getproperties()[["datetime"]] )
  


  # - record attributes
  actual_rec_attrs <- DBI::dbGetQuery( dbcon,
                                       paste( "select cast(uid_rec as varchar(128)) as uid_rec, str_key, str_qual, str_value, int_vseq from tbl_adt_record_attrs",
                                              "where ( uid_rec = ", base::sQuote( test_rec_obj$getproperties()[["id"]], q = FALSE), ");") )

  testthat::expect_equal( base::nrow(actual_rec_attrs), length(expected_attrs) )

  testthat::expect_equal( base::unique(actual_rec_attrs[, "uid_rec"] ), test_rec_obj$getproperties()[["id"]] )
  

  #   attr values  
  
  actual_attrs <- character(0)

  for ( ridx in 1:base::nrow(actual_rec_attrs) ) {
    
    attr_key <- paste( actual_rec_attrs[ ridx, c( "str_key", "str_qual" ) ], collapse = ":" )

    if ( grepl( "^.*:value$", attr_key, ignore.case = TRUE ) )
      attr_key <- gsub( "^(.*):value$", "\\1", attr_key, ignore.case = TRUE )

    actual_attrs[ attr_key ] <- utils::URLdecode( actual_rec_attrs[ ridx, "str_value"] )

  }
  
  testthat::expect_equal( base::sort(base::names(actual_attrs)), base::sort(base::names(expected_attrs)) )
  testthat::expect_equal( actual_attrs[ base::sort(base::names(actual_attrs)) ],  expected_attrs[ base::sort(base::names(expected_attrs)) ] )


  #   value fragment sequence  
  testthat::expect_equal( base::unique(actual_rec_attrs[, "int_vseq"] ), 0 )
  
  
  
  

  # - record links
  #' @cx.tests Commit of single record results in a commit link of one
  actual_rec_links <- DBI::dbGetQuery( dbcon,
                                       paste( c( "select cast(uid as varchar(128)) as uid, cast(uid_rec as varchar(128)) as uid_rec, ts_datetime from tbl_adt_commits",
                                                 "where ( uid in (select uid from tbl_adt_commits where ( uid_rec = ",
                                                 base::sQuote( test_rec_obj$getproperties()[["id"]], q = FALSE),
                                                 ") ) );" ), collapse = " ")  )

  testthat::expect_equal( base::nrow(actual_rec_links), 1 )


  
  
  # - work records and attributes

  actual_wrkrecs <- DBI::dbGetQuery( dbcon,
                                     paste( "select cast(uid as varchar(128)) as uid from tbl_adt_wrkrecords",
                                            "where ( uid = ", base::sQuote( test_rec_obj$getproperties()[["id"]], q = FALSE), ");", collapse = " ") )

  testthat::expect_equal( base::nrow(actual_wrkrecs), 0 )


  actual_wrkrec_attrs <- DBI::dbGetQuery( dbcon,
                                          paste( "select cast(uid_rec as varchar(128)) as uid_rec from tbl_adt_wrkrecord_attrs",
                                                 "where ( uid_rec = ", base::sQuote( test_rec_obj$getproperties()[["id"]], q = FALSE), ");", collapse = " ") )

  testthat::expect_equal( base::nrow(actual_wrkrec_attrs), 0 )
  
  
})








testthat::test_that( "audit.rdbstore.commitMultiRecordWithAttr", {
  
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
  

  # - clean test record
  #   note: sample any valid event name
  #   note: random reference string for object.type, object.class, object.path, actor and env
  #   note: SHA-1 digest of random string for object.hash
  #   note: random phrase for label with spaces 
  test_rec_objs <- replicate( 3, 
                              cxaudit::cxaudit_record( list( "event" = base::sample( cxaudit:::.cxaudit_eventnames(), 1),
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
                                                             "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") )  ), 
                              simplify = TRUE )

  
    
  
  # - generate random attributes with random values, names and qualifiers
  
  test_rec_objattr <- replicate( 10, 
                                 base::trimws(paste( base::sample( c( base::letters, as.character(0:9), rep_len( " ", 5)), 30, replace = TRUE), collapse = "")),
                                 simplify = TRUE )
  
  test_rec_objattr_names <- replicate( 7, 
                                       paste( base::sample( c( base::letters, as.character(0:9)), 20, replace = TRUE), collapse = ""),
                                       simplify = TRUE )
  
  test_rec_objattr_names <- append( test_rec_objattr_names, 
                                    paste( utils::head( test_rec_objattr_names, n = 3 ), paste( base::sample( c( base::letters, as.character(0:9)), 10, replace = TRUE), collapse = ""), sep = ":" ) )
  
  base::names( test_rec_objattr ) <- test_rec_objattr_names
  
  
  #   add attributes to first object
  test_rec_objs[[ 1 ]]$setattributes( test_rec_objattr ) 
  
  
  
  # - store
  dbstore <- cxaudit:::.cxaudit_rdbstore( dbcon )
  
  
  # -- test
  #' @cx.tests Commit of multiple record with attributes
  testthat::expect_true( dbstore$commit( test_rec_objs ) )
  
  
  # -- record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( dbcon,
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  
  
  # -- expectations

  # - expected objects
  
  expected_objs <- test_rec_objs

  expected_obj_ids <- character(0)
  
    for ( xobj in test_rec_objs ) 
      expected_obj_ids <- append( expected_obj_ids, xobj$getproperties()[["id"]] )
  
  base::names( expected_objs ) <- expected_obj_ids


  # - expected attr
  expected_attrs <- test_rec_objattr 
  
  
  # -- assertions
  
  actual_recs <- DBI::dbGetQuery( dbcon,
                                  paste( "select cast(uid as varchar(128)) as uid, str_event, str_objtype, str_objclass, str_objname, str_objbin, str_objhash, str_label, str_actor, str_env, dt_date, ts_datetime from tbl_adt_records",
                                         "where ( uid in (", paste( base::sQuote( expected_obj_ids, q = FALSE), collapse = ", "), ") );") )
  
  # - number of records returned
  testthat::expect_equal( base::nrow(actual_recs), length(expected_objs) )

  
  # - record values
  for ( xrow in 1:base::nrow(actual_recs) ) {
    
    xrec <- actual_recs[ xrow, ]

    xobj <- expected_objs[[ xrec[, "uid" ] ]]

    
    # - record values

    testthat::expect_equal( xrec[, "str_event"], xobj$getproperties()[["event"]] )
     
    testthat::expect_equal( xrec[, "str_objtype"], xobj$getproperties()[["object.type"]] )
    testthat::expect_equal( xrec[, "str_objclass"], xobj$getproperties()[["object.class"]] )
    testthat::expect_equal( paste( xrec[, "str_objbin"], xrec[, "str_objname"], sep = "/"), xobj$getproperties()[["object.path"]] )
    testthat::expect_equal( xrec[, "str_objhash"], xobj$getproperties()[["object.hash"]] )
     
    testthat::expect_equal( utils::URLdecode(xrec[, "str_label"]), xobj$getproperties()[["label"]] )
     
    testthat::expect_equal( xrec[, "str_actor"], xobj$getproperties()[["actor"]] )
    testthat::expect_equal( xrec[, "str_env"], xobj$getproperties()[["env"]] )
     
    
    testthat::expect_equal( as.POSIXct( xrec[, "dt_date"], tz = "UTC", format = "%Y-%m-%d" ), 
                            as.POSIXct( format( xobj$getproperties()[["datetime"]], format = "%Y-%m-%d"), tz = "UTC", format = "%Y-%m-%d" ) ) 
    
    testthat::expect_equal( as.POSIXct( xrec[, "ts_datetime"], tz = "UTC", format = "%Y-%m-%d %H:%M:%S" ), xobj$getproperties()[["datetime"]] )

  }  # end of for-loop across actual records
  
  

  # - record attributes
  
  actual_rec_attrs <- DBI::dbGetQuery( dbcon,
                                       paste( "select cast(uid_rec as varchar(128)) as uid_rec, str_key, str_qual, str_value, int_vseq from tbl_adt_record_attrs",
                                              "where ( uid_rec in (", paste( base::sQuote( expected_obj_ids, q = FALSE), collapse = ", "), ") );") )
  
  #   number of attribute record 
  #'  @cx.tests Only attributes assigned to a committed record is stored
  testthat::expect_equal( base::nrow(actual_rec_attrs), length(expected_attrs) )
  testthat::expect_equal( base::unique( actual_rec_attrs[, "uid_rec"] ), utils::head( expected_obj_ids, n = 1) )

  #   attribute values
  actual_attrs <- character(0)
  
  for ( ridx in 1:base::nrow(actual_rec_attrs) ) {
    
    attr_key <- paste( actual_rec_attrs[ ridx, c( "str_key", "str_qual" ) ], collapse = ":" )
    
    if ( grepl( "^.*:value$", attr_key, ignore.case = TRUE ) )
      attr_key <- gsub( "^(.*):value$", "\\1", attr_key, ignore.case = TRUE )
    
    actual_attrs[ attr_key ] <- utils::URLdecode( actual_rec_attrs[ ridx, "str_value"] )
    
  }
  
  testthat::expect_equal( base::sort(base::names(actual_attrs)), base::sort(base::names(expected_attrs)) )
  testthat::expect_equal( actual_attrs[ base::sort(base::names(actual_attrs)) ],  expected_attrs[ base::sort(base::names(expected_attrs)) ] )

  #   value fragment sequence  
  testthat::expect_equal( base::unique(actual_rec_attrs[, "int_vseq"] ), 0 )


  # - record links
  #' @cx.tests Commit of multiple records result in commit links identifying all committed records
  actual_rec_links <- DBI::dbGetQuery( dbcon,
                                       paste( c( "select cast(uid as varchar(128)) as uid, cast(uid_rec as varchar(128)) as uid_rec, ts_datetime from tbl_adt_commits",
                                                 "where ( uid in (select uid from tbl_adt_commits where ( uid_rec = ", base::sQuote( utils::head( expected_obj_ids, n = 1), q = FALSE), ") ) );" ), collapse = " ") )
  
  testthat::expect_equal( base::nrow(actual_rec_links), length(expected_objs) )
  testthat::expect_equal( base::sort(as.character(actual_rec_links[, "uid_rec"])), base::sort(expected_obj_ids) )
  

  
  # - work records and attributes
  
  actual_wrkrecs <- DBI::dbGetQuery( dbcon,
                                     paste( "select cast(uid as varchar(128)) as uid from tbl_adt_wrkrecords",
                                            "where ( uid in (", paste( base::sQuote( expected_obj_ids, q = FALSE), collapse = ", "), ") );") )

  testthat::expect_equal( base::nrow(actual_wrkrecs), 0 )


  actual_wrkrec_attrs <- DBI::dbGetQuery( dbcon,
                                          paste( "select cast(uid_rec as varchar(128)) as uid_rec from tbl_adt_wrkrecord_attrs",
                                                 "where ( uid_rec in (", paste( base::sQuote( expected_obj_ids, q = FALSE), collapse = ", "), ") );") )

  testthat::expect_equal( base::nrow(actual_wrkrec_attrs), 0 )
  
})


