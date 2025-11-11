#
#
# Tests for audit list
#
# SQLite and DB pool
#


#' @cx.testsfor cxaudit::cxaudit_list()



testthat::test_that( "audit.list.auditEnabledSQLiteDbPoolNoRecordsDefaultCriteria", {
  
  
  # -- stage 
  
  
  # - database connection
  
  test_dbcon <- base::get( "testdbpool", envir = cxaudit_test_env )
  
  if ( base::nrow( DBI::dbGetQuery( test_dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) > 0 )
    testthat::fail( "Collection of records already exists" )
  
  # -- record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( test_dbcon,
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  
  # - test area
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  

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
  
  
  # - inject empty configuration
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  
  test_libs <- file.path( test_root, "libpaths", fsep = "/" )
  
  if ( ! dir.create( test_libs, recursive = TRUE ) )
    testthat::fail( "Could not stage test library path root" )
  
  .libPaths( c( test_libs, .libPaths() ) )
  
  
  if ( ! dir.create( file.path( test_libs, "cxapp", fsep = "/" ), recursive = TRUE ) ||
       inherits( try( base::writeLines( c( "# app properties", 
                                           "AUDIT = enable", 
                                           "AUDIT.STORE = database", 
                                           "AUDIT.DB.VENDOR = sqlite" ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage app properties file" )
  
  
  # - test store
  test_store <- cxaudit::cxaudit_store( test_dbcon )
  
  
  # -- test
  #' @cx.tests List audit records with default filter criteria from SQLite database with no records
  result <- cxaudit::cxaudit_list( store = test_store )

  
  # -- assertions
  testthat::expect_true( base::is.list(result) )
  testthat::expect_length( result, 0 )
  

})





testthat::test_that( "audit.list.auditEnabledSQLiteDbPoolMultiRecordsDefaultCriteria", {
  
  
  # -- stage 
  
  
  # - database connection
  
  test_dbcon <- base::get( "testdbpool", envir = cxaudit_test_env )
  
  if ( base::nrow( DBI::dbGetQuery( test_dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) > 0 )
    testthat::fail( "Collection of records already exists" )
  
  # -- record clean up
  on.exit( {
    
    for ( xtbl in c( "tbl_adt_wrkrecord_attrs", "tbl_adt_wrkrecords", "tbl_adt_commits", "tbl_adt_record_attrs", "tbl_adt_records") )
      DBI::dbExecute( test_dbcon,
                      paste( "delete from", xtbl, ";") )
    
  }, add = TRUE )
  
  
  
  # - test area
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
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
  
  
  # - inject empty configuration
  
  current_libpaths <- .libPaths()
  
  on.exit( {
    .libPaths( current_libpaths )
  }, add = TRUE )
  
  
  test_libs <- file.path( test_root, "libpaths", fsep = "/" )
  
  if ( ! dir.create( test_libs, recursive = TRUE ) )
    testthat::fail( "Could not stage test library path root" )
  
  .libPaths( c( test_libs, .libPaths() ) )
  
  
  if ( ! dir.create( file.path( test_libs, "cxapp", fsep = "/" ), recursive = TRUE ) ||
       inherits( try( base::writeLines( c( "# app properties", 
                                           "AUDIT = enable", 
                                           "AUDIT.STORE = database", 
                                           "AUDIT.DB.VENDOR = sqlite" ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage app properties file" )
  
  
  
  
  # - date range
  #   note: using a 500 second span for reference
  lst_datetime <-  trunc.POSIXt( as.POSIXct( sapply( 1:500, function(x) Sys.time() - x ) , tz = "UTC" ), units = "secs" )
  
  
  # - test records
  #   note: sample any valid event name
  #   note: random reference string for object.type, object.class, object.path, actor and env
  #   note: SHA-1 digest of random string for object.hash
  #   note: random phrase for label with spaces   
  #   note: using datetime in seconds to coerce order for limit boundary selection ... lst_datetime is chronologically descending so limit n should be utils::head( records )
  
  test_rec_objs <- lapply( lst_datetime, function(x) {
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
                                   "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = ""), 
                                   "datetime" = as.POSIXct(x, tz = "UTC ") ) )
    
  })
  
  

  # - test store
  test_store <- cxaudit::cxaudit_store( test_dbcon )
  
  
  # - stage records
  test_stage <- cxaudit::cxaudit_commit( test_rec_objs, store = test_store )
  testthat::expect_true( test_stage )
  
  
  # -- test
  #' @cx.tests List audit records with default filter criteria from SQLite database with number of records greater than default limit
  result <- cxaudit::cxaudit_list( store = test_store )
  
  
  # -- expected
  
  # note: test_rec_objs are chronologically descending
  # note: assuming default limit is 300
  # note: assuming default collating order is last records, i.e. utils::head()
  expected_objs <- utils::head( test_rec_objs, n = 300 )
  base::names(expected_objs) <- lapply(expected_objs, function(x) { x$getproperties()[["id"]] } )
  

  # -- assertions
  
  # - result
  testthat::expect_true( base::is.list(result) )
  testthat::expect_length( result, 300 )
  
  base::names(result) <- lapply( result, function(x) { x$getproperties()[["id"]] } )
  
  testthat::expect_equal( base::sort(base::names(result)), base::sort(base::names(expected_objs)) )
  
  # - result records
  #   note: using property values as surrogate
  for ( xid in base::names(result) ) 
    testthat::expect_equal( result[[ xid ]]$getproperties()[ cxaudit:::.cxaudit_propertynames() ], 
                            expected_objs[[ xid ]]$getproperties()[ cxaudit:::.cxaudit_propertynames() ] )

  # - result filter
  result_filter <- base::attr( result, "filter" )
  
  # note: expecting max limit
  testthat::expect_equal( as.numeric(result_filter[["limit"]]), length(expected_objs) )
  testthat::expect_equal( result_filter[["select"]], "last")

  
})







