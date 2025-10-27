#
#
# Tests for audit commit
#
# SQLite
#


#' @cx.testsfor cxaudit::cxaudit_commit()




testthat::test_that( "audit.commit.auditEnabledSQLiteSingleRecord", {
  
  
  # -- stage 

  
  # - database connection
  
  test_dbcon <- base::get( "testdbcon", envir = cxaudit_test_env )
  
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
  

  # - fail cache
  
  test_failcache <- cxapp::cxapp_standardpath( base::tempfile( pattern = "fail-cache-", tmpdir = test_root, fileext = "") )
  
  if ( ! dir.exists( test_failcache ) && ! dir.create( test_failcache, recursive = TRUE ) )
    testthat::fail("Could not stage test fail cache directory")
  
  
  
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
                                           "AUDIT.DB.VENDOR = sqlite", 
                                           "AUDIT.FAILCACHE = enable", 
                                           paste0( "AUDIT.FAILCACHE.PATH = ", test_failcache ) ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage app properties file" )
  
  
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
                                                 "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") ) )
  
  
  # - fail cache init
  
  test_failcache_obj <- cxaudit::cxaudit_failcache()
  
  testthat::expect_true( test_failcache_obj$isenabled() )
  testthat::expect_true( test_failcache_obj$isempty() )
  
  
  # - test store
  test_store <- cxaudit::cxaudit_store( test_dbcon )
  
  
  # -- test
  #' @cx.tests Commit single record to SQLite database with fail cache enabled
  result <- cxaudit::cxaudit_commit( test_rec_obj, store = test_store )
  
  
  # -- expected 
  expected_objs <- test_rec_obj
  
  
  # -- assertions
  
  # - committed records
  
  act_recs <- test_store$records()
  base::names(act_recs) <- lapply( act_recs, function(x) { x$getproperties()[["id"]] })
  
  testthat::expect_length( act_recs, length(expected_objs) )

  testthat::expect_equal( base::sort(base::names(act_recs[[1]]$getproperties())), base::sort(base::names(expected_objs$getproperties()))  )
  testthat::expect_equal( act_recs[[1]]$getproperties()[ cxaudit:::.cxaudit_propertynames() ], expected_objs$getproperties()[ cxaudit:::.cxaudit_propertynames() ] )

  testthat::expect_length( act_recs[[1]]$getattributes(), length(expected_objs$getattributes()) )  

  
  # - fail cache empty
  # note: no record should go to fail cache
  testthat::expect_true( test_failcache_obj$isempty() )

})





testthat::test_that( "audit.commit.auditEnabledSQLiteMultiRecord", {
  
  
  # -- stage 
  
  
  # - database connection
  
  test_dbcon <- base::get( "testdbcon", envir = cxaudit_test_env )
  
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
  
  
  # - fail cache
  
  test_failcache <- cxapp::cxapp_standardpath( base::tempfile( pattern = "fail-cache-", tmpdir = test_root, fileext = "") )
  
  if ( ! dir.exists( test_failcache ) && ! dir.create( test_failcache, recursive = TRUE ) )
    testthat::fail("Could not stage test fail cache directory")
  
  
  
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
                                           "AUDIT.DB.VENDOR = sqlite", 
                                           "AUDIT.FAILCACHE = enable", 
                                           paste0( "AUDIT.FAILCACHE.PATH = ", test_failcache ) ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage app properties file" )
  
  
  # - clean test record
  #   note: sample any valid event name
  #   note: random reference string for object.type, object.class, object.path, actor and env
  #   note: SHA-1 digest of random string for object.hash
  #   note: random phrase for label with spaces 
  test_rec_objs <- replicate( sample( 5:25, 1), 
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
                                                             "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") ) ),
                              simplify = TRUE)
  
  if ( ! inherits( test_rec_objs, "list") )
    testthat::fail( "Could not stage list of records" )
  
  
  # - fail cache init
  
  test_failcache_obj <- cxaudit::cxaudit_failcache()
  
  testthat::expect_true( test_failcache_obj$isenabled() )
  testthat::expect_true( test_failcache_obj$isempty() )
  
  
  # - test store
  test_store <- cxaudit::cxaudit_store( test_dbcon )
  
  
  # -- test
  #' @cx.tests Commit multiple records to SQLite database with fail cache enabled
  result <- cxaudit::cxaudit_commit( test_rec_objs, store = test_store )
  
  
  # -- expected 
  expected_objs <- test_rec_objs
  base::names(expected_objs) <- lapply( expected_objs, function(x) { x$getproperties()[["id"]] })
  
  # -- assertions
  
  # - committed records
  
  act_recs <- test_store$records()
  base::names(act_recs) <- lapply( act_recs, function(x) { x$getproperties()[["id"]] })

  testthat::expect_length( act_recs, length(expected_objs) )

  testthat::expect_equal( base::sort(base::names(act_recs)), base::sort(base::names(expected_objs)) )
  
  for ( xid in base::names(expected_objs) ) {
    
    testthat::expect_equal( base::sort(base::names(act_recs[[ xid ]]$getproperties())), base::sort(base::names(expected_objs[[ xid ]]$getproperties()))  )
    testthat::expect_equal( act_recs[[ xid ]]$getproperties()[ cxaudit:::.cxaudit_propertynames() ], expected_objs[[ xid ]]$getproperties()[ cxaudit:::.cxaudit_propertynames() ] )
    
    testthat::expect_length( act_recs[[ xid ]]$getattributes(), length(expected_objs[[ xid ]]$getattributes()) )
  }
  
  
  
  # - fail cache empty
  # note: no record should go to fail cache
  testthat::expect_true( test_failcache_obj$isempty() )
  
})






testthat::test_that( "audit.commit.auditEnabledSQLiteMultiRecordFailCacheDisabled", {
  
  
  # -- stage 
  
  
  # - database connection
  
  test_dbcon <- base::get( "testdbcon", envir = cxaudit_test_env )
  
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
                                           "AUDIT.DB.VENDOR = sqlite", 
                                           "AUDIT.FAILCACHE = disable" ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage app properties file" )
  
  
  # - clean test record
  #   note: sample any valid event name
  #   note: random reference string for object.type, object.class, object.path, actor and env
  #   note: SHA-1 digest of random string for object.hash
  #   note: random phrase for label with spaces 
  test_rec_objs <- replicate( sample( 5:25, 1), 
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
                                                             "env" = paste( base::sample( c( base::letters, as.character(0:9)), 30, replace = TRUE), collapse = "") ) ),
                              simplify = TRUE)
  
  if ( ! inherits( test_rec_objs, "list") )
    testthat::fail( "Could not stage list of records" )
  
  
  # - fail cache init
  
  test_failcache_obj <- cxaudit::cxaudit_failcache()
  
  testthat::expect_false( test_failcache_obj$isenabled() )

  
  # - test store
  test_store <- cxaudit::cxaudit_store( test_dbcon )
  
  
  # -- test
  #' @cx.tests Commit multiple records to SQLite database with fail cache disabled
  result <- cxaudit::cxaudit_commit( test_rec_objs, store = test_store )
  
  
  # -- expected 
  expected_objs <- test_rec_objs
  base::names(expected_objs) <- lapply( expected_objs, function(x) { x$getproperties()[["id"]] })
  
  # -- assertions
  
  # - committed records
  
  act_recs <- test_store$records()
  base::names(act_recs) <- lapply( act_recs, function(x) { x$getproperties()[["id"]] })
  
  testthat::expect_length( act_recs, length(expected_objs) )
  
  testthat::expect_equal( base::sort(base::names(act_recs)), base::sort(base::names(expected_objs)) )
  
  for ( xid in base::names(expected_objs) ) {
    
    testthat::expect_equal( base::sort(base::names(act_recs[[ xid ]]$getproperties())), base::sort(base::names(expected_objs[[ xid ]]$getproperties()))  )
    testthat::expect_equal( act_recs[[ xid ]]$getproperties()[ cxaudit:::.cxaudit_propertynames() ], expected_objs[[ xid ]]$getproperties()[ cxaudit:::.cxaudit_propertynames() ] )
    
    testthat::expect_length( act_recs[[ xid ]]$getattributes(), length(expected_objs[[ xid ]]$getattributes()) )
  }
  
  

})

