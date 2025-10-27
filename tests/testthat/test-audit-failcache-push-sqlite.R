#
#
# Tests for audit record fail cache store
#
# Push
#

#' @cx.testsfor cxaudit:::cxaudit_failcache()


#
#  Note: Tests for now only work with SQLite since that is a simple dependency
#


testthat::test_that( "audit.failcache.pushCachedNotEnabled", {
  
  
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
  
  
  # - test cache path
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-failcache-", tmpdir = test_root, fileext = "") )
  
  if ( ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail( "Fail cache directory path could not be staged" )
  
  
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
  
  
  # - add records to the fail cache
  
  # note: important that AUDIT.FAILCACHE equals disable
  
  if ( ! dir.create( file.path( test_libs, "cxapp", fsep = "/" ), recursive = TRUE ) ||
       inherits( try( base::writeLines( c( "# app properties", 
                                           "AUDIT = enabled", 
                                           "AUDIT.STORE = database",
                                           "AUDIT.DB.VENDOR = sqlite",
                                           "AUDIT.FAILCACHE = enable",
                                           paste0( "AUDIT.FAILCACHE.PATH = ", test_cachepath ) ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage empty app properties file" )
  

  # - test cache
  test_initcache <- cxaudit::cxaudit_failcache()


  
  # - stage test records
  #   note: sample any valid event name
  #   note: random reference string for object.type, object.class, object.path, actor and env
  #   note: SHA-1 digest of random string for object.hash
  #   note: random phrase for label with spaces 
  test_rec_objs <- replicate( sample( 5:50, 1), 
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

  # attributes
  
  test_obj_attrs <- replicate( length(test_rec_objs),
                               sapply( replicate( sample( 0:10, 1), paste( sample( base::letters, sample( 5:20, 1), replace = TRUE ), collapse = "" ), simplify = TRUE),
                                       function (x) {
                                         base::trimws(paste( sample( c( base::letters, base::LETTERS, as.character(0:9), rep_len( " ", 3) ), sample( 10:60, 1), replace = TRUE ), collapse = "" ))
                                       },
                                       USE.NAMES = TRUE ),
                               simplify = TRUE )
  
  
  for ( xidx in 1:length(test_rec_objs) )
    if ( length( test_obj_attrs[[xidx]]) > 0 )
      test_rec_objs[[xidx]]$setattributes( test_obj_attrs[[xidx]] )

    
  # - stage objects in cache
  testthat::expect_true( test_initcache$cache( test_rec_objs ) )
  testthat::expect_false( test_initcache$isempty() )  
  
  if ( file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) &&
       ! file.remove( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not remove init cache configuration" )
  
  
  # - configure disabled fail cache

    
  # reset cached configuration
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    base::rm( list = ".cxapp.wrkcache.config", envir = .GlobalEnv )
  
  if ( exists( ".cxapp.wrkcache.config", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash app config" )  
  

  if ( inherits( try( base::writeLines( c( "# app properties", 
                                           "AUDIT = enabled", 
                                           "AUDIT.STORE = database",
                                           "AUDIT.DB.VENDOR = sqlite",
                                           "AUDIT.FAILCACHE = disable",
                                           paste0( "AUDIT.FAILCACHE.PATH = ", test_cachepath ) ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage empty app properties file" )

  
  test_obj <- cxaudit::cxaudit_failcache()  
  

  
  # -- test
  #' @cx.tests Push fail cache with fail cache disabled results in error
  testthat::expect_error( test_obj$push( store = cxaudit::cxaudit_store( test_dbcon ) ), regexp = "^The audit record fail cache is not enabled$" )
  
  
  # -- assertions
  
  # - no records committed
  testthat::expect_equal( base::nrow( DBI::dbGetQuery( test_dbcon, "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ), 0 )  
  
})




testthat::test_that( "audit.failcache.pushNoCachedRecords", {
  
  
  # -- stage 
  
  
  # - database connection
  
  test_dbcon <- base::get( "testdbcon", envir = cxaudit_test_env )
  
  if ( base::nrow( DBI::dbGetQuery( test_dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) > 0 )
    testthat::fail( "Collection of records already exists" )

  # - record clean up
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
  
  
  # - test cache path
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-failcache-", tmpdir = test_root, fileext = "") )
  
  if ( ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail( "Fail cache directory path could not be staged" )
  
  
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
                                           "AUDIT.FAILCACHE = enabled",
                                           paste0( "AUDIT.FAILCACHE.PATH = ", test_cachepath ) ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage empty app properties file" )
  
  
  # - test cache
  test_obj <- cxaudit::cxaudit_failcache()
  
  
  # -- test
  #' @cx.tests Push empty fail cache results in TRUE
  result <- test_obj$push( store = cxaudit::cxaudit_store( test_dbcon ) )
  
  
  # -- assertions
  
  # - response with no cached records
  testthat::expect_true( result )

  
  # - no records committed
  testthat::expect_equal( base::nrow( DBI::dbGetQuery( test_dbcon, "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ), 0 )  
  
})






testthat::test_that( "audit.failcache.pushCachedRecords", {
  
  
  # -- stage 
  
  
  # - database connection
  
  test_dbcon <- base::get( "testdbcon", envir = cxaudit_test_env )
  
  if ( base::nrow( DBI::dbGetQuery( test_dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) > 0 )
    testthat::fail( "Collection of records already exists" )

  
  # - record clean up
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
  
  
  # - test cache path
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-failcache-", tmpdir = test_root, fileext = "") )
  
  if ( ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail( "Fail cache directory path could not be staged" )
  
  
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
                                           "AUDIT.FAILCACHE = enabled",
                                           paste0( "AUDIT.FAILCACHE.PATH = ", test_cachepath ) ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage empty app properties file" )
  
  
  # - clean test records
  #   note: sample any valid event name
  #   note: random reference string for object.type, object.class, object.path, actor and env
  #   note: SHA-1 digest of random string for object.hash
  #   note: random phrase for label with spaces 
  
  test_events <- cxaudit:::.cxaudit_eventnames()[ ! cxaudit:::.cxaudit_eventnames() %in% "import" ]
  
  test_rec_objs <- replicate( sample( 5:25, 1), 
                              cxaudit::cxaudit_record( list( "event" = base::sample( test_events, 1),
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
  
  
  
  
  # attributes
  
  test_obj_attrs <- replicate( length(test_rec_objs),
                               sapply( replicate( sample( 0:10, 1), paste( sample( base::letters, sample( 5:20, 1), replace = TRUE ), collapse = "" ), simplify = TRUE),
                                       function (x) {
                                         base::trimws(paste( sample( c( base::letters, base::LETTERS, as.character(0:9), rep_len( " ", 3) ), sample( 10:60, 1), replace = TRUE ), collapse = "" ))
                                       },
                                       USE.NAMES = TRUE ),
                               simplify = TRUE )
  
  
  for ( xidx in 1:length(test_rec_objs) )
    if ( length( test_obj_attrs[[xidx]]) > 0 )
      test_rec_objs[[xidx]]$setattributes( test_obj_attrs[[xidx]] )
  
  
  
  # - test cache
  test_obj <- cxaudit::cxaudit_failcache()
  
  
  # - stage objects in cache
  testthat::expect_true( test_obj$cache( test_rec_objs ) )
  testthat::expect_false( test_obj$isempty() )
  

  # -- test
  #' @cx.tests Push multiple cached objects to audit trail store
  result <- test_obj$push( store = cxaudit::cxaudit_store( test_dbcon ) )

  
  # -- expected
  
  expected_recs <- test_rec_objs
  base::names(expected_recs) <- unlist( lapply( expected_recs, function(x) { x$getproperties()[["id"]] } ), use.names = FALSE ) 
  
  
  expected_import_props <- list( "event" = "import", 
                                 "object.type" = "audit.record",
                                 "object.class" = "audit.record", 
                                 "object.path" = base::tolower(test_cachepath),
                                 "label" = "Import of audit fail cache records", 
                                 "actor" = as.character(Sys.info()[["user"]]) )
  
  expected_import_attr <- c( "record.count" = as.character(length(expected_recs)) )
  
  
  # -- assertions
  
  testthat::expect_true( result )
  
  
  # - committed records

  actual_recs <- cxaudit:::.cxaudit_rdbstore( test_dbcon )$records()
  base::names(actual_recs) <- unlist( lapply( actual_recs, function(x) { x$getproperties()[["id"]] } ), use.names = FALSE )

  # note: adding import record on top of expected records
  testthat::expect_length( actual_recs, length(expected_recs) + 1 )
  
  
  # - cached records
  #   note: retrieve one record at a time to verify attributes
  
  test_rdbstore <- cxaudit:::.cxaudit_rdbstore( test_dbcon )
  

  for ( xitem in base::names(expected_recs) ) {
    
    # get object from store
    act_obj <- test_rdbstore$get( xitem )
    

    # properties
    exp_props <- expected_recs[[xitem]]$getproperties()
    act_props <- act_obj$getproperties()
    
    testthat::expect_equal( act_props[ base::sort(cxaudit:::.cxaudit_propertynames()) ], exp_props[ base::sort(cxaudit:::.cxaudit_propertynames()) ] )
    
    
    # attributes
    exp_attr <- expected_recs[[xitem]]$getattributes()
    act_attr <- act_obj$getattributes()
    
    testthat::expect_equal( act_attr[ base::sort(base::names(act_attr))] , exp_attr[ base::sort(base::names(exp_attr)) ] )
    
    
    base::rm( list = c( "act_obj", "act_props", "exp_props", "exp_attr", "act_attr") )
  }
  
  
  # - import record
  
  actual_import_recid <- base::names(actual_recs)[ ! base::names(actual_recs) %in% base::names(expected_recs)]
  testthat::expect_length( actual_import_recid, 1 )

  # get complete import object  
  imprt_obj <- test_rdbstore$get( actual_import_recid )
  
  # assess known properties
  # note: need to expand this with environment reference
  for ( xprop in base::names(expected_import_props) )
    testthat::expect_equal( imprt_obj$getproperties()[[ xprop ]], expected_import_props[[ xprop ]] )

  # assess attributes
  # note: need to expand this with configuration properties
  for ( xattr in base::names(expected_import_attr) )
    testthat::expect_equal( imprt_obj$getattributes()[[ xattr ]], expected_import_attr[[ xattr ]] )
  
  # assess links
  imprt_objlinks <- imprt_obj$getlinks()
  base::names(imprt_objlinks) <- unlist( lapply( imprt_objlinks, function(x) { x$getproperties()[["id"]] } ), use.names = FALSE )
  
  testthat::expect_equal( base::sort(base::names(imprt_objlinks)), base::sort(base::names(expected_recs)) )
  

  # - clean cache path
  testthat::expect_length( list.files( test_cachepath, recursive = TRUE ), 0 )
  
  testthat::expect_length( list.dirs( test_cachepath, recursive = TRUE), 1 )
  testthat::expect_equal( list.dirs( test_cachepath, recursive = TRUE), test_cachepath )
  
})






testthat::test_that( "audit.failcache.pushCachedRecordsWithLckBlock", {
  
  
  # -- stage 
  
  
  # - database connection
  
  test_dbcon <- base::get( "testdbcon", envir = cxaudit_test_env )
  
  if ( base::nrow( DBI::dbGetQuery( test_dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) > 0 )
    testthat::fail( "Collection of records already exists" )
  
  
  # - record clean up
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
  
  
  # - test cache path
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-failcache-", tmpdir = test_root, fileext = "") )
  
  if ( ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail( "Fail cache directory path could not be staged" )
  
  
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
                                           "AUDIT.FAILCACHE = enabled",
                                           paste0( "AUDIT.FAILCACHE.PATH = ", test_cachepath ) ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage empty app properties file" )
  
  
  # - clean test records
  #   note: sample any valid event name
  #   note: random reference string for object.type, object.class, object.path, actor and env
  #   note: SHA-1 digest of random string for object.hash
  #   note: random phrase for label with spaces 
  
  test_events <- cxaudit:::.cxaudit_eventnames()[ ! cxaudit:::.cxaudit_eventnames() %in% "import" ]
  
  test_rec_objs <- replicate( sample( 5:25, 1), 
                              cxaudit::cxaudit_record( list( "event" = base::sample( test_events, 1),
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
  
  
  
  
  # attributes
  
  test_obj_attrs <- replicate( length(test_rec_objs),
                               sapply( replicate( sample( 0:10, 1), paste( sample( base::letters, sample( 5:20, 1), replace = TRUE ), collapse = "" ), simplify = TRUE),
                                       function (x) {
                                         base::trimws(paste( sample( c( base::letters, base::LETTERS, as.character(0:9), rep_len( " ", 3) ), sample( 10:60, 1), replace = TRUE ), collapse = "" ))
                                       },
                                       USE.NAMES = TRUE ),
                               simplify = TRUE )
  
  
  for ( xidx in 1:length(test_rec_objs) )
    if ( length( test_obj_attrs[[xidx]]) > 0 )
      test_rec_objs[[xidx]]$setattributes( test_obj_attrs[[xidx]] )
  
  
  
  # - locked test records

  test_rec_obj_lcks <- replicate( sample( 5:15, 1), 
                                  cxaudit::cxaudit_record( list( "event" = base::sample( test_events, 1),
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
  
  
  
  
  
  # - test cache
  test_obj <- cxaudit::cxaudit_failcache()
  
  
  # - stage objects in cache
  testthat::expect_true( test_obj$cache( test_rec_objs ) )
  testthat::expect_false( test_obj$isempty() )

  
  # - stage locked objects
  testthat::expect_true( test_obj$cache( test_rec_obj_lcks ) )
  
  test_lck_refid <- test_rec_obj_lcks[[1]]$getproperties()[["id"]]
  test_lck_lstfiles <- list.files( test_cachepath, full.names = TRUE, recursive = TRUE, include.dirs = FALSE )

  test_lck_path <- base::unique(base::dirname( test_lck_lstfiles[ grepl( paste0( ".*/", test_lck_refid, "\\.json$" ), test_lck_lstfiles, perl = TRUE ) ] )) 
  testthat::expect_length( test_lck_path, 1 )

  # inject lck file  
  base::writeLines( "test lock", con = file.path( test_lck_path, "failcache.lck", fsep = "/" ) )
  testthat::expect_true( file.exists(file.path( test_lck_path, "failcache.lck", fsep = "/" )) )
  


  # -- test
  #' @cx.tests Push multiple cached objects to audit trail store ignoring locked cache entries
  result <- test_obj$push( store = cxaudit::cxaudit_store( test_dbcon ) )
  
  


  # -- expected

  expected_recs <- test_rec_objs
  base::names(expected_recs) <- unlist( lapply( expected_recs, function(x) { x$getproperties()[["id"]] } ), use.names = FALSE )


  expected_import_props <- list( "event" = "import",
                                 "object.type" = "audit.record",
                                 "object.class" = "audit.record",
                                 "object.path" = base::tolower(test_cachepath),
                                 "label" = "Import of audit fail cache records",
                                 "actor" = as.character(Sys.info()[["user"]]) )

  expected_import_attr <- c( "record.count" = as.character(length(expected_recs)) )

  
  expected_lck_recs <- test_rec_obj_lcks
  base::names(expected_lck_recs) <- unlist( lapply( expected_lck_recs, function(x) { x$getproperties()[["id"]] } ), use.names = FALSE )
  

  # -- assertions

  testthat::expect_true( result )


  # - committed records

  actual_recs <- cxaudit:::.cxaudit_rdbstore( test_dbcon )$records()
  base::names(actual_recs) <- unlist( lapply( actual_recs, function(x) { x$getproperties()[["id"]] } ), use.names = FALSE )

  # note: adding import record on top of expected records
  testthat::expect_length( actual_recs, length(expected_recs) + 1 )


  # - cached records
  #   note: retrieve one record at a time to verify attributes

  test_rdbstore <- cxaudit:::.cxaudit_rdbstore( test_dbcon )


  for ( xitem in base::names(expected_recs) ) {

    # get object from store
    act_obj <- test_rdbstore$get( xitem )


    # properties
    exp_props <- expected_recs[[xitem]]$getproperties()
    act_props <- act_obj$getproperties()

    testthat::expect_equal( act_props[ base::sort(cxaudit:::.cxaudit_propertynames()) ], exp_props[ base::sort(cxaudit:::.cxaudit_propertynames()) ] )


    # attributes
    exp_attr <- expected_recs[[xitem]]$getattributes()
    act_attr <- act_obj$getattributes()

    testthat::expect_equal( act_attr[ base::sort(base::names(act_attr))] , exp_attr[ base::sort(base::names(exp_attr)) ] )


    base::rm( list = c( "act_obj", "act_props", "exp_props", "exp_attr", "act_attr") )
  }


  # - import record

  actual_import_recid <- base::names(actual_recs)[ ! base::names(actual_recs) %in% base::names(expected_recs)]
  testthat::expect_length( actual_import_recid, 1 )

  # get complete import object
  imprt_obj <- test_rdbstore$get( actual_import_recid )

  # assess known properties
  # note: need to expand this with environment reference
  for ( xprop in base::names(expected_import_props) )
    testthat::expect_equal( imprt_obj$getproperties()[[ xprop ]], expected_import_props[[ xprop ]] )

  # assess attributes
  # note: need to expand this with configuration properties
  for ( xattr in base::names(expected_import_attr) )
    testthat::expect_equal( imprt_obj$getattributes()[[ xattr ]], expected_import_attr[[ xattr ]] )

  # assess links
  imprt_objlinks <- imprt_obj$getlinks()
  base::names(imprt_objlinks) <- unlist( lapply( imprt_objlinks, function(x) { x$getproperties()[["id"]] } ), use.names = FALSE )

  testthat::expect_equal( base::sort(base::names(imprt_objlinks)), base::sort(base::names(expected_recs)) )


  # - remnant of cache path
  testthat::expect_length( list.files( test_cachepath, recursive = TRUE ), length(test_rec_obj_lcks) + 1 )

  testthat::expect_length( list.files( test_cachepath, pattern = "\\.lck", recursive = TRUE, include.dirs = FALSE ), 1 )
  
  act_lck_refids <- gsub( ".*/(.*)\\.json$", "\\1", list.files( test_cachepath, pattern = "\\.json", recursive = TRUE, include.dirs = FALSE ), perl = TRUE ) 
  
  testthat::expect_equal( base::sort(act_lck_refids), base::sort(base::names(expected_lck_recs)) )


})





testthat::test_that( "audit.failcache.pushCachedRecordsMultiBlock", {
  
  
  # -- stage 
  
  
  # - database connection
  
  test_dbcon <- base::get( "testdbcon", envir = cxaudit_test_env )
  
  if ( base::nrow( DBI::dbGetQuery( test_dbcon, paste( "select cast( uid as varchar(128) ) as uid from tbl_adt_records ;" ) ) ) > 0 )
    testthat::fail( "Collection of records already exists" )
  
  
  # - record clean up
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
  
  
  # - test cache path
  test_cachepath <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-failcache-", tmpdir = test_root, fileext = "") )
  
  if ( ! dir.create( test_cachepath, recursive = TRUE ) )
    testthat::fail( "Fail cache directory path could not be staged" )
  
  
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
                                           "AUDIT.FAILCACHE = enabled",
                                           paste0( "AUDIT.FAILCACHE.PATH = ", test_cachepath ) ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage empty app properties file" )
  
  
  # - clean test records
  #   note: simplified record with no attributes 
  #   note: sample any valid event name
  #   note: random reference string for object.type, object.class, object.path, actor and env
  #   note: SHA-1 digest of random string for object.hash
  #   note: random phrase for label with spaces 
  
  test_events <- cxaudit:::.cxaudit_eventnames()[ ! cxaudit:::.cxaudit_eventnames() %in% "import" ]
  
  test_rec_objs <-  replicate( 3, 
                               replicate( sample( 5:10, 1), 
                                          cxaudit::cxaudit_record( list( "event" = base::sample( test_events, 1),
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
                                          simplify = TRUE ),
                               simplify = TRUE )
  


  # - locked test records

  test_rec_obj_lcks <- replicate( sample( 5:15, 1),
                                  cxaudit::cxaudit_record( list( "event" = base::sample( test_events, 1),
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





  # - test cache
  test_obj <- cxaudit::cxaudit_failcache()


  # - stage object blocks in cache
  for ( xblock in test_rec_objs ) 
    testthat::expect_true( test_obj$cache( xblock ) )
  
  testthat::expect_false( test_obj$isempty() )


  # - stage locked objects
  testthat::expect_true( test_obj$cache( test_rec_obj_lcks ) )

  test_lck_refid <- test_rec_obj_lcks[[1]]$getproperties()[["id"]]
  test_lck_lstfiles <- list.files( test_cachepath, full.names = TRUE, recursive = TRUE, include.dirs = FALSE )

  test_lck_path <- base::unique(base::dirname( test_lck_lstfiles[ grepl( paste0( ".*/", test_lck_refid, "\\.json$" ), test_lck_lstfiles, perl = TRUE ) ] ))
  testthat::expect_length( test_lck_path, 1 )

  # inject lck file
  base::writeLines( "test lock", con = file.path( test_lck_path, "failcache.lck", fsep = "/" ) )
  testthat::expect_true( file.exists(file.path( test_lck_path, "failcache.lck", fsep = "/" )) )



  # -- test
  #' @cx.tests Push multiple cached objects to audit trail store ignoring locked cache entries
  result <- test_obj$push( store = cxaudit::cxaudit_store( test_dbcon ) )




  # -- expected

  # - expected number of record blocks
  expected_blocks <- length( test_rec_objs )

  
  # - expected blocks of records
  expected_rec_blocks <- list()

  #   note: preserve the block hierarchy from test_rec_objs ... i.e. length() + 1 
  for ( xblock in test_rec_objs ) {
    base::names(xblock) <- unlist( lapply( xblock, function(x) { x$getproperties()[["id"]] } ), use.names = FALSE )
    expected_rec_blocks[[ length(expected_rec_blocks) + 1 ]] <- xblock
  }

  
  # - expected list of records across blocks  
  expected_recs <- unlist( expected_rec_blocks, recursive = FALSE )


  # - expected main import properties  
  expected_import_props <- list( "event" = "import",
                                 "object.type" = "audit.record",
                                 "object.class" = "audit.record",
                                 "object.path" = base::tolower(test_cachepath),
                                 "label" = "Import of audit fail cache records",
                                 "actor" = as.character(Sys.info()[["user"]]) )


  # -- assertions

  testthat::expect_true( result )


  # - committed records

  actual_recs <- cxaudit:::.cxaudit_rdbstore( test_dbcon )$records()
  base::names(actual_recs) <- unlist( lapply( actual_recs, function(x) { x$getproperties()[["id"]] } ), use.names = FALSE )

  # note: adding import record on top of expected records
  # note: number of import records equal to the number of blocks
  testthat::expect_length( actual_recs, length(expected_recs) + expected_blocks )


  # - cached records
  #   note: retrieve one record at a time to verify attributes

  test_rdbstore <- cxaudit:::.cxaudit_rdbstore( test_dbcon )


  for ( xitem in base::names(expected_recs) ) {

    # get object from store
    act_obj <- test_rdbstore$get( xitem )


    # properties
    exp_props <- expected_recs[[xitem]]$getproperties()
    act_props <- act_obj$getproperties()

    testthat::expect_equal( act_props[ base::sort(cxaudit:::.cxaudit_propertynames()) ], exp_props[ base::sort(cxaudit:::.cxaudit_propertynames()) ] )


    # attributes
    testthat::expect_equal( length(expected_recs[[xitem]]$getattributes()), length(act_obj$getattributes()) )


    base::rm( list = c( "act_obj", "act_props", "exp_props" ) )
  }


  # - import records

  #   note: use filter for import event on records
  #   note: only expecting import records related to this specific test scenario
  actual_import_recs <- test_rdbstore$records( list( "events" = "import" ) )
  base::names(actual_import_recs) <- lapply( actual_import_recs, function(x) { x$getproperties()[["id"]] })

  testthat::expect_length( actual_import_recs, expected_blocks )
  testthat::expect_false( any( base::names(actual_import_recs) %in% base::names(expected_recs) ) )
  
  for ( ximprt in actual_import_recs ) {

    # get complete import record    
    imprt_rec <- test_rdbstore$get( ximprt$getproperties()[["id"]] )
    testthat::expect_equal( imprt_rec$getproperties()[["id"]], ximprt$getproperties()[["id"]] )

    # import record properties
    testthat::expect_equal( imprt_rec$getproperties()[ base::sort(base::names(expected_import_props)) ], 
                            expected_import_props[ base::sort(base::names(expected_import_props)) ] )
    
    
    # import record attributes
    testthat::expect_true( "record.count" %in% base::names(imprt_rec$getattributes()))
    
    # get import record links
    imprt_links <- imprt_rec$getlinks()
    base::names(imprt_links) <- lapply( imprt_links, function(x) { x$getproperties()[["id"]] } )

    # identify block and ensure that all link IDs are in the block
    # note: the any() in the if statement ensure that negative membership is included in the test
    for ( xblock in expected_rec_blocks )
      if ( any( base::names(imprt_links) %in% base::names(xblock)) ) {
        testthat::expect_equal( base::sort(base::names(imprt_links)), base::sort(base::names(xblock)) )
        testthat::expect_equal( as.numeric(imprt_rec$getattributes()[["record.count"]]), length(xblock) )
      }

    
    base::rm( list = c( "imprt_rec", "imprt_links" ) )
    
  }  #  end of for-loop across import event records    
  

})
