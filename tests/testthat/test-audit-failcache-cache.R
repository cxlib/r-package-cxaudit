#
#
# Tests for audit record fail cache store
#
# Adding content to fail cache
#

#' @cx.testsfor cxaudit:::cxaudit_failcache()




testthat::test_that( "audit.failcache.cacheNotEnabled", {
  
  
  # -- stage 
  
  
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
                                           "AUDIT.FAILCACHE = disabled",
                                           paste0( "AUDIT.FAILCACHE.PATH = ", test_cachepath ) ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage empty app properties file" )
  
  
  # - test cache
  test_obj <- cxaudit::cxaudit_failcache()
  
  
  # -- test
  #' @cx.tests Add audit records to fail cache with records specified missing results in an error
  testthat::expect_error( test_obj$cache(), regexp = "^The audit record fail cache is not enabled$" )
  
  # -- assertions
  testthat::expect_length( list.files( test_cachepath, pattern = "\\.json$", recursive = TRUE, include.dirs = FALSE ), 0 )
  
  
})




testthat::test_that( "audit.failcache.cacheEnabledAuditDisabled", {
  
  
  # -- stage 
  
  
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
                                           "AUDIT = disable",
                                           "AUDIT.FAILCACHE = enable",
                                           paste0( "AUDIT.FAILCACHE.PATH = ", test_cachepath ) ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage empty app properties file" )
  
  
  # - test cache
  test_obj <- cxaudit::cxaudit_failcache()
  
  
  # -- test
  #' @cx.tests Add audit records to fail cache with records specified missing results in an error
  testthat::expect_error( test_obj$cache(), regexp = "^The audit record fail cache is not enabled$" )
  
  # -- assertions
  testthat::expect_length( list.files( test_cachepath, pattern = "\\.json$", recursive = TRUE, include.dirs = FALSE ), 0 )
  
  
})





testthat::test_that( "audit.failcache.cacheNone", {
  
  
  # -- stage 
  
  
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
                                           "AUDIT.FAILCACHE = enabled",
                                           paste0( "AUDIT.FAILCACHE.PATH = ", test_cachepath ) ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage empty app properties file" )
  
  
  # - test cache
  test_obj <- cxaudit::cxaudit_failcache()
  
  
  # -- test
  #' @cx.tests Add audit records to fail cache with records specified missing results in an error
  testthat::expect_error( test_obj$cache(), regexp = "^A list of or an single audit record not specified$" )
  
  # -- assertions
  testthat::expect_length( list.files( test_cachepath, pattern = "\\.json$", recursive = TRUE, include.dirs = FALSE ), 0 )
  
  
})




testthat::test_that( "audit.failcache.cacheNull", {
  
  
  # -- stage 
  
  
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
                                           "AUDIT.FAILCACHE = enabled",
                                           paste0( "AUDIT.FAILCACHE.PATH = ", test_cachepath ) ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage empty app properties file" )
  
  
  # - test cache
  test_obj <- cxaudit::cxaudit_failcache()
  
  
  # -- test
  #' @cx.tests Add audit records to fail cache with records specified as Null results in an error
  testthat::expect_error( test_obj$cache( NULL ), regexp = "^A list of or an single audit record not specified$" )
  
  # -- assertions
  testthat::expect_length( list.files( test_cachepath, pattern = "\\.json$", recursive = TRUE, include.dirs = FALSE ), 0 )
  
})





testthat::test_that( "audit.failcache.cacheInvalidType", {
  
  
  # -- stage 
  
  
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
                                           "AUDIT.FAILCACHE = enabled",
                                           paste0( "AUDIT.FAILCACHE.PATH = ", test_cachepath ) ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage empty app properties file" )
  
  
  # - test cache
  test_obj <- cxaudit::cxaudit_failcache()
  
  
  # -- test
  #' @cx.tests Add audit records of invalid type to fail cache results in an error
  testthat::expect_error( test_obj$cache( paste( sample( base::letters, 20, replace = TRUE ), collapse = "" ) ), regexp = "^A list of or an single audit record not specified$" )
  
  # -- assertions
  testthat::expect_length( list.files( test_cachepath, pattern = "\\.json$", recursive = TRUE, include.dirs = FALSE ), 0 )
  
})





testthat::test_that( "audit.failcache.cacheEmptyList", {
  
  
  # -- stage 
  
  
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
                                           "AUDIT.FAILCACHE = enabled",
                                           paste0( "AUDIT.FAILCACHE.PATH = ", test_cachepath ) ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage empty app properties file" )
  
  
  # - test cache
  test_obj <- cxaudit::cxaudit_failcache()
  
  
  # -- test
  #' @cx.tests Add empty list of audit records to fail cache results no records added
  result <- test_obj$cache( list() ) 
  
  
  # -- assertions
  
  #    note: adding no records is the same as add all specified records (futility)
  testthat::expect_true( result )
  
  testthat::expect_length( list.files( test_cachepath, pattern = "\\.json$", recursive = TRUE, include.dirs = FALSE ), 0 )
  
  
})




testthat::test_that( "audit.failcache.cacheSingleRecordNoAttr", {
  
  
  # -- stage 
  
  
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
                                           "AUDIT.FAILCACHE = enabled",
                                           paste0( "AUDIT.FAILCACHE.PATH = ", test_cachepath ) ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage empty app properties file" )
  
  
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
  
  
  
  # - test cache
  test_obj <- cxaudit::cxaudit_failcache()
  
  
  # -- test
  #' @cx.tests Add single audit record without attributes to fail cache
  result <- test_obj$cache( test_rec_obj ) 
  
  
  # -- expected
  
  expected_recs <- test_rec_obj
  
  expected_cache_files <- paste0( expected_recs$getproperties()[["id"]], ".json" )
  
  expected_props <- cxaudit:::.cxaudit_propertynames()
  
  
  # -- assertions
  
  testthat::expect_true( result )
  
  
  # - cached records
  
  actual_cache_files <- list.files( test_cachepath, pattern = "\\.json$", recursive = TRUE, include.dirs = FALSE )
  
  testthat::expect_length( actual_cache_files, length(expected_cache_files) )
  testthat::expect_equal( base::sort(base::basename( actual_cache_files )), base::sort(expected_cache_files) )
  
  # - lock file
  testthat::expect_false( file.exists( file.path( base::unique(base::dirname(actual_cache_files)), "failcache.lck", fsep = "/") ) )
  
  
  # - assess file
  
  xfile <- actual_cache_files[ grepl( paste0( "^.*/", expected_recs$getproperties()[["id"]], ".json$" ), actual_cache_files ) ]
  testthat::expect_length( xfile, 1 )
  
  
  # - assess cached record
  
  lst_rec <- jsonlite::fromJSON( file.path( test_cachepath, xfile, fsep = "/" ) )
  
  testthat::expect_true( "label" %in% base::names(lst_rec) )
  lst_rec[["label"]] <- utils::URLdecode(lst_rec[["label"]])
  
  testthat::expect_true( "datetime" %in% base::names(lst_rec) )
  lst_rec[["datetime"]] <- as.POSIXct( lst_rec[["datetime"]], format = "%Y-%m-%dT%H:%M:%S", tz = "UTC" )
  
  
  testthat::expect_equal( base::sort(base::names(lst_rec)), base::sort(expected_props) )
  testthat::expect_equal( lst_rec[ base::sort(base::names(lst_rec)) ], expected_recs$getproperties()[ base::sort(expected_props) ] )
  
  testthat::expect_false( "attributes" %in% base::names(lst_rec) )
  
})




testthat::test_that( "audit.failcache.cacheSingleRecordWithAttr", {
  
  
  # -- stage 
  
  
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
                                           "AUDIT.FAILCACHE = enabled",
                                           paste0( "AUDIT.FAILCACHE.PATH = ", test_cachepath ) ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage empty app properties file" )
  
  
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
  
  # attributes
  test_obj_attr <- replicate( 10, 
                              base::trimws(paste( sample( c( base::letters, base::LETTERS, as.character(0:9), rep_len( " ", 3) ), sample( 10:60, 1), replace = TRUE ), collapse = "" )),
                              simplify = TRUE )
  
  base::names(test_obj_attr) <- replicate( 10, paste( paste( sample( base::letters, sample( 5:20, 1), replace = TRUE ), collapse = "" ), 
                                                      "value", 
                                                      sep = ":" ),
                                           simplify = TRUE )
  
  test_rec_obj$setattributes( test_obj_attr )
  
  
  # - test cache
  test_obj <- cxaudit::cxaudit_failcache()
  
  
  # -- test
  #' @cx.tests Add single audit record with attributes to fail cache
  result <- test_obj$cache( test_rec_obj ) 
  
  
  # -- expected
  
  expected_recs <- test_rec_obj
  
  expected_cache_files <- paste0( expected_recs$getproperties()[["id"]], ".json" )
  
  expected_props <- cxaudit:::.cxaudit_propertynames()
  
  
  
  # -- assertions
  
  testthat::expect_true( result )
  
  
  # - cached records
  
  actual_cache_files <- list.files( test_cachepath, pattern = "\\.json$", recursive = TRUE, include.dirs = FALSE )
  
  testthat::expect_length( actual_cache_files, length(expected_cache_files) )
  testthat::expect_equal( base::sort(base::basename( actual_cache_files )), base::sort(expected_cache_files) )
  
  # - lock file
  testthat::expect_false( file.exists( file.path( base::unique(base::dirname(actual_cache_files)), "failcache.lck", fsep = "/") ) )
  
  
  
  # - assess file
  
  xfile <- actual_cache_files[ grepl( paste0( "^.*/", expected_recs$getproperties()[["id"]], ".json$" ), actual_cache_files ) ]
  testthat::expect_length( xfile, 1 )
  
  
  # - assess cached record
  
  lst_rec <- jsonlite::fromJSON( file.path( test_cachepath, xfile, fsep = "/" ) )
  
  testthat::expect_true( "label" %in% base::names(lst_rec) )
  lst_rec[["label"]] <- utils::URLdecode(lst_rec[["label"]])
  
  testthat::expect_true( "datetime" %in% base::names(lst_rec) )
  lst_rec[["datetime"]] <- as.POSIXct( lst_rec[["datetime"]], format = "%Y-%m-%dT%H:%M:%S", tz = "UTC" )
  
  
  testthat::expect_true( all(expected_props %in% base::names(lst_rec) ) )
  testthat::expect_equal( lst_rec[ expected_props ], expected_recs$getproperties()[ expected_props ] )
  
  
  testthat::expect_true( "attributes" %in% base::names(lst_rec) )
  
  actual_attr_names <- base::names( lst_rec[["attributes"]] )
  
  lst_rec[["attributes"]] <- utils::URLdecode(lst_rec[["attributes"]])
  base::names(lst_rec[["attributes"]]) <- actual_attr_names
  
  
  testthat::expect_equal( base::sort(base::names(lst_rec[["attributes"]])), base::sort(base::names(expected_recs$getattributes())) )
  
  
  actual_rec_attrs <- base::unlist( lst_rec[["attributes"]], use.names = TRUE )
  testthat::expect_equal( actual_rec_attrs[ base::sort(base::names(actual_rec_attrs)) ], 
                          expected_recs$getattributes()[ base::sort(base::names(expected_recs$getattributes())) ] )
  
  
})






testthat::test_that( "audit.failcache.cacheMultiRecordWithAttr", {
  
  
  # -- stage 
  
  
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
  
  
  
  # - test cache
  test_obj <- cxaudit::cxaudit_failcache()
  
  
  # -- test
  #' @cx.tests Add multiple audit records with attributes to fail cache
  result <- test_obj$cache( test_rec_objs )
  
  
  
  # -- expected
  
  expected_recs <- test_rec_objs
  
  expected_cache_files <- unlist( lapply( expected_recs, function(x) { paste0( x$getproperties()[["id"]], ".json" ) } ), use.names = FALSE )
  
  expected_props <- cxaudit:::.cxaudit_propertynames()
  
  
  
  # -- assertions
  
  testthat::expect_true( result )
  
  
  # - cached records
  
  actual_cache_files <- list.files( test_cachepath, pattern = "\\.json$", recursive = TRUE, include.dirs = FALSE )
  
  testthat::expect_length( actual_cache_files, length(expected_cache_files) )
  testthat::expect_equal( base::sort(base::basename( actual_cache_files )), base::sort(expected_cache_files) )
  
  # - cached records belong to same group
  #   note: records are grouped by year-month and unique transaction ID
  testthat::expect_length( base::unique(base::dirname(actual_cache_files)), 1 )
  
  # - lock file
  testthat::expect_false( file.exists( file.path( base::unique(base::dirname(actual_cache_files)), "failcache.lck", fsep = "/") ) )
  
  
  
  # - assess cached records
  
  for ( xrec in expected_recs ) {
    
    # determine cached record file
    
    xfile <- actual_cache_files[ grepl( paste0( "^.*/", xrec$getproperties()[["id"]], ".json$" ), actual_cache_files, perl = TRUE ) ]
    testthat::expect_length( xfile, 1 )
    
    
    # import record
    
    lst_rec <- jsonlite::fromJSON( file.path( test_cachepath, xfile, fsep = "/" ) )
    
    testthat::expect_true( "label" %in% base::names(lst_rec) )
    lst_rec[["label"]] <- utils::URLdecode(lst_rec[["label"]])
    
    testthat::expect_true( "datetime" %in% base::names(lst_rec) )
    lst_rec[["datetime"]] <- as.POSIXct( lst_rec[["datetime"]], format = "%Y-%m-%dT%H:%M:%S", tz = "UTC" )
    
    
    testthat::expect_true( all(expected_props %in% base::names(lst_rec) ) )
    testthat::expect_equal( lst_rec[ expected_props ], xrec$getproperties()[ expected_props ] )
    
    if ( "attributes" %in% base::names(lst_rec) ) {
      
      actual_attr_names <- base::union( base::names(lst_rec[["attributes"]]), base::names(xrec$getattributes()) )
      
      testthat::expect_true( all( actual_attr_names %in% base::names(lst_rec[["attributes"]]) ) )
      testthat::expect_true( all( actual_attr_names %in% base::names(xrec$getattributes()) ) )
      
      
      lst_rec_attrnames <- base::names(lst_rec[["attributes"]])  
      lst_rec[["attributes"]] <- utils::URLdecode(lst_rec[["attributes"]])
      base::names(lst_rec[["attributes"]]) <- lst_rec_attrnames
      
      
      actual_rec_attrs <- base::unlist( lst_rec[["attributes"]], use.names = TRUE )
      testthat::expect_equal( actual_rec_attrs[ base::sort(base::names(actual_rec_attrs)) ],
                              xrec$getattributes()[ base::sort(base::names(xrec$getattributes())) ] )
      
    }
    
    # clean iteration
    base::rm( list = "lst_rec" )        
    
  }
  
  
})






