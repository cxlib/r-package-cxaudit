#
#
# Tests for audit record fail cache store
#
# 
#

#' @cx.testsfor cxaudit:::cxaudit_failcache()



testthat::test_that( "audit.failcache.noFailCacheConfig", {
  
  
  # -- stage 
  
  
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
                                           "AUDIT = enable" ),
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage empty app properties file" )
  
  
  # - test object
  test_obj <- cxaudit::cxaudit_failcache()  
  
  
  # -- test
  #' @cx.tests Audit record fail cache disabled when AUDIT.FAILCACHE app configuration property is missing
  result <- test_obj$isenabled()

  
  
  # -- assertions
  
  testthat::expect_false( result )
  
})





testthat::test_that( "audit.failcache.configInvalidEnableSwitch", {
  
  
  # -- stage 
  
  
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
                                           paste0( "AUDIT.FAILCACHE = ", paste( sample( base::letters, 100, replace = TRUE ), collapse = "" ) ) ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage empty app properties file" )
  
  
  # - test cache
  test_obj <- cxaudit::cxaudit_failcache() 
  
  
  # -- test
  #' @cx.tests Audit record fail cache disabled when AUDIT.FAILCACHE app configuration property is not a valid enable switch
  result <- test_obj$isenabled()
  
  
  # -- assertions
  
  testthat::expect_false( result )
  
})





testthat::test_that( "audit.failcache.configEnablePathNotConfigured", {
  
  
  # -- stage 
  
  
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
                                           paste0( "AUDIT.FAILCACHE = enabled" ) ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage empty app properties file" )
  
  
  # -- test
  #' @cx.tests Audit record fail cache path AUDIT.FAILCACHE.PATH not defined when fail cache is enabled
  testthat::expect_error( cxaudit::cxaudit_failcache(), regexp = "^Audit fail cache path not configured$" )

})




testthat::test_that( "audit.failcache.configEnablePathNotExist", {
  
  
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
  
  if ( dir.exists( test_cachepath ) )
    testthat::fail( "Fail cache path exists" )
  
  
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
  
  
  # -- test
  #' @cx.tests Audit record fail cache path AUDIT.FAILCACHE.PATH does not exist when fail cache is enabled
  testthat::expect_error( cxaudit::cxaudit_failcache(), regexp = "^Audit fail cache directory does not exist$" )
  
})




testthat::test_that( "audit.failcache.configEnablePathExistsAsFile", {
  
  
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

  if ( inherits( try( base::writeLines( "Test file", con = test_cachepath ) ), "try-error" ) )
    testthat::fail( "Failed to stage cache path as file" )
    

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
  
  
  # -- test
  #' @cx.tests Audit record fail cache path AUDIT.FAILCACHE.PATH does not exist when fail cache is enabled
  testthat::expect_error( cxaudit::cxaudit_failcache(), regexp = "^Audit fail cache directory does not exist$" )
  
})





testthat::test_that( "audit.failcache.configEnablePathExists", {
  
  
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
  #' @cx.tests Audit record fail cache path AUDIT.FAILCACHE.PATH exists when fail cache is enabled
  result <- test_obj$isenabled()
  

  # -- assertions
  
  testthat::expect_true( result )

})



testthat::test_that( "audit.failcache.isNotEnabled", {
  
  
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
  #' @cx.tests Assert audit record fail cache as disabled when app property AUDIT.FAILCACHE is equal to disabled
  result <- test_obj$isenabled()
  
  
  # -- assertions
  
  testthat::expect_false( result )
  
})





testthat::test_that( "audit.failcache.isEmptyNoEntries", {
  
  
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
  #' @cx.tests Audit fail cache is asserted as empty when the cache does not contain any files
  result <- test_obj$isempty()
  
  
  # -- assertions
  
  testthat::expect_true( result )
  
})






testthat::test_that( "audit.failcache.isEmptyNoJsonFiles", {
  
  
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
  
  
  # - test cache "misc" files
  #   note: no file should end in .json
  
  test_cachefiles <- replicate( 20, 
                                base::tempfile( pattern = "test-file-", 
                                                tmpdir = test_cachepath,
                                                fileext = paste0( ".", paste( sample( base::letters, 10, replace = TRUE ), collapse = "" ))), 
                                simplify = TRUE )
  
  for ( xfile in test_cachefiles )
    base::writeLines( paste( sample( base::letters, 10, replace = TRUE ), collapse = "" ), 
                      con = xfile )
  
  
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
  #' @cx.tests Audit fail cache is asserted as empty when the cache does not contain any JSON files
  result <- test_obj$isempty()
  
  
  # -- assertions
  
  testthat::expect_true( result )
  
})




testthat::test_that( "audit.failcache.isEmptyJsonFiles", {
  
  
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
  
  
  # - test cache files
  test_cachefiles <- replicate( 20, 
                                base::tempfile( pattern = "test-file-", 
                                                tmpdir = test_cachepath,
                                                fileext = ".json"), 
                                simplify = TRUE )
  
  for ( xfile in test_cachefiles )
    base::writeLines( paste( sample( base::letters, 10, replace = TRUE ), collapse = "" ), 
                      con = xfile )
  
  
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
  #' @cx.tests Audit fail cache is asserted as not empty when the cache does contains JSON files
  result <- test_obj$isempty()
  
  
  # -- assertions
  
  testthat::expect_false( result )
  
})


