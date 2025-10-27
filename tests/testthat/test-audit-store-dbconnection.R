#
#
# Tests for storage object by configuration
#
# Existing database connection
#


#' @cx.testsfor cxaudit::cxaudit_store()


#
#  Note: Tests for now only work with SQLite since that is a simple dependency
#




testthat::test_that( "audit.store.auditConfigEnabledDatabaseDBConnectionVendorNotDefined", {
  
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
       inherits( try( base::writeLines( c( "# empty app properties", 
                                           "AUDIT = enabled", 
                                           "AUDIT.STORE = database" ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ),
                 "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage app properties file with auditing disabled" )
  
  

  # - test connection 
  #   note: using the connection from setup
  
  if ( ! base::exists( "testdbcon", envir = cxaudit_test_env ) )
    testthat::fail( "Expected test connection defined in setup does not exists")
  
  test_dbcon <- base::get( "testdbcon", envir = cxaudit_test_env )
  
  
  
  
  # -- test
  #' @cx.tests Audit record database store when database connection specified but database vendor undefined
  testthat::expect_error( cxaudit::cxaudit_store( test_dbcon ), regexp = "^Audit store configuration missing or not supported$" )

})




testthat::test_that( "audit.store.auditConfigEnabledDatabaseDBConnectionVendorInvalid", {
  
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
       inherits( try( base::writeLines( c( "# empty app properties", 
                                           "AUDIT = enabled", 
                                           "AUDIT.STORE = database", 
                                           paste0( "AUDIT.DB.VENDOR = ", paste( sample( base::letters, 100, replace = TRUE ), collapse = "" )) ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ),
                 "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage app properties file with auditing disabled" )
  
  
  
  # - test connection 
  #   note: using the connection from setup
  
  if ( ! base::exists( "testdbcon", envir = cxaudit_test_env ) )
    testthat::fail( "Expected test connection defined in setup does not exists")
  
  test_dbcon <- base::get( "testdbcon", envir = cxaudit_test_env )
  
  
  
  
  # -- test
  #' @cx.tests Audit record database store when database connection specified and database vendor undefined
  testthat::expect_error( cxaudit::cxaudit_store( test_dbcon ), regexp = "^Audit store configuration missing or not supported$" )
  
})






testthat::test_that( "audit.store.auditConfigEnabledDatabaseDBConnectionVendorSQLite", {
  
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
       inherits( try( base::writeLines( c( "# empty app properties", 
                                           "AUDIT = enabled", 
                                           "AUDIT.STORE = database", 
                                           "AUDIT.DB.VENDOR = sqlite" ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ),
                 "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage app properties file with auditing disabled" )
  
  
  
  # - test connection 
  #   note: using the connection from setup
  
  if ( ! base::exists( "testdbcon", envir = cxaudit_test_env ) )
    testthat::fail( "Expected test connection defined in setup does not exists")
  
  test_dbcon <- base::get( "testdbcon", envir = cxaudit_test_env )
  
  
  
  
  # -- test
  #' @cx.tests Audit record database store when database connection specified and database vendor SQLite
  result <- cxaudit::cxaudit_store( test_dbcon )
  
  
  # -- assertions
  
  testthat::expect_equal( as.character(class(result)), ".cxaudit_rdbstore" )
  testthat::expect_equal( attr(class(result), "package", exact = TRUE), "cxaudit" )
  
  
  
})
