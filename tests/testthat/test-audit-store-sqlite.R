#
#
# Tests for storage object by configuration
#
# SQLite
#


#' @cx.testsfor cxaudit::cxaudit_store()


#
#  Note: Tests for now only work with SQLite since that is a simple dependency
#



testthat::test_that( "audit.store.auditConfigEnabledDatabaseSqliteDbFileExists", {

  # -- stage 
  
  
  # - test area
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - move in-memory existing rdb database pool 

  prev_rdbcon <- NA
  
  if ( exists( ".cxaudit.rdbpool", envir = .GlobalEnv ) )
    prev_rdbcon <- get( ".cxaudit.rdbpool", envir = .GlobalEnv )
  
  on.exit( {
    
    if ( exists( ".cxaudit.rdbpool", envir = .GlobalEnv ) ) {
      pool::poolClose(get( ".cxaudit.rdbpool", envir = .GlobalEnv ))
      base::rm( list = ".cxaudit.rdbpool", envir = .GlobalEnv )
    }
    
    if ( ! base::suppressWarnings(is.na(prev_rdbcon)) )
      base::assign( ".cxaudit.rdbpool", prev_rdbcon, envir = .GlobalEnv )
    
  }, add = TRUE )
  
  
  if ( exists( ".cxaudit.rdbpool", envir = .GlobalEnv ) )
    base::rm( list = ".cxaudit.rdbpool", envir = .GlobalEnv )
  
  if ( exists( ".cxaudit.rdbpool", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash database pool" )  
  
  
  
  
  # - stage sqlite database

  # test_dbfile <- base::tempfile( pattern = "sqlite-db-", tmpdir = base::tempdir(), fileext = ".db" )
  
    
  test_dbfile <- base::tempfile( pattern = "sqlite-db-", tmpdir = file.path( test_root, "db-sqlite", fsep = "/" ), fileext = ".db" )
  
  if ( ! dir.exists(base::dirname(test_dbfile)) && ! dir.create( base::dirname(test_dbfile), recursive = TRUE ) )
    testthat::fail( "Could not create parent directory for SQLite database file" )
  
  
  test_dbcon <-  DBI::dbConnect( RSQLite::SQLite(), test_dbfile )
  
  on.exit( {

    # note: only disconnect if the db connection is valid
    if ( DBI::dbIsValid(test_dbcon) && ! DBI::dbDisconnect( test_dbcon ) )
      stop( "Failed to clear database test connection" )
    
  }, add = TRUE, after = FALSE )
  
  

  test_sql <- c( "create table tbl_test (")

  test_tbl <- try( DBI::dbExecute( test_dbcon, "create table tbl_test ( str   varchar(128) );" ) )
  
  if ( inherits( test_tbl, "try-error") )
    testthat::fail( "Failed use test database connection to create test table" )
  
  
  
  
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
                                           "AUDIT.DB.VENDOR = sqlite", 
                                           paste0( "AUDIT.DB.PATH = ", test_dbfile ) ), 
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ),
                 "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage app properties file with auditing disabled" )
  
  
  # -- test
  #' @cx.tests Audit record SQLite database store enabled when the file defined by AUDIT.DB.PATH app configuration property exists
  result <- cxaudit::cxaudit_store()


  # -- assertions
  
  testthat::expect_equal( as.character(class(result)), ".cxaudit_rdbstore" )
  testthat::expect_equal( attr(class(result), "package", exact = TRUE), "cxaudit" )

})




testthat::test_that( "audit.store.auditConfigEnabledDatabaseSqliteDbFilePathNotDefined", {

  # -- stage


  # - test area

  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "", tmpdir = base::tempdir(), fileext = "") )

  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )

  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")



  # - move in-memory existing rdb database pool

  prev_rdbcon <- NA

  if ( exists( ".cxaudit.rdbpool", envir = .GlobalEnv ) )
    prev_rdbcon <- get( ".cxaudit.rdbpool", envir = .GlobalEnv )

  on.exit( {

    if ( exists( ".cxaudit.rdbpool", envir = .GlobalEnv ) ) {
      pool::poolClose(get( ".cxaudit.rdbpool", envir = .GlobalEnv ))
      base::rm( list = ".cxaudit.rdbpool", envir = .GlobalEnv )
    }

    if ( ! base::suppressWarnings(is.na(prev_rdbcon)) )
      base::assign( ".cxaudit.rdbpool", prev_rdbcon, envir = .GlobalEnv )

  }, add = TRUE )


  if ( exists( ".cxaudit.rdbpool", envir = .GlobalEnv ) )
    base::rm( list = ".cxaudit.rdbpool", envir = .GlobalEnv )

  if ( exists( ".cxaudit.rdbpool", envir = .GlobalEnv ) )
    testthat::fail( "Could not stash database pool" )




  # - stage sqlite database

  test_dbfile <- base::tempfile( pattern = "sqlite-db-", tmpdir = file.path( test_root, "db-sqlite", fsep = "/" ), fileext = ".db" )

  if ( ! dir.create( base::dirname(test_dbfile), recursive = TRUE ) )
    testthat::fail( "Filed to stage SQLite database file parent directory" )

  test_dbcon <-  DBI::dbConnect( RSQLite::SQLite(), test_dbfile )

  on.exit( {
    
    # note: only disconnect if the db connection is valid
    if ( DBI::dbIsValid(test_dbcon) && ! DBI::dbDisconnect( test_dbcon ) )
      stop( "Failed to clear database test connection" )
    
  }, add = TRUE, after = FALSE )
  



  test_sql <- c( "create table tbl_test (")

  test_tbl <- try( DBI::dbExecute( test_dbcon, "create table tbl_test ( str   varchar(128) );" ) )

  if ( inherits( test_tbl, "try-error") )
    testthat::fail( "Failed use test database connection to create test table" )




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


  # -- test
  #' @cx.tests Audit record SQLite database store enabled when the database file not defined by AUDIT.DB.PATH app configuration property
  result <- cxaudit::cxaudit_store()


  # -- assertions

  testthat::expect_equal( as.character(class(result)), ".cxaudit_rdbstore" )
  testthat::expect_equal( attr(class(result), "package", exact = TRUE), "cxaudit" )

})


