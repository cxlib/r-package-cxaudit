#
#
# Tests for audit get
#
# SQLite
#


#' @cx.testsfor cxaudit::cxaudit_get()



testthat::test_that( "audit.get.auditEnableNoRefId", {
  
  
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
                                           "AUDIT.DB.VENDOR = sqlite" ),
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage app properties file" )
  

  # - test store
  test_store <- cxaudit::cxaudit_store( test_dbcon )
  
  
  
  # -- test
  #' @cx.tests Get audit record with missing reference ID when auditing enabled for SQLite database store
  testthat::expect_error( cxaudit::cxaudit_get( store = test_store ), regexp = "^The specified record identity or reference is missing or invalid$" )
  
})






testthat::test_that( "audit.get.auditEnableRefIdNull", {
  
  
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
                                           "AUDIT.DB.VENDOR = sqlite" ),
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage app properties file" )
  
  
  # - test store
  test_store <- cxaudit::cxaudit_store( test_dbcon )
  
  
  
  # -- test
  #' @cx.tests Get audit record with reference ID equal to NULL when auditing enabled for SQLite database store
  testthat::expect_error( cxaudit::cxaudit_get( NULL, store = test_store ), regexp = "^The specified record identity or reference is missing or invalid$" )
  
})



testthat::test_that( "audit.get.auditEnableRefIdInvalid", {
  
  
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
                                           "AUDIT.DB.VENDOR = sqlite" ),
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage app properties file" )
  
  
  # - test store
  test_store <- cxaudit::cxaudit_store( test_dbcon )
  
  
  
  # -- test
  #' @cx.tests Get audit record with invalid reference ID when auditing enabled for SQLite database store
  testthat::expect_error( cxaudit::cxaudit_get( paste( sample( base::letters, 200, replace = TRUE), collapse = ""), store = test_store ), 
                          regexp = "^The record identifier is invalid$" )
  
})






testthat::test_that( "audit.get.auditEnableValidRefIdEmptyDatabase", {
  
  
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
                                           "AUDIT.DB.VENDOR = sqlite" ),
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage app properties file" )
  
  
  # - test id
  test_refid <- uuid::UUIDgenerate()
  
  
  # - test store
  test_store <- cxaudit::cxaudit_store( test_dbcon )
  
  
  
  
  # -- test
  #' @cx.tests Get audit record with valid reference ID from empty database results in NULL value when auditing enabled and using SQLite database store
  result <- cxaudit::cxaudit_get( test_refid, store = test_store )
  
  
  # -- assertions
  
  testthat::expect_null( result )

})





testthat::test_that( "audit.get.auditEnableValidByRefId", {
  
  
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
                                           "AUDIT.DB.VENDOR = sqlite" ),
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage app properties file" )
  

  
  # - test store
  test_store <- cxaudit::cxaudit_store( test_dbcon )
  

  # - test records
  #   note: 10 records blocks with each block randomly containing 1 to 20 records
  #   note: 1 block corresponds to 1 commit (records committed together are linked)
  #   note: sample any valid event name
  #   note: random reference string for object.type, object.class, object.path, actor and env
  #   note: SHA-1 digest of random string for object.hash
  #   note: random phrase for label with spaces   

  test_rec_objs <- replicate( 10, 
                              replicate( sample( 1:20, 1 ),
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
                                         simplify = TRUE ),
                              simplify = TRUE )
    

    
  # - stage records
  for ( xblock in test_rec_objs )
    testthat::expect_true( cxaudit::cxaudit_commit( xblock, store = test_store ) )
  
  # - test record id
  test_rec_refids <- unlist( lapply( base::unlist( test_rec_objs, recursive = TRUE ), function(x) { x$getproperties()[["id"]] } ) )
  test_refid <- sample( test_rec_refids, 1 )
  
  # -- test
  #' @cx.tests Get audit record by reference ID property when auditing enabled and using SQLite database store
  result <- cxaudit::cxaudit_get( test_refid, store = test_store )
  
  
  # -- expected
  
  expected_refid <- test_refid
  
  expected_objs <- unlist( test_rec_objs, recursive = TRUE )
  base::names(expected_objs) <- lapply( expected_objs, function(x) { x$getproperties()[["id"]] } )

  
  # - expected record links  
  expected_linkids <- character(0)
  
  for ( xblock in test_rec_objs ) {
    
    # block of one has no links ... cannot link onto itself
    if ( length(xblock) == 1 ) {

      # simplify = TRUE in replicated makes a block of one equal to the record object      
      if ( xblock$getproperties()[["id"]] == expected_refid )
        break()

      next()
    }
    
    xblock_refids <- unlist(lapply( xblock, function(x) { x$getproperties()[["id"]] } ))

    if ( expected_refid %in% xblock_refids ) {
      expected_linkids <- xblock_refids[ ! xblock_refids %in% expected_refid ]
      break()
    }

  }

  
  # -- assertions
  
  # - resulting object 
  testthat::expect_true( inherits( result, "cxaudit_record" ) )
  testthat::expect_equal( result$getproperties()[["id"]], expected_refid )
  testthat::expect_equal( result$getproperties()[ cxaudit:::.cxaudit_propertynames() ], expected_objs[[ expected_refid ]]$getproperties()[ cxaudit:::.cxaudit_propertynames() ] )
  

  # - result object links
  testthat::expect_length( result$getlinks(), length(expected_linkids) )

  if ( length(result$getlinks()) > 0 ) {  
    result_linkids <- unlist(lapply( result$getlinks(), function(x) { x$getproperties()[["id"]] } ))
    testthat::expect_equal( base::sort(result_linkids), base::sort(expected_linkids) )
  }
  

})






testthat::test_that( "audit.get.auditEnableValidByObject", {
  
  
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
                                           "AUDIT.DB.VENDOR = sqlite" ),
                                        con = base::file.path( test_libs, "cxapp", "app.properties" ) ) ), "try-error" ) ||
       ! file.exists( base::file.path( test_libs, "cxapp", "app.properties" ) ) )
    testthat::fail( "Could not stage app properties file" )
  
  
  
  # - test store
  test_store <- cxaudit::cxaudit_store( test_dbcon )
  
  
  # - test records
  #   note: 10 records blocks with each block randomly containing 1 to 20 records
  #   note: 1 block corresponds to 1 commit (records committed together are linked)
  #   note: sample any valid event name
  #   note: random reference string for object.type, object.class, object.path, actor and env
  #   note: SHA-1 digest of random string for object.hash
  #   note: random phrase for label with spaces   
  
  test_rec_objs <- replicate( 10, 
                              replicate( sample( 1:20, 1 ),
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
                                         simplify = TRUE ),
                              simplify = TRUE )
  

  
  # - stage records
  for ( xblock in test_rec_objs )
    testthat::expect_true( cxaudit::cxaudit_commit( xblock, store = test_store ) )
  
  # - test record object reference
  test_rec_objref <- sample( unlist( test_rec_objs, recursive = TRUE ), 1 )[[1]]

  
  # test_rec_refids <- unlist( lapply( base::unlist( test_rec_objs, recursive = TRUE ), function(x) { x$getproperties()[["id"]] } ) )
  # test_refid <- sample( test_rec_refids, 1 )
  
  # -- test
  #' @cx.tests Get audit record by reference to audit record when auditing enabled and using SQLite database store
  result <- cxaudit::cxaudit_get( test_rec_objref, store = test_store )
  
  
  # -- expected
  
  expected_refid <- test_rec_objref$getproperties()[["id"]]
  
  expected_objs <- unlist( test_rec_objs, recursive = TRUE )
  base::names(expected_objs) <- lapply( expected_objs, function(x) { x$getproperties()[["id"]] } )
  
  
  # - expected record links  
  expected_linkids <- character(0)
  
  for ( xblock in test_rec_objs ) {
    
    # block of one has no links ... cannot link onto itself
    if ( length(xblock) == 1 ) {
      
      # simplify = TRUE in replicated makes a block of one equal to the record object      
      if ( xblock$getproperties()[["id"]] == expected_refid )
        break()
      
      next()
    }
    
    xblock_refids <- unlist(lapply( xblock, function(x) { x$getproperties()[["id"]] } ))
    
    if ( expected_refid %in% xblock_refids ) {
      expected_linkids <- xblock_refids[ ! xblock_refids %in% expected_refid ]
      break()
    }
    
  }
  
  
  # -- assertions
  
  # - resulting object 
  testthat::expect_true( inherits( result, "cxaudit_record" ) )
  testthat::expect_equal( result$getproperties()[["id"]], expected_refid )
  testthat::expect_equal( result$getproperties()[ cxaudit:::.cxaudit_propertynames() ], expected_objs[[ expected_refid ]]$getproperties()[ cxaudit:::.cxaudit_propertynames() ] )
  
  
  # - result object links
  testthat::expect_length( result$getlinks(), length(expected_linkids) )
  
  if ( length(result$getlinks()) > 0 ) {  
    result_linkids <- unlist(lapply( result$getlinks(), function(x) { x$getproperties()[["id"]] } ))
    testthat::expect_equal( base::sort(result_linkids), base::sort(expected_linkids) )
  }
  
  
})


