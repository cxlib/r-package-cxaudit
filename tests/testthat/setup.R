#
#  Setup routine for test scenarios 
#
#
#



# -- set up test environment 

cxaudit_test_env <- base::new.env()



# -- identify sql file for testing with SQLite 

sql_roots <- character(0)


# - testthat working directory
sql_roots <- append( sql_roots, 
                     file.path( base::getwd(), c( "../../db", "../../inst/db" ), fsep = "/" ) )

  
# - current working directory
sql_roots <- append( sql_roots, 
                     file.path( base::getwd(), c( "db", "inst/db" ), fsep = "/" ) )


# - cxaudit package install directory                )
sql_roots <- append( sql_roots, 
                     file.path( .libPaths(), "cxaudit/db", fsep = "/" ) )

# - sql file paths
sql_paths <-  file.path( sql_roots, "sqlite.sql", fsep = "/" ) 


# - sql file (first occurence in list)

if ( ! any(file.exists(sql_paths)) )
  stop( "Could not find SQLite source file (sqlite.sql)")

sql_xpath <- utils::head( sql_paths[ file.exists(sql_paths) ], n = 1 )





# -- import sql statements

sql_file <- try( base::readLines( sql_xpath ), silent = FALSE )

if ( inherits( sql_file, "try-error" ) )
  stop( "Could not import sql statements" )


sql_lines <- paste0( base::gsub( "\\s{2,}", " ", 
                                 unlist( strsplit( paste( sql_file[ which( ! base::startsWith( base::trimws(sql_file), "--") & (base::trimws(sql_file) != "") ) ], 
                                                          collapse = " " ), 
                                                   ";", fixed = TRUE ), use.names = FALSE ) ), 
                     ";" )



# -- database connection

if ( ! base::exists( "testdbcon", envir = cxaudit_test_env ) || is.null(cxaudit_test_env$testdb) ) 
  base::assign( "testdbcon", 
                DBI::dbConnect( RSQLite::SQLite(), 
                                base::tempfile( pattern = "sqlite-db-", tmpdir = base::tempdir(), fileext = ".db") ),
                envir = cxaudit_test_env )


# -- database connection pool

if ( ! base::exists( "testdbpool", envir = cxaudit_test_env ) || is.null(cxaudit_test_env$testdb) ) 
  base::assign( "testdbpool", 
                pool::dbPool( RSQLite::SQLite(), 
                              dbname = base::tempfile( pattern = "sqlite-pooldb-", tmpdir = base::tempdir(), fileext = ".db"), 
                              minSize = 1,
                              maxSize = 1 ), 
                envir = cxaudit_test_env )
  



# - initiate database tables
for ( xdbcon in c( "testdbcon", "testdbpool") )
  for ( xsql in sql_lines )
    if ( inherits( try( DBI::dbExecute( base::get( xdbcon, envir = cxaudit_test_env ), xsql ), silent = FALSE ), "try-error" ) )
      stop( "Could not initiate SQLite database tables for ", xdbcon )






# -- teardown test database

withr::defer({
  
  # - close database connection
  if ( base::exists( "testdbcon", envir = cxaudit_test_env ) || is.null(cxaudit_test_env$testdb) ) {
    DBI::dbDisconnect( base::get("testdbcon", envir = cxaudit_test_env ) )
    base::rm( list = "testdbcon", envir = cxaudit_test_env )
  }
  
  
  # - close database connection pool
  if ( base::exists( "testdbpool", envir = cxaudit_test_env ) || is.null(cxaudit_test_env$testdb) ) {
    pool::poolClose(  base::get("testdbpool", envir = cxaudit_test_env ) )
    base::rm( list = "testdbpool", envir = cxaudit_test_env )
  }
  
  
  # - drop database files

  base::unlink( list.files( base::tempdir(), pattern = "\\.db$" ), recursive = FALSE, force = TRUE )
  

}, testthat::teardown_env() )





