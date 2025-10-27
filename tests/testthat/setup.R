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





# -- import  sql statements

sql_file <- try( base::readLines( sql_xpath ), silent = FALSE )

if ( inherits( sql_file, "try-error" ) )
  stop( "Could not import sql statements" )


sql_lines <- paste0( base::gsub( "\\s{2,}", " ", 
                                 unlist( strsplit( paste( sql_file[ which( ! base::startsWith( base::trimws(sql_file), "--") & (base::trimws(sql_file) != "") ) ], 
                                                          collapse = " " ), 
                                                   ";", fixed = TRUE ), use.names = FALSE ) ), 
                     ";" )



# -- database

if ( ! base::exists( "testdbcon", envir = cxaudit_test_env ) || is.null(cxaudit_test_env$testdb) ) {

  # - initiate SQLite database file
  db_file <- base::tempfile( pattern = "sqlite-db-", tmpdir = base::tempdir(), fileext = ".db")

  # - create database connection  
  base::assign( "testdbcon", DBI::dbConnect( RSQLite::SQLite(), db_file ), envir = cxaudit_test_env )
  
}


# - initiate database tables
for ( xsql in sql_lines )
  if ( inherits( try( DBI::dbExecute( base::get( "testdbcon", envir = cxaudit_test_env ), xsql ), silent = FALSE ), "try-error" ) )
    stop( "Could not initiate SQLite database tables" )




# -- teardown test database

withr::defer({
  
  # - close database connection
  DBI::dbDisconnect( base::get("testdbcon", envir = cxaudit_test_env ) )
  base::assign( "testdbcon", NULL, envir = cxaudit_test_env )
  
  
  # - drop database
  base::unlink( db_file, recursive = FALSE, force = TRUE )
  

}, testthat::teardown_env() )





