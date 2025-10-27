#' Utility function to return an audit record storage object
#' 
#' @param x Database connection (optional)
#' 
#' @returns A simple cxaudit storage object
#' 
#' @description
#' A cxaudit storage object represents storage methods to commit, retrieve and
#' list audit records.
#' 
#' Note that below configuration uses app properties and \link[cxapp]{cxapp_config}.
#' 
#' If the `AUDIT` app property is not equal to enabled, `NULL` is returned.
#' 
#' If the `AUDIT` app property is equal to `enable` or `enabled`, the app
#' property `AUDIT.STORE` includes the keyword `database` and the app property
#' `AUDIT.DB.VENDOR` is equal to `sqlite` or `postgres`, all case insensitive, 
#' an initialized internal relational database storage object 
#' \link[cxaudit]{.cxaudit_rdbstore} is returned. 
#' 
#' If a connection `x` is specified, the storage object returned uses the connection.
#' If a connection is not specified, a connection is initiated from the cxaudit
#' configuration parameters.   
#' 
#' A database connection can be initiated when an existing connection is not
#' specified. 
#' 
#' For SQLite, use the following app properties
#' \itemize{
#'   \item `AUDIT.STORE` equal to `database`
#'   \item `AUDIT.DB.VENDOR` equal to `sqlite`
#'   \item `AUDIT.DB.PATH` where the path represents the SQLite database file
#' }
#' 
#' For Postgres, use the following app properties
#' \itemize{
#'   \item `AUDIT.STORE` equal to `database`
#'   \item `AUDIT.DB.VENDOR` equal to `postgres`
#'   \item `AUDIT.DB.DRIVER` as the R package database driver. Default is
#'         `RPostgreSQL::PostgreSQL()`.
#'   \item `AUDIT.DB.DATABASE` as database name
#'   \item `AUDIT.DB.HOST` as the database host
#'   \item `AUDIT.DB.PORT` as the database host port. Default is 5432.
#'   \item `AUDIT.DB.USERNAME` as the database account
#'   \item `AUDIT.DB.PASSWORD` as the database account password. Note that the 
#'         app configuration supports environmental variables and vaults.
#' }
#' 
#' Database connections use a pool of connections (\link[pool]{dbPool}) for
#' connection resilience even though most database transactions are single 
#' thread and relies on a single database connection. The pook configuration 
#' can be set using the app properties 
#' 
#' \itemize{
#'   \item `AUDIT.DB.POOL.MINSIZE` as pool minimum size. Default is 1.
#'   \item `AUDIT.DB.POOL.MAXSIZE` as pool maximum size. Default is 25.
#'   \item `AUDIT.DB.POOL.IDLETIMEOUT` as pool connection timeout. Default is 
#'         120 seconds.
#' }
#' 
#' 
#' 
#' 
#' 
#' 
#' @export

cxaudit_store <- function(x) {
  
  
  # -- configuration options
  cfg <- cxapp::cxapp_config()
  
  
  # -- audit enabled/disabled
  if ( ! is.logical( cfg$option( "audit", unset = FALSE ) ) || ! cfg$option( "audit", unset = FALSE ) )
    return(NULL)
  
  
  # -- supported relational databases
  if ( "database" %in% base::tolower(cfg$option( "AUDIT.STORE" ) ) &&
       any( c( "sqlite", "postgres") %in% base::tolower(cfg$option( "AUDIT.DB.VENDOR" )) ) ) {

    # - with specified connection object
    if ( ! missing(x) && ! is.null(x) )
      return(invisible( cxaudit:::.cxaudit_rdbstore( x ) ))

    
    # - create connection object if it does not exist
    
    if ( ! base::exists( ".cxaudit.rdbpool", envir = .GlobalEnv ) ) {
      
      # - postgres
      
      if ( base::tolower(cfg$option( "AUDIT.DB.VENDOR", unset = "unknwon" )) == "postgres" ) 
        base::assign( ".cxaudit.rdbpool", 
                      pool::dbPool( drv = eval(parse( text = cfg$option( "AUDIT.DB.DRIVER", unset = "RPostgreSQL::PostgreSQL()" ))),
                                    dbname = cfg$option( "AUDIT.DB.DATABASE", unset = "auditor" ),
                                    host = cfg$option( "AUDIT.DB.HOST", unset = "localhost" ),
                                    port = cfg$option( "AUDIT.DB.PORT", unset = 5432 ),
                                    user = cfg$option( "AUDIT.DB.USERNAME", unset = "auditor" ),
                                    password = cfg$option( "AUDIT.DB.PASSWORD", unset = paste( sample( c( base::letters, as.character(0:9) ), 40, replace = TRUE ), collapse = "") ),
                                    minSize = cfg$option( "AUDIT.DB.POOL.MINSIZE", unset = 1),
                                    maxSize = cfg$option( "AUDIT.DB.POOL.MAXSIZE", unset = 25),
                                    idleTimeout = cfg$option( "AUDIT.DB.POOL.IDLETIMEOUT", unset = 120) ),
                      envir = .GlobalEnv )
      
      
      # - sqlite
      
      if ( base::tolower(cfg$option( "AUDIT.DB.VENDOR", unset = "unknwon" )) == "sqlite" ) 
        base::assign( ".cxaudit.rdbpool", 
                      pool::dbPool( RSQLite::SQLite(),
                                    dbname = cfg$option( "AUDIT.DB.PATH", unset = base::tempfile( pattern = "sqlite-", tmpdir = base::tempdir(), fileext = ".db" ) ),
                                    minSize = cfg$option( "AUDIT.DB.POOL.MINSIZE", unset = 1),
                                    maxSize = cfg$option( "AUDIT.DB.POOL.MAXSIZE", unset = 25),
                                    idleTimeout = cfg$option( "AUDIT.DB.POOL.IDLETIMEOUT", unset = 120) ),
                      envir = .GlobalEnv )
      
      
      if ( ! base::exists( ".cxaudit.rdbpool", envir = .GlobalEnv ) )
        stop( "Database vendor not supported" )

    }
    
    # - return database storage object
    return(invisible( cxaudit:::.cxaudit_rdbstore( base::get( ".cxaudit.rdbpool", envir = .GlobalEnv ) ) ))
    
  }  # - end of audit.store equal to database 
    
    
 

  stop( "Audit store configuration missing or not supported" )
  
}