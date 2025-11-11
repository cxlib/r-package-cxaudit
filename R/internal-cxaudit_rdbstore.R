#' Internal utility reference class to manage audit record database storage
#' 
#' @field .attr Internal attribute storage
#' 
#' @method initialize initialize
#' @method records records
#' @method show show
#' 
#' @description
#' Internal utility reference class to use a database to store audit records.
#' 
#' The `list` method retrieves a list of records given the conditions `x`. See
#' \link[cxaudit]{cxaudit_list} for supported options. The results are returned
#' as a nested list.
#' 
#'  
#' The `commit`method takes a list `x` of audit records and stores them in the 
#' database. The entries of `x` are of type \link[cxaudit]{cxaudit_record}.
#' 
#' 





.cxaudit_rdbstore <- methods::setRefClass( ".cxaudit_rdbstore",
                                           fields = list( ".attr" = "list" ) )



.cxaudit_rdbstore$methods( "new" = function() {
})
  


.cxaudit_rdbstore$methods( "initialize" = function(x) {
  "Initialize relation database store"


  # -- initialize internals
  .self$.attr <- list()


  # -- configuration
  cfg <- cxapp::.cxappconfig()
  
  
  # - debug setting
  .self$.attr[["mode.try.silent"]] <- ! cfg$option( "mode.debug", unset = FALSE )

  

  # -- database connection
  
  if ( is.null(x) )
    stop( "Database connection missing" )

  
  
  
  is_dbcon_validresp <- try( DBI::dbGetQuery( x, "select current_timestamp;" ), silent = .self$.attr[["mode.try.silent"]] )
  
  if ( inherits( is_dbcon_validresp, "try-error" ) )
    stop( "Database connection not valid or unreachable" )
  
  
  # is_dbcon_valid <- try( DBI::dbIsValid(x), silent = .self$.attr[["mode.try.silent"]] )
  # 
  # if ( inherits( is_dbcon_valid, "try-error") || ! inherits( is_dbcon_valid, "logical" ) || ! is_dbcon_valid )
  #   stop( "Database connection not valid" )

  .self$.attr[["dbcon"]] <- x

})



.cxaudit_rdbstore$methods( "commit" = function(x) {
  "Commit audit records to database"
  
  
  if ( missing(x) || is.null(x) || ! inherits( x, c( "list", "cxaudit_record") ) )
    stop( "Audit records missing or invalid" )
  
  
  # -- arguments in list form

  lst_records <- list()
  
  if ( inherits( x, "list") )
    lst_records <- x

  if ( inherits( x, "cxaudit_record") )
    lst_records[[1]] <- x
  
  
  # -- list of records
  
  lst <- list()

  for ( xitem in lst_records ) {
    
    if (  ! inherits( xitem, "cxaudit_record" ) || is.null(attr(class(xitem), "package")) || ( attr(class(xitem), "package") != "cxaudit" ) )
      stop( "One or more committed items is not an audit record" )

    
    # - record properties
    xitem_props <- xitem$getproperties()
    
    if ( any( ! cxaudit:::.cxaudit_propertynames() %in% base::names(xitem_props) ) )
      stop( "Audit record is incomplete and missing required properties" )


    # - retain first occurrence    
    if ( xitem_props[["id"]] %in% base::names(lst) )
      next()

    lst[[ xitem_props[["id"]] ]] <- xitem_props
    
    
    # - record attributes
    xitem_attrs <- xitem$getattributes()

    if ( length(xitem_attrs) > 0 )    
      lst[[ xitem_props[["id"]] ]][["_attrs"]] <- xitem_attrs


  }  # - end of for-loop across submitted records
  
  

  

  # -- generate sql insert value strings
  
  sql_rec_insrt <- character(0)
  sql_attr_insrt <- character(0)
  
  
  sql_maxlength <- c( "event" = 50, 
                      "object.type" = 100, 
                      "object.class" = 100, 
                      "object_name" = 100, 
                      "object_bin" = 4096, 
                      "object.hash" = 128, 
                      "label" = 1024,
                      "actor" = 512, 
                      "env" = 512, 
                      "attr.key" = 100, 
                      "attr.qual" = 100, 
                      "attr.value" = 1024 )
  
  
  
  for ( xref in base::names(lst) ) {
    
    # -- properties
    
    sql_values <- character(0)
    
    for ( xprop in c( "id", "event", "object.type", "object.class", "object.path", "object.hash", "label", "actor", "env", "datetime" ) ) {

      if ( ! xprop %in% base::names(lst[[xref]]) )
        stop( "Expected property not defined" )
      
      
      if ( xprop == "object.path" ) {
        
        if ( ( base::nchar(base::basename( as.character(lst[[xref]][[xprop]]) )) > sql_maxlength[ "object_name"] ) ||
             ( base::nchar(base::dirname( as.character(lst[[xref]][[xprop]]) )) > sql_maxlength[ "object_bin"] ) )
          stop( "Object path elements exceeds database table column size")
          
        
        sql_values <- append( sql_values,
                              c( base::sQuote( base::tolower(base::basename( as.character(lst[[xref]][[xprop]]) )), q = FALSE ), 
                                 base::sQuote( ifelse( grepl( "/", lst[[xref]][[xprop]]), base::tolower(base::dirname( as.character(lst[[xref]][[xprop]]) )), "/" ), q = FALSE ) ) )
        next()
      }
        
      if ( xprop == "label" ) {
        
        if ( base::nchar(utils::URLencode( lst[[xref]][[xprop]], reserved = TRUE)) > sql_maxlength["label"] )
          stop( "Encoded record label exceeds database table column size" )
        
        sql_values <- append( sql_values,
                              base::sQuote( utils::URLencode( lst[[xref]][[xprop]], reserved = TRUE), q = FALSE ) )
        next()
      }
      
      
      if ( xprop == "datetime" ) {
        
        # note: first value is associated with record dt_date
        # note: second value is associated with record ts_datetime
        sql_values <- append( sql_values,
                              c( base::sQuote( format( lst[[xref]][[xprop]], format = "%Y-%m-%d" ), q = FALSE ), 
                                 base::sQuote( format( lst[[xref]][[xprop]], format = "%Y-%m-%d %H:%M:%S" ), q = FALSE ) ) )
        next()
      }
      
      
      if ( xprop %in% base::names(sql_maxlength) && ( base::nchar(lst[[xref]][[xprop]]) > sql_maxlength[xprop] ) )
        stop( "Property ", xprop, " exceeds database table column size or maximum column size not defined" )
            
      sql_values <- append( sql_values, base::sQuote( base::tolower(base::trimws( as.character(lst[[xref]][[xprop]]) )), q = FALSE) )
      
    }  # - end of for-loop across list of properties

    sql_rec_insrt <- append( sql_rec_insrt, paste0( "(", paste( sql_values, collapse = ","), ")" ) )

    # - end of properties
    
    
    # - attributes
    
    if ( ! "_attrs" %in% base::names(lst[[xref]]) || (length(lst[[xref]][["_attrs"]]) == 0) )
      next()
    

    for ( xattr in base::names(lst[[xref]][["_attrs"]])) {
      
      xattr_keys <- unlist(strsplit( xattr, ":", fixed = TRUE ))
      
      if ( length(xattr_keys) != 2 )
        xattr_keys[2] <- "value"
      
      
      if ( (base::nchar(xattr_keys[1]) > sql_maxlength["attr.key"]) ||
           (base::nchar(xattr_keys[2]) > sql_maxlength["attr.qual"])  )
        stop( "Attribute key and/or qualifier exceeds database table column size" )
      
      if ( (base::nchar(utils::URLencode( base::trimws(lst[[xref]][["_attrs"]][[xattr]]), reserved = TRUE)) > sql_maxlength["attr.value"]) )
        stop( "Encoded attribute value exceeds database table column size" )

      sql_values <- c( base::sQuote(lst[[xref]][["id"]], q = FALSE ), 
                       base::sQuote( xattr_keys[1:2], q = FALSE ), 
                       base::sQuote( utils::URLencode( base::trimws(lst[[xref]][["_attrs"]][[xattr]]), reserved = TRUE), q = FALSE ) )
      
      sql_attr_insrt <- append( sql_attr_insrt, paste0( "(", paste( sql_values, collapse = "," ), ")" ) )
      
    }
    
    
    # - end of properties
    
  }  # - end of for-loop across audit records
  
  
  
  # -- build sql sequence

  
  sql <- character(0)
  
    
  # - add work records
  
  sql <- append( sql, 
                 paste( c( "insert into tbl_adt_wrkrecords", 
                           "( uid, str_event, str_objtype, str_objclass, str_objname, str_objbin, str_objhash, str_label, str_actor, str_env, dt_date, ts_datetime )", 
                           "values", 
                           paste( sql_rec_insrt, collapse = ", " ), 
                           ";" ), collapse = "  " ) ) 
    
    
  # - add work attributes

  if ( length(sql_attr_insrt) > 0 ) 
    sql <- append( sql, 
                   paste( c( "insert into tbl_adt_wrkrecord_attrs", 
                             "( uid_rec, str_key, str_qual, str_value )", 
                             "values", 
                             paste( sql_attr_insrt, collapse = ", " ), 
                             ";" ), collapse = "  " ) ) 
  
  
  # - add insert query for record 

  sql <- append( sql, 
                 paste( c( "insert into tbl_adt_records", 
                           "( uid, str_event, str_objtype, str_objclass, str_objname, str_objbin, str_objhash, str_label, str_actor, str_env, dt_date, ts_datetime )", 
                           "select uid, str_event, str_objtype, str_objclass, str_objname, str_objbin, str_objhash, str_label, str_actor, str_env, dt_date, ts_datetime from tbl_adt_wrkrecords",
                           "where ( uid in (", paste( base::sQuote( base::names(lst), q = FALSE), collapse = ", " ), ") )",
                           ";" ), collapse = "  " ) )
  
  
  # - add insert query for record attributes
  
  sql <- append( sql, 
                 paste( c( "insert into tbl_adt_record_attrs", 
                           "(uid_rec, str_key, str_qual, str_value, int_vseq)", 
                           "select uid_rec, str_key, str_qual, str_value, int_vseq from tbl_adt_wrkrecord_attrs",
                           "where ( uid_rec in (", paste( base::sQuote( base::names(lst), q = FALSE), collapse = ", " ), ") )",
                           ";" ), collapse = "  " ) )

  
  # - add commit
  
  uid_commit <- uuid::UUIDgenerate()
  
  sql <- append( sql, 
                 paste( c( "insert into tbl_adt_commits", 
                           "( uid, uid_rec )", 
                           "values", 
                           paste( paste0( "(", paste( base::sQuote(uid_commit, q = FALSE), base::sQuote(base::names(lst), q = FALSE), sep = ", " ), ")" ), collapse = ", " ), 
                           ";" ), collapse = " " ) )
  
  
  # - drop work records
  
  sql <- append( sql, 
                 paste( c( "delete from tbl_adt_wrkrecord_attrs", 
                           "where ( uid_rec in (", paste( base::sQuote( base::names(lst), q = FALSE), collapse = ", " ), ") )",
                           ";" ), collapse = "  " ) )
  
  sql <- append( sql, 
                 paste( c( "delete from tbl_adt_wrkrecords", 
                           "where ( uid in (", paste( base::sQuote( base::names(lst), q = FALSE), collapse = ", " ), ") )",
                           ";" ), collapse = "  " ) )
  
  

  
  # -- commit to database
  
  if ( inherits( .self$.attr[["dbcon"]], c( "Pool", "pool") ) ) {

    # - connection is from a pool ... pool::dbPool()
    
    pool_commit <- try( pool::poolWithTransaction( .self$.attr[["dbcon"]], function( dbcon ) {
      
      # note: pool::poolWithTransaction() performs commit and eventual rollback on error
      
      for ( xstmt in sql ) 
        DBI::dbExecute( dbcon, xstmt )
    
    }), silent = .self$.attr[["mode.try.silent"]] )

        
    if ( inherits( pool_commit, "try-error" ) )
      stop( "Could not commit audit records to the database pool conneciton" )
    

  } else {
    
    # - a database connection object ( not pool::dbPool() )
    
    if ( ! DBI::dbBegin( .self$.attr[["dbcon"]] ) )
      stop( "Could not start transaction" )
    
    
    for ( xstmt in sql ) 
      DBI::dbExecute( .self$.attr[["dbcon"]], xstmt ) 
    
    
    if ( ! DBI::dbCommit( .self$.attr[["dbcon"]] ) )  {
      
      if ( ! DBI::dbRollback( .self$.attr[["dbcon"]] ) )
        stop( "Could not commit transaction and rollback failed" )
      
      return(invisible(FALSE))
    }

  }  # end of if-else-statement for connection inherited from a database pool
  


  return(invisible(TRUE))
})





.cxaudit_rdbstore$methods( "get" = function(x) {

  
  if ( missing(x) || is.null(x) || any(is.na(x)) || (length(x) != 1) || ! inherits( x, "character") || ! uuid::UUIDvalidate(x) )
    stop( "Audit record identifier missing or not in a valid format" )
  
  
  # -- sql 

  sql <- paste( "select cast(a.uid as varchar(128)) as uid, a.str_event, a.str_objtype, a.str_objclass, a.str_objname, a.str_objbin, a.str_objhash, a.str_label, a.str_actor, a.str_env, a.ts_datetime,", 
                "cast(b.uid_rec as varchar(128)) as uid_rec, b.str_key, b.str_qual, b.str_value, b.int_vseq",
                "from (select * from tbl_adt_records where ( uid = ", base::sQuote( base::trimws(x), q = FALSE), ") ) a",
                "left join tbl_adt_record_attrs b", 
                "on ( a.uid = b.uid_rec )",
                "order by uid, str_key, str_qual, int_vseq",
                ";" )

  db_qry <- try( DBI::dbGetQuery( .self$.attr[["dbcon"]], sql ) )
  
  if ( inherits( db_qry, "try-error" ) )
    stop( "Database query to retrieve audit record resulted in an error" )

  if ( base::nrow(db_qry) == 0 )
    return(invisible(NULL))


  # -- integrity check
  if ( base::unique(as.character(db_qry[, "uid"])) != base::trimws(x) )
    stop( "Integrity constraint on record ID failed with unexpected value" )
  
  

  # -- main audit record
  
  rec_props <- list( "id" = db_qry[ 1, "uid"], 
                     "event" = db_qry[ 1, "str_event"], 
                     "object.type" = db_qry[ 1, "str_objtype"],
                     "object.class" = db_qry[ 1, "str_objclass"],
                     "object.path" = paste( db_qry[ 1, "str_objbin"], db_qry[ 1, "str_objname"], sep =  ifelse( db_qry[ 1, "str_objbin"] == "/", "", "/" ) ), 
                     "object.hash" = db_qry[ 1, "str_objhash"], 
                     "label" = utils::URLdecode( db_qry[ 1, "str_label"] ), 
                     "actor" = db_qry[ 1, "str_actor"], 
                     "env" = db_qry[ 1, "str_env"],
                     "datetime" = as.POSIXct( db_qry[ 1, "ts_datetime"], format = "%Y-%m-%d %H:%M:%S" ) )
  
  audit_rec <- cxaudit::cxaudit_record( rec_props )
  
    
  # -- record attributes
  if ( ! is.na( db_qry[ 1, "uid_rec"] ) ) {
    
    audit_rec_attr <- character(0)
    
    for ( xrow in 1:base::nrow(db_qry) ) {
      
      xname <- base::tolower(base::trimws( db_qry[ xrow, "str_key"] ))
      
      if ( base::tolower(base::trimws(db_qry[ xrow, "str_qual"])) != "value" )
        xname <- paste0( xname, ":", base::tolower(base::trimws( db_qry[ xrow, "str_qual"] )) )
      
      xvalue <- utils::URLdecode(db_qry[ xrow, "str_value"])
      
      audit_rec_attr[ xname ] <- xvalue
    }

    if ( length(audit_rec_attr) > 0 )
      audit_rec$setattributes( audit_rec_attr )
    
  }
    
  
  # -- close query
  base::rm( list = c( "db_qry", "sql" ) )
  
  
  
  # -- record links
  
  #    note: query strategy
  #    note: 1) select all commits tbl_adt_commits:uid related to the requested record tbl_adt_commits:uid_rec
  #    note: 2) select all records tbl_adt_commits:uid_rec associated with the identified commits in (1)
  #    note: 3) select all records tbl_adt_records:uid identified in (2)
  
  sql <- paste( "select cast(uid as varchar(128)) as uid, str_event, str_objtype, str_objclass, str_objname, str_objbin, str_objhash, str_label, str_actor, str_env, ts_datetime from tbl_adt_records", 
                "where ( uid in ( select uid_rec from tbl_adt_commits",
                "where ( uid in (select uid from tbl_adt_commits where ( uid_rec = ", base::sQuote( base::trimws(x), q = FALSE), " )) ) ) );" )

  db_qry <- try( DBI::dbGetQuery( .self$.attr[["dbcon"]], sql ) )
    
  if ( inherits( db_qry, "try-error" ) )
    stop( "Database query to retrieve audit record links resulted in an error" )
  
  if ( base::nrow(db_qry) == 0 )
    return(invisible(audit_rec))
  

  # - add record links 
  #   note: filter out main record
    
  lnk_recs <- list()

  for ( xrow in 1:base::nrow(db_qry) ) 
    if ( base::trimws(rec_props[["id"]]) != base::trimws(db_qry[ xrow, "uid"]) )
      lnk_recs[[ length(lnk_recs) + 1 ]] <- cxaudit::cxaudit_record( list( "id" = db_qry[ xrow, "uid"], 
                                                                           "event" = db_qry[ xrow, "str_event"], 
                                                                           "object.type" = db_qry[ xrow, "str_objtype"],
                                                                           "object.class" = db_qry[ xrow, "str_objclass"],
                                                                           "object.path" = paste( db_qry[ xrow, "str_objbin"], db_qry[ xrow, "str_objname"], sep =  ifelse( db_qry[ xrow, "str_objbin"] == "/", "", "/" ) ), 
                                                                           "object.hash" = db_qry[ xrow, "str_objhash"], 
                                                                           "label" = utils::URLdecode( db_qry[ xrow, "str_label"] ), 
                                                                           "actor" = db_qry[ xrow, "str_actor"], 
                                                                           "env" = db_qry[ xrow, "str_env"],
                                                                           "datetime" = as.POSIXct( db_qry[ xrow, "ts_datetime"], format = "%Y-%m-%d %H:%M:%S" ) ) )
  
  if ( length(lnk_recs) > 0 )
    if ( inherits( try( audit_rec$.setlinks( lnk_recs ) ), "try-error" ) )
      stop( "Could not link audit records" )
  
  
  # -- close query
  base::rm( list = c( "db_qry", "sql" ) )
  

  return(invisible(audit_rec))
})





.cxaudit_rdbstore$methods( "records" = function(x) {

  
    # -- default filter
    #    note: all filters are assumed meaning "and"
    rec_filter <- list( "events" = NULL,
                        "object.types" = NULL,
                        "object.classes" = NULL,
                        "object.parentpaths" = NULL, 
                        "object.paths" = NULL,
                        "object.names" = NULL,
                        "object.hashes" = NULL,
                        "actors" = NULL,
                        "envs" = NULL,
                        "from" = base::as.POSIXct( Sys.Date() - 30, tz = "UTC"),
                        "to" = base::as.POSIXct( Sys.time(), tz = "UTC"),
                        "limit" = 300,
                        "offset" = NULL,
                        "select" = "last" )
    
    

    # -- identify submitted filter

    if ( ! missing(x) && ! is.null(x) && ! any(is.na(x)) )
      for ( xitem in base::tolower(base::trimws(names(x))) )
        if ( xitem %in% names(rec_filter) ) {
          
          if ( xitem %in% c( "from", "to") && ! inherits(x[[xitem]], c( "POSIXct", "POSIXt" ) ) )
            stop( "Invalid date/time specified for date/time interval" )
            
          
          if ( xitem %in% c( "from", "to", "limit", "offset", "select") && (length(x[[xitem]]) != 1) ) 
            stop( "Only single value expected for filter options from, to, limit, offset and select" )
          
          
          if ( xitem == "select" && ! base::tolower(base::trimws(x[[xitem]])) %in% c( "first", "last" ) )
            stop( "Record block selection invalid" )
          
          
          if ( xitem %in% c( "limit", "offset" ) && ! is.null(x[[xitem]]) && ! is.numeric(x[[xitem]]) )
            stop( "Record limit or offset not a number" )
          
          

          # - raw value
          if ( xitem %in% c( "from", "to") ) {
            rec_filter[[ xitem ]] <- x[[xitem]]
            next()
          }
          
        
          
          rec_filter[[ xitem ]] <- base::tolower(base::trimws(x[[xitem]]))
          
        }

    
    
    # -- ensure required filter parts are not NA
    for ( xitem in c( "from", "to" ) )
      if ( is.null(rec_filter[[xitem]]) || is.na(rec_filter[[xitem]])) 
        stop( "The record filter property ", xitem, " cannot equal NULL or NA")
      



    # -- assemble SQL filter
    #    note: strategy is to limit the number of records that need to be traversed
    #    note: prioritize identifying blocks of records as early as possible 
    #    note: use block record indexed columns as early as possible to influence 
    #          database execution plan

    sql_whr <- character(0)
    
    col_map <- c( "events" = "str_event",
                  "object.types" = "str_objtype", "object.classes" = "str_objclass", "object.names" = "str_objname", "object.hashes" = "str_objhash",
                  "envs" = "str_env", "actors" = "str_actor" )

    
    # - add filter on date/time range
    #   note: record block index column dt_date for date range 

    sql_whr <- append( sql_whr,
                       c( paste( "dt_date >=", base::sQuote(base::trimws(format( rec_filter[["from"]], format = "%Y-%m-%d")), q = FALSE), sep = " " ), 
                          paste( "dt_date <=", base::sQuote(base::trimws(format( rec_filter[["to"]], format = "%Y-%m-%d")), q = FALSE), sep = " " ), 
                          paste( "ts_datetime >=", base::sQuote(base::trimws(format( rec_filter[["from"]], format = "%Y-%m-%d %H:%M:%S")), q = FALSE), sep = " " ), 
                          paste( "ts_datetime <=", base::sQuote(base::trimws(format( rec_filter[["to"]], format = "%Y-%m-%d %H:%M:%S")), q = FALSE), sep = " " ) ) )


    # - add record block index column for object parent paths  
    
    if ( ! is.null(rec_filter[["object.parentpaths"]]) && ! all(is.na(rec_filter[["object.parentpaths"]])) && (length(rec_filter[["object.parentpaths"]]) > 0) )  
      sql_whr <- append( sql_whr, 
                         paste( "str_objbin in (", paste( base::sQuote( base::tolower(base::unique(base::trimws(rec_filter[["object.parentpaths"]]))), q = FALSE), collapse = ", "), ")" ) )

    
    # - add record block index column for object paths  
    
    if ( ! is.null(rec_filter[["object.paths"]]) && ! all(is.na(rec_filter[["object.paths"]])) && (length(rec_filter[["object.paths"]]) > 0) )  {

        sql_whr_itemx <- character(0)
        
        for ( xpath in rec_filter[["object.paths"]] )
          sql_whr_itemx <- append( sql_whr_itemx, 
                                   paste( "(", paste( "str_objbin =", base::sQuote(base::tolower(base::dirname(base::trimws(xpath))), q = FALSE) ), 
                                               "and",
                                               paste( "str_objname =", base::sQuote(base::tolower(base::basename(base::trimws(xpath))), q = FALSE) ), 
                                          ")" )  )
                                   
      
        sql_whr <- append( sql_whr, 
                           paste0( "(", paste( sql_whr_itemx, collapse = " or " ), ")" ) )  

    }  # - end of if-statement for object paths

    

    # - add filter properties
    
    for ( xitem in c( "events", "object.types", "object.classes", "object.hashes", "object.names", "actors", "envs" ) )
      if ( xitem %in% base::tolower(names(rec_filter)) && ! is.null(rec_filter[[xitem]]) && (length(rec_filter[[xitem]]) > 0) ) 
        sql_whr <- append( sql_whr,
                           paste( col_map[xitem], "in (", paste( base::sQuote( base::tolower(base::trimws(rec_filter[[xitem]])), q = FALSE), collapse = ", " ), ")", sep = " " ) )
        

      


    
    # -- assemble SQL query
    
    sql <- c( "select cast(uid as varchar(128)) as uid, str_event, str_objtype, str_objclass, str_objname, str_objbin, str_objhash, str_label, str_actor, str_env, ts_datetime", 
              "from tbl_adt_records",
              paste( "where (", paste( paste( "(", sql_whr, ")", sep = " " ), collapse = " and "), ")" ),
              paste( "order by ts_datetime", ifelse( ( base::tolower(base::trimws(rec_filter[["select"]])) == "first" ), "", "desc" ) ) )
              

    if ( "limit" %in% names(rec_filter) && ! is.null(rec_filter[["limit"]])  )
      sql <- append( sql,
                     paste( "limit", as.character(rec_filter[["limit"]]), sep = " ") )
    
    if ( "offset" %in% names(rec_filter) && ! is.null(rec_filter[["offset"]]) )
      sql <- append( sql,
                     paste( "offset", as.character(rec_filter[["offset"]]), sep = " ") )


        
    # -- initialize return
    
    recs <- list()
    attr( recs, "filter" ) <- rec_filter[ ! as.logical(lapply( rec_filter, is.null) ) ]

    
    # -- query database

    db_qry <- try( DBI::dbGetQuery( .self$.attr[["dbcon"]], paste( c( sql, ";"), collapse = " " ) ), silent = TRUE )

    if ( inherits( db_qry, "try-error" ) || ( base::nrow(db_qry) == 0 ) )
      return(invisible( recs ))
    

    # note: forcing time zone UTC
    
    for ( xrow in 1:base::nrow(db_qry) ) 
      recs[[ xrow ]] <- cxaudit::cxaudit_record( list( "id" = db_qry[ xrow, "uid"], 
                                                       "event" = db_qry[ xrow, "str_event"], 
                                                       "object.type" = db_qry[ xrow, "str_objtype"],
                                                       "object.class" = db_qry[ xrow, "str_objclass"],
                                                       "object.path" = paste( db_qry[ xrow, "str_objbin"], db_qry[ xrow, "str_objname"], sep =  ifelse( db_qry[ xrow, "str_objbin"] == "/", "", "/" ) ), 
                                                       "object.hash" = db_qry[ xrow, "str_objhash"], 
                                                       "label" = utils::URLdecode( db_qry[ xrow, "str_label"] ), 
                                                       "actor" = db_qry[ xrow, "str_actor"], 
                                                       "env" = db_qry[ xrow, "str_env"],
                                                       "datetime" = as.POSIXct( db_qry[ xrow, "ts_datetime"], format = "%Y-%m-%d %H:%M:%S", tz = "UTC" ) ) )
      

    base::rm( list = c( "db_qry", "sql", "sql_whr" ) )
    
    
    
    return(invisible(recs))
})



.cxaudit_rdbstore$methods( "show" = function(x) {
  print(.self$.attr[["dbcon"]])
})
  