--
--  the simplest database schema for auditor on SQLite 
--
--



-- main record table

create table if not exists tbl_adt_records (
   uid             uuid not null primary key,
   str_event       varchar(50) not null,
   str_objtype     varchar(100) not null,
   str_objclass    varchar(100) not null,
   str_objname     varchar(100) not null,
   str_objbin      varchar(4096) not null,
   str_objhash     varchar(128) not null,
   str_label       varchar(1024) not null,
   str_actor       varchar(512) not null,
   str_env         varchar(512) not null,
   dt_date         date not null default current_date, 
   ts_datetime     timestamp not null default current_timestamp  
);


--  main records block record indexes 
--  note: poor mans BRIN implementation
--  note: used in conjunction with where clauses to quickly subset records before costly traversals

create index if not exists bridx_adt_records_date on tbl_adt_records ( dt_date ) ;
create index if not exists bridx_adt_records_objbin on tbl_adt_records ( str_objbin ) ;




-- attributes associated with the main record

create table if not exists tbl_adt_record_attrs (
   uid_rec	  uuid not null references tbl_adt_records(uid),
   str_key    varchar(100) not null,
   str_qual   varchar(100) not null default 'value',
   str_value  varchar(1024),
   int_vseq   int default 0
);



-- crude approach to linking audit records

create table if not exists tbl_adt_commits (
   uid             uuid not null,
   uid_rec         uuid not null references tbl_adt_records(uid),
   ts_datetime     timestamp not null default current_timestamp  
);




-- working copy of main record table

create table if not exists tbl_adt_wrkrecords (
   uid             uuid not null primary key,
   str_event       varchar(50) not null,
   str_objtype        varchar(100) not null,
   str_objclass       varchar(100) not null,
   str_objname        varchar(100) not null,
   str_objbin         varchar(4096) not null,
   str_objhash        varchar(128) not null,
   str_label       varchar(512) not null,
   str_actor       varchar(512) not null,
   str_env         varchar(512) not null,
   dt_date         date not null default current_date,
   ts_datetime     timestamp not null default current_timestamp  
);



-- working copy of attributes associated with the main work record

create table if not exists tbl_adt_wrkrecord_attrs (
   uid_rec	  uuid not null references tbl_adt_wrkrecords(uid),
   str_key    varchar(100) not null,
   str_qual   varchar(100) not null default 'value',
   str_value  varchar(1024),
   int_vseq   int default 0
);



