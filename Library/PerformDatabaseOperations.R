.qual_tbl <- function(db, name, schema_tag) {
  if (! is.na(config(schema_tag))) {
    con <- if (inherits(db, 'src_dbi')) db$con else db
    name <- dbplyr::in_schema(config(schema_tag),
                              DBI::dbQuoteIdentifier(con, name))
  }
  #print(config(schema_tag))
  #print(DBI::dbQuoteIdentifier(con, name))
  #print(name)
  tbl(db, name) #%>% show_query()
  #print(name)
}



#' Connect to a CDM data table
#' @md
#'
#' @param db The database connection
#' @param name The name of the table
#'
#' @return A [dplyr::tbl()]] pointing to the table
cdm_tbl <-
  function(db, name)
    .qual_tbl(db, config('table_names')[[name]], 'cdm_schema')


#' Connect to a CDM vocabulary table
#' @md
#'
#' @param db The database connection
#' @param name The name of the table
#'
#' @return A [dplyr::tbl()]] pointing to the table
vocab_tbl <-
  function(db, name)
    .qual_tbl(db, config('table_names')[[name]], 'vocabulary_schema')


#' Connect to a dqa table
#' @md
#'
#' @param db The database connection
#' @param name The name of the table
#'
#' @return A [dplyr::tbl()]] pointing to the table
dqa_tbl <-
  function(db, name)
    .qual_tbl(db, config('table_names')[[name]], 'dqa_schema')

### write to a remote source 
create_database_copy<-function(db_tbl, table_name)
{
  print(table_name)
  if(db_has_table(req_env$db_src, table_name))
  {
    db_drop_table(req_env$db_src, table_name, force = FALSE)
    print(table_name)
  }
  print('here')
  print(table_name)
  dplyr::copy_to(req_env$db_src, db_tbl, name  = table_name, overwrite = FALSE)
  print(table_name)
  db_tbl<- tbl(req_env$db_src, table_name)
  
  return(db_tbl)
  
}


establish_database_connection_OHDSI<-function(config)
{
    library(DatabaseConnector);
    library(RJDBC);
    #jdbcDrivers<<-new.env();

    #initialize database connection parameters
    driver <- config$db$driver;

    dbname <- config$db$dbname;
    dbuser <- config$db$dbuser;
    dbpass <- config$db$dbpass;
    dbhost <- config$db$dbhost;
    dbport <- config$db$dbport;
    dbschema <- config$db$schema;


    if (driver == "sql server") #special handling for sql server
    {
      connectionDetails <- createConnectionDetails(dbms=tolower(driver), server=dbhost,user=dbuser,password=dbpass,schema=dbname,port=dbport)

    }
    else
    {
      if(tolower(driver)=="oracle")
      {
        connectionDetails <- createConnectionDetails(dbms=tolower(driver), 
                                                     server=paste(dbhost,"/",dbname,sep=""),
                                                     user=dbuser,password=dbpass,
                                                     schema=dbschema,port=dbport)
      }
      else {
      connectionDetails <- createConnectionDetails(dbms=tolower(driver), 
                                                   server=paste(dbhost,"/",dbname,sep=""),
                                                   user=dbuser,password=dbpass,
                                                   schema=dbschema,port=dbport
                                                   ,extraSettings="ssl=true&sslmode=verify-full")
      }
    }
        # flog.info(connectionDetails)
    con <- connect(connectionDetails)
        # flog.info(con)

    return(con)
}


close_database_connection_OHDSI <- function(con,config)
{
    #special handling for ODBC drivers
    disconnect(con)

}

retrieve_dataframe<-function(con,config,table_name)
{
    #special handling for ODBC drivers
    if (grepl(config$db$driver,"ODBC",ignore.case=TRUE))
    {
        table_name<-toupper(table_name)
        df<-sqlFetch(con, paste(config$db$schema, table_name, sep="."))
    }
    else
    {
      if (grepl(config$db$driver,"Oracle",ignore.case=TRUE))
      {
          table_name<-toupper(table_name)
          df<-querySql(con, table_name, schema = config$db$schema)
        }
        else
        {
          df<-querySql(con, c(config$db$schema,table_name))
        }
    }
    #converting all names to lower case for consistency
    names(df) <- tolower(names(df))
    return(df);

}


# retrieve counts
retrieve_dataframe_count<-function(con,config,table_name,column_list)
{

  #special handling for ODBC drivers
  if (grepl(config$db$driver,"ODBC",ignore.case=TRUE))
  {
    table_name<-toupper(table_name)
    column_list<-toupper(column_list)
    query<-paste("select count(",column_list,") from ",config$db$schema,".",table_name,sep="");
    df<-sqlQuery(con, query)
  }
  else
  {
    if (grepl(config$db$driver,"Oracle",ignore.case=TRUE))
    {
      table_name<-toupper(table_name)
      column_list<-toupper(column_list)
      query<-paste("select count(",column_list,") from ",config$db$schema,".",table_name,sep="");
      df<-querySql(con, query)
    }
    else
    {
      query<-paste("select count(",column_list,") from ",config$db$schema,".",table_name,sep="");
      df<-querySql(con, query)
    }
  }
  #converting all names to lower case for consistency
  names(df) <- tolower(names(df))
  return(df);

}

retrieve_dataframe_record_count<-function(con,config,table_name)
{
  
  #special handling for ODBC drivers
  if (grepl(config$db$driver,"ODBC",ignore.case=TRUE))
  {
    table_name<-toupper(table_name)
    #column_list<-toupper(column_list)
    query<-paste("select count(*) from ",config$db$schema,".",table_name,sep="");
    df<-sqlQuery(con, query)
  }
  else
  {
    if (grepl(config$db$driver,"Oracle",ignore.case=TRUE))
    {
      table_name<-toupper(table_name)
      #column_list<-toupper(column_list)
      query<-paste("select count(*) from ",config$db$schema,".",table_name,sep="");
      df<-querySql(con, query)
    }
    else
    {
      query<-paste("select count(*) from ",config$db$schema,".",table_name,sep="");
      df<-querySql(con, query)
    }
  }
  #converting all names to lower case for consistency
  names(df) <- tolower(names(df))
  return(df);
  
}

retrieve_dataframe_count_group<-function(con,config,table_name,column_list, field_name)
{

  #special handling for ODBC drivers
  if (grepl(config$db$driver,"ODBC",ignore.case=TRUE))
  {
    table_name<-toupper(table_name)
    column_list<-toupper(column_list)
    query<-paste("select ",field_name,", count(distinct ",column_list,") from ",config$db$schema,".",table_name," group by ",field_name,sep="");
    df<-sqlQuery(con, query)
  }
  else
  {
    if (grepl(config$db$driver,"Oracle",ignore.case=TRUE))
    {
      table_name<-toupper(table_name)
      column_list<-toupper(column_list)
      query<-paste("select ",field_name,", count(distinct ",column_list,") from ",config$db$schema,".",table_name," group by ",field_name,sep="");
      df<-querySql(con, query)
    }
    else
    {
      query<-paste("select ",field_name,", count(distinct ",column_list,") from ",config$db$schema,".",table_name," group by ",field_name,sep="");
      df<-querySql(con, query)
    }
  }
  #converting all names to lower case for consistency
  names(df) <- tolower(names(df))
  return(df);

}
# printing top 5 values
retrieve_dataframe_top_5<-function(con,config,table_name, field_name)
{

  #special handling for ODBC drivers
  if (grepl(config$db$driver,"ODBC",ignore.case=TRUE))
  {
    table_name<-toupper(table_name)
    query<-paste("select * from (select ",field_name,", count(*) as count from ",
                 config$db$schema,".",table_name, " where ",
                 field_name," is not null group by ",
                 field_name ," order by 2 desc) where rownum<=5"
                 ,sep="");
    df<-sqlQuery(con, query)
  }
  else
  {
    if (grepl(config$db$driver,"Oracle",ignore.case=TRUE))
    {
      table_name<-toupper(table_name)
      query<-paste("select * from (select ",field_name,", count(*) as count from ",
                   config$db$schema,".",table_name, " where ",
                   field_name," is not null group by ",
                   field_name ," order by 2 desc) where rownum<=5"
                   ,sep="");
      df<-querySql(con, query)
    }
    else
    {
      query<-paste("select ",field_name,", count(*) as count from ",config$db$schema,".",table_name," where ",field_name," is not null group by ",field_name
                   ," order by 2 desc limit 5"
                   ,sep="");
      df<-querySql(con, query)
    }
  }
  #converting all names to lower case for consistency
  names(df) <- tolower(names(df))
  return(df);

}

retrieve_dataframe_top_20_clause<-function(con,config,table_name, field_name,clause)
{

  #special handling for ODBC drivers
  if (grepl(config$db$driver,"ODBC",ignore.case=TRUE))
  {
    table_name<-toupper(table_name)
    query<-paste("select * from (select ",field_name,", count(*) as count from ",
                 config$db$schema,".",table_name, " where ",
                 clause," and ",field_name," is not null group by ",
                 field_name ," order by 2 desc) where rownum<=20"
                 ,sep="");
    df<-sqlQuery(con, query)
  }
  else
  {
    if (grepl(config$db$driver,"Oracle",ignore.case=TRUE))
    {
      table_name<-toupper(table_name)
      query<-paste("select * from (select ",field_name,", count(*) as count from ",
                   config$db$schema,".",table_name, " where ",
                   clause," and ",field_name," is not null group by ",
                   field_name ," order by 2 desc) where rownum<=20"
                   ,sep="");
      df<-querySql(con, query)
    }
    else
    {
      query<-paste("select ",field_name,", count(*) as count from ",config$db$schema,".",table_name,
                   " where ",clause," and ",field_name," is not null group by ",field_name
                   ," order by 2 desc limit 20"
                   ,sep="");
      df<-querySql(con, query)
    }
  }
  #converting all names to lower case for consistency
  names(df) <- tolower(names(df))
  return(df);

}
retrieve_dataframe_clause<-function(con,config,schema,table_name,column_list,clauses)
{

  #special handling for ODBC drivers
  if (grepl(config$db$driver,"ODBC",ignore.case=TRUE))
  {
    table_name<-toupper(table_name)
    column_list<-toupper(column_list)
    query<-paste("select ",column_list," from ",schema,".",table_name," where ",clauses,sep="");
    df<-sqlQuery(con, query)
  }
  else
  {
    if (grepl(config$db$driver,"Oracle",ignore.case=TRUE))
    {
      table_name<-toupper(table_name)
      column_list<-toupper(column_list)
      query<-paste("select ",column_list," from ",schema,".",table_name," where ",clauses,sep="");
      df<-querySql(con, query)
    }
    else
    {
      query<-paste("select ",column_list," from ",schema,".",table_name," where ",clauses,sep="");
      # flog.info(query)
      #print(query)
      df<-querySql(con, query)
    }
  }
  #converting all names to lower case for consistency
  names(df) <- tolower(names(df))
  return(df);

}

retrieve_dataframe_join_clause<-function(con,config,schema1,table_name1, schema2,table_name2,column_list,clauses)
{

  #special handling for ODBC drivers
  if (grepl(config$db$driver,"ODBC",ignore.case=TRUE))
  {
  table_name1<-toupper(table_name1)
  table_name2<-toupper(table_name2)
  column_list<-toupper(column_list)
  clauses<-toupper(clauses)
  query<-paste("select distinct ",column_list," from ",schema1,".",table_name1
               ,",",schema2,".",table_name2
               ," where ",clauses,sep="");
  df<-sqlQuery(con, query)
  }
  else
  {
    if (grepl(config$db$driver,"Oracle",ignore.case=TRUE))
    {
    table_name1<-toupper(table_name1)
    table_name2<-toupper(table_name2)
    column_list<-toupper(column_list)
    clauses<-toupper(clauses)
    query<-paste("select distinct ",column_list," from ",schema1,".",table_name1
                 ,",",schema2,".",table_name2
                 ," where ",clauses,sep="");
    df<-querySql(con, query)
  }
  else
  {
    query<-paste("select distinct ",column_list," from ",schema1,".",table_name1
                 ,",",schema2,".",table_name2
                 ," where ",clauses,sep="");
    # flog.info(query)
    df<-querySql(con, query)
  }
  }
  #converting all names to lower case for consistency
  names(df) <- tolower(names(df))
  return(df);
}

retrieve_dataframe_join_clause_group<-function(con,config,schema1,table_name1, schema2,table_name2,column_list,clauses)
{

  #special handling for ODBC drivers
  if (grepl(config$db$driver,"ODBC",ignore.case=TRUE))
  {
    table_name1<-toupper(table_name1)
    table_name2<-toupper(table_name2)
    column_list<-toupper(column_list)
    #clauses<-toupper(clauses)
    query<-paste("select ",column_list,", count(*) as count from ",schema1,".",table_name1
                 ,",",schema2,".",table_name2
                 ," where ",clauses
                 ," group by ",column_list
                 ,sep="");
    df<-sqlQuery(con, query)
  }
  else
  {
    if (grepl(config$db$driver,"Oracle",ignore.case=TRUE))
    {
      table_name1<-toupper(table_name1)
      table_name2<-toupper(table_name2)
      column_list<-toupper(column_list)
      #clauses<-toupper(clauses)
      query<-paste("select ",column_list,", count(*) as count from ",schema1,".",table_name1
                   ,",",schema2,".",table_name2
                   ," where ",clauses
                   ," group by ",column_list
                   ,sep="");
      df<-querySql(con, query)
    }
    else
    {
      query<-paste("select ",column_list,", count(*) as count from ",schema1,".",table_name1
                   ,",",schema2,".",table_name2
                   ," where ",clauses
                   ," group by ",column_list
                   ," order by 2 desc"
                   ,sep="");
      # flog.info(query)
      df<-querySql(con, query)
    }
  }
  #converting all names to lower case for consistency
  names(df) <- tolower(names(df))
  return(df);
}

retrieve_dataframe_group<-function(con,config,table_name,field_name)
{

  #special handling for ODBC drivers
  if (grepl(config$db$driver,"ODBC",ignore.case=TRUE))
  {
    table_name<-toupper(table_name)
    field_name<-toupper(field_name)
    query<-paste("select ",field_name,", count(*) as Freq from ",config$db$schema,".",table_name," group by ",field_name,sep="");
    df<-sqlQuery(con, query)
  }
  else
  {
    if (grepl(config$db$driver,"Oracle",ignore.case=TRUE))
    {
      table_name<-toupper(table_name)
      field_name<-toupper(field_name)
      query<-paste("select ",field_name,", count(*) as Freq from ",config$db$schema,".",table_name," group by ",field_name,sep="");
      df<-querySql(con, query)
    }
    else
    {
      query<-paste("select ",field_name,", count(*) as Freq from ",config$db$schema,".",table_name," group by ",field_name,sep="");
      #print(query)
      #print(con) 
      querySql(con, query)
      #print('crossed1')
      #print(querySql(con, query))
      df<-querySql(con, query)
      #print(df)
      #print('crossed2')
      #print(query)
    }
  }
  #converting all names to lower case for consistency
  names(df) <- tolower(names(df))
  print(names(df))
  return(df);

}
retrieve_dataframe_group_clause<-function(con,config,table_name,field_name, clauses)
{

  #special handling for ODBC drivers
  if (grepl(config$db$driver,"ODBC",ignore.case=TRUE))
  {
    table_name<-toupper(table_name)
    field_name<-toupper(field_name)
    query<-paste("select ",field_name,", count(*) as Freq from ",config$db$schema,".",table_name," where ",clauses," group by ",field_name,sep="");
    df<-sqlQuery(con, query)
  }
  else
  {
    if (grepl(config$db$driver,"Oracle",ignore.case=TRUE))
    {
      table_name<-toupper(table_name)
      field_name<-toupper(field_name)
      query<-paste("select ",field_name,", count(*) as Freq from ",config$db$schema,".",table_name," where ",clauses," group by ",field_name,sep="");
      df<-querySql(con, query)
    }
    else
    {
      query<-paste("select ",field_name,", count(*) as Freq from ",config$db$schema,".",table_name," where ",clauses," group by ",field_name,sep="");
      df<-querySql(con, query)
    }
  }
  #converting all names to lower case for consistency
  names(df) <- tolower(names(df))
  return(df);

}
retrieve_dataframe_ratio_group<-function(con,config,table_name,column_list, field_name)
  {

        #special handling for ODBC drivers
  if (grepl(config$db$driver,"ODBC",ignore.case=TRUE))
  {
              table_name<-toupper(table_name)
              column_list<-toupper(column_list)
              query<-paste("select ",field_name,", ",column_list," from ",config$db$schema,".",table_name," group by ",field_name,sep="");
              df<-sqlQuery(con, query)
            }
    else
       {
         if (grepl(config$db$driver,"Oracle",ignore.case=TRUE))
         {
                  table_name<-toupper(table_name)
                  column_list<-toupper(column_list)
                  query<-paste("select ",field_name,", ",column_list," from ",config$db$schema,".",table_name," group by ",field_name,sep="");
                  df<-querySql(con, query)
               }
            else
             {
                  query<-paste("select ",field_name,",",column_list," from ",config$db$schema,".",table_name," group by ",field_name,sep="");
                  df<-querySql(con, query)
                }
         }
     #converting all names to lower case for consistency
        names(df) <- tolower(names(df))
      return(df);

      }
retrieve_dataframe_ratio_group_join<-function(con,config,table_name_1, table_name_2,ratio_formula, group_by_field,join_field)
  {
       #special handling for ODBC drivers
  if (grepl(config$db$driver,"ODBC",ignore.case=TRUE))
  {
              table_name_1<-toupper(table_name_1)
              table_name_2<-toupper(table_name_2)
              ratio_formula<-toupper(ratio_formula)
              group_by_field<-toupper(group_by_field)
              join_field<-toupper(join_field)
              query<-paste("select ",group_by_field,", ",ratio_formula," from ",config$db$schema,".",table_name_1,",",config$db$schema,".",table_name_2,
                                         " where ",table_name_1,".",join_field,"=",table_name_2,".",join_field,
                                       " group by ",group_by_field,sep="");
          df<-sqlQuery(con, query)
            }
     else
        {
          if (grepl(config$db$driver,"Oracle",ignore.case=TRUE))
          {
                table_name_1<-toupper(table_name_1)
                  table_name_2<-toupper(table_name_2)
                  ratio_formula<-toupper(ratio_formula)
                  group_by_field<-toupper(group_by_field)
                  join_field<-toupper(join_field)
                  query<-paste("select ",group_by_field,", ",ratio_formula," from ",config$db$schema,".",table_name_1,",",config$db$schema,".",table_name_2,
                                                 " where ",table_name_1,".",join_field,"=",table_name_2,".",join_field,
                                                   " group by ",group_by_field,sep="");
            df<-querySql(con, query)
                }
           else
              {
                  query<-paste("select ",group_by_field,", ",ratio_formula," from ",config$db$schema,".",table_name_1,",",config$db$schema,".",table_name_2,
                                                   " where ",table_name_1,".",join_field,"=",table_name_2,".",join_field,
                                                  " group by ",group_by_field,sep="");
                  df<-querySql(con, query)
                }
          }
     #converting all names to lower case for consistency
        names(df) <- tolower(names(df))
      return(df);

      }
retrieve_dataframe_OHDSI<-function(con,config,table_name)
{
    df<-querySql(con,paste("SELECT * FROM ",config$db$schema,".",table_name,sep=""))
    #df <- as.ram(data)
    #converting all names to lower case for consistency
    names(df) <- tolower(names(df))
    return(df);
}

# for cases where all values in a field belong to one vocab.
get_vocabulary_name_by_concept_code <- function (concept_code,con, config)
{
  #return(df_vocabulary_name[1][1])

  #concept_code<-gsub("^\\s+|\\s+$", "",concept_code)
  concept_code<-trim(unlist(strsplit(concept_code,"\\|"))[1])
   flog.info(concept_code)
  df_vocabulary_name<-retrieve_dataframe_clause(con,config,config$db$vocab_schema,"concept","vocabulary_id",paste("CONCEPT_CODE in ('",concept_code,"')",sep=""))

  final_vocabulary_name<-""
  for (row_num in 1:nrow(df_vocabulary_name))
  {
    final_vocabulary_name<-paste(final_vocabulary_name,df_vocabulary_name[row_num,1],sep="")
  }
  return(final_vocabulary_name)
}
# for cases where values in a field may be drawn from multiple vocabularies, e.g. procedure source value
get_vocabulary_name_by_concept_codes <- function (con,config, schema1,table_name, field_name, schema2,domain)
{
  #return(df_vocabulary_name[1][1])

  #concept_code<-gsub("^\\s+|\\s+$", "",concept_code)
  #concept_code<-trim(unlist(strsplit(concept_code,"\\|"))[1])
  # flog.info(concept_code)
  df_vocabulary_name<-retrieve_dataframe_join_clause(con,config,schema1,table_name,schema2,"concept","vocabulary_id",
                                                     paste(field_name,"= concept_code and upper(domain_id) =upper('",domain,"')",sep="")
                                                     )

  final_vocabulary_name<-""
  for (row_num in 1:nrow(df_vocabulary_name))
  {
    final_vocabulary_name<-paste(final_vocabulary_name,df_vocabulary_name[row_num,1],"|",sep="")
  }
  return(final_vocabulary_name)
}
#returns a list
get_vocabulary_name_by_concept_ids <- function (con, config, table_name, field_name, domain)
{
  df_vocabulary_name<-retrieve_dataframe_join_clause(con,config,config$db$schema,table_name,config$db$vocab_schema,"concept","vocabulary_id",
                                                     paste(field_name,"= concept_id and upper(domain_id) =upper('",domain,"')",sep="")
  )
  return(df_vocabulary_name$vocabulary_id)
}
get_vocabulary_name <- function (concept_id,con, config)
{
  df_vocabulary_name<-retrieve_dataframe_clause(con,config,config$db$vocab_schema,"concept","vocabulary_id",paste("CONCEPT_ID in (",concept_id,")"))
  return(df_vocabulary_name[1][1])
}
get_concept_name <- function (concept_id,con, config)
{
  df_concept_name<-retrieve_dataframe_clause(con,config,config$db$vocab_schema,"concept","concept_name",paste("CONCEPT_ID in (",concept_id,")"))
  return(df_concept_name[1][1])
}

get_concept_name_by_concept_code <- function (concept_code,con, config)
{
  concept_code<-gsub("^\\s+|\\s+$", "",concept_code)
  df_concept_name<-retrieve_dataframe_clause(con,config,config$db$vocab_schema,"concept","concept_name",paste("CONCEPT_CODE in ('",concept_code,"')",sep=""))

  # flog.info(class(df_concept_name))
  # flog.info(df_concept_name)
  # flog.info(dim(df_concept_name))

  # there could be multiple concepts sharing the same concept code
  final_concept_name<-""
  for (row_num in 1:nrow(df_concept_name))
  {
    # flog.info(row_num)
    # flog.info(df_concept_name[1,1])
    # flog.info(nrow(df_concept_name))
    final_concept_name<-paste(final_concept_name,df_concept_name[row_num,1],"|",sep="")
  }
  return(final_concept_name)
}
