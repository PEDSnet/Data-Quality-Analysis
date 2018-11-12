###Perform operations on Databases###

.qual_tbl <- function(db, name, schema_tag) {
  if (! is.na(config(schema_tag))) {
    con <- if (inherits(db, 'src_dbi')) db$con else db
    name <- dbplyr::in_schema(config(schema_tag),
                              DBI::dbQuoteIdentifier(con, name))
  }
  tbl(db, name) #%>% show_query()
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
  if(db_has_table(req_env$db_src, table_name))
  {
    db_drop_table(req_env$db_src, table_name, force = FALSE)
  }
  dplyr::copy_to(req_env$db_src, db_tbl, name  = table_name, overwrite = FALSE)
  db_tbl<- tbl(req_env$db_src, table_name)
  
  return(db_tbl)
  
}


retrieve_dataframe_count<-function(table_df, column_list, distinction = F){
  if(distinction) table_df <- table_df %>% distinct_(column_list)
  counts  = table_df %>%
    select_(column_list) %>%
    na.omit() %>%
    tally() %>%
    as.data.frame()
  
  counts = as.integer(counts[1,1])
  test_that("Retrieve Dataframe Count Correct Length", expect_equal(length(counts), 1))
  return(counts)
}


retrieve_dataframe_record_count<-function(table_df)
{
   table_df = table_df %>%
       tally() %>%
       as.data.frame()
  test_that("Retrieve_dataframe_record_count does not have unique total row value", 
            expect_equal(length(table_df), 1))
  return(table_df)
}


retrieve_dataframe_count_group<-function(table_df, count_column, field_name){
  counts = table_df %>%
      rename_(count_column = count_column) %>%
      group_by_(field_name) %>%
      filter(!is.na(count_column)) %>%
      summarize(count = n_distinct(count_column)) %>%
      as.data.frame()
   test_that("Testing Retrieve Dataframe Count Group", 
             expect_equal(colnames(counts), c(field_name, "count")))
   return(counts)
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

retrieve_dataframe_top_5<-function(table_df, field_name){
  table_df <- table_df %>%
    filter_(paste("!is.na(", field_name, ")")) %>%
    group_by_(field_name) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    as.data.frame()

  l = min(nrow(table_df), 5)
  return(table_df[1:l,])
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


retrieve_dataframe_clause<-function(table_df ,column_list,clauses)
{
  table_df = table_df %>%
    filter_(clauses) 
  if(column_list[1] == "count(*)"){
      table_df <- table_df %>%
        tally() %>%
        as.data.frame()
  }
  else{
    table_df <- table_df %>%
      select_(.dots = column_list) %>%
      as.data.frame()
  }
  return(table_df)
}


retrieve_dataframe_ratio_group_join<-function(table_df, table_df2, num, den, group_by_field,join_field){
  table_df <- table_df %>%
    mutate_(var1 = num, var2 = den) %>% ###Need to avoid .x and .y columns
    inner_join(table_df2, by = join_field) %>%
    group_by_(group_by_field) %>%
    summarize(numer = n_distinct(var1),
              denom = n_distinct(var2)) %>%
    mutate(numer = as.double(numer)) %>%
    mutate(denom = as.double(denom)) %>%
    mutate(ratio = numer/denom) %>%
    select_(group_by_field, 'ratio') %>%
    as.data.frame() %>%
    mutate(ratio = round(ratio, 2)) 
  colnames(table_df)[2] = sprintf('%s_%s_ratio', num, den)
  return(table_df)
}

retrieve_dataframe_ratio_group<-function(table_df, num, den, group_by_field){
  table_df <- table_df %>%
    mutate_(var1 = num, var2 = den) %>%
    group_by_(group_by_field) %>%
    summarise(numer = n_distinct(var1),
              denom = n_distinct(var2)) %>%
    mutate(numer = as.double(numer)) %>%
    mutate(denom = as.double(denom)) %>%
    mutate(ratio = numer/denom) %>%
    select_(group_by_field, 'ratio') %>%
    as.data.frame() %>%
    mutate(ratio = round(ratio, 2)) 
  colnames(table_df)[2] = sprintf('%s_%s_ratio', num, den)
  return(table_df)
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


retrieve_dataframe_join_clause_group<-function(table_df, table_df2,join_field, 
                                               group_by_field, clauses){
  table_df <- table_df %>%
    inner_join(table_df2, by = setNames("concept_id", join_field)) %>%
    filter_(clauses) %>%
    group_by_(group_by_field) %>%
    summarise(counts = n()) %>%
    arrange(desc(counts)) %>%
    as.data.frame()
   
  return(table_df)
}


retrieve_dataframe_group_join<-function(table_df, table_df2, keep_fields,
                                        group_by_field,join_field)
{
  table_df <- table_df %>%
    inner_join(table_df2, by = join_field) %>%
    group_by_(group_by_field) %>%
    select_(keep_fields) %>%
    as.data.frame()
  return(table_df)
}

retrieve_dataframe_group <- function(table_df, field_name, distinct_field = NULL){
  if(!is.null(distinct_field)) table_df <- table_df %>% rename_(distinct_field = distinct_field)
  table_df = table_df %>%
    group_by_(field_name)

  if(!is.null(distinct_field)){
    table_df <- table_df %>%
      summarize(freq = n_distinct(distinct_field)) %>%
      as.data.frame()
  }
  else{
    table_df <- table_df %>%
      summarize(freq = n()) %>%
      as.data.frame()
  }
  table_df$freq = as.numeric(table_df$freq) 
  test_that("Testing that retrieve_dataframe_group has correct naming",
            expect_equal(colnames(table_df), c(field_name, "freq")))
  return(table_df)
}


retrieve_dataframe_group_clause <- function(table_df, field_name, clauses){
  counts_group = table_df %>%
    filter_(clauses) %>%
    group_by_(field_name) %>%
    summarize(count = n()) %>%
    mutate(count = as.numeric(count)) %>%
    as.data.frame()
  return(table_df)
}

get_vocabulary_name_by_concept_ids <- function (table_name, field_name, domain, table_df, table_df2)
{
  vocab_name = table_df %>%
    filter(domain_id %in% domain) %>%
    inner_join(table_df2 , by = c("concept_id" = field_name)) %>%
    select(vocabulary_id) %>%
    distinct() %>%
    collect()
  return(vocab_name)
}

get_concept_name <- function(table_df, df_concept_id){
  concept_name <- table_df %>%
    filter(concept_id == df_concept_id) %>%
    select(concept_name) %>% 
    collect()
  return(concept_name)
}

get_vocabulary_name <- function(table_df, df_concept_id){
  vocab_name <- table_df %>%
    filter(concept_id == df_concept_id) %>%
    select(vocabulary_id) %>%
    collect()
  return(vocab_name)
}

