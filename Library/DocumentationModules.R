trim <- function (x) gsub("^\\s+|\\s+$", "", x)

strEndsWith <- function(haystack, needle)
{
  hl <- nchar(haystack)
  nl <- nchar(needle)
  if(nl>hl)
  {
    return(F)
  } else
  {
    return(substr(haystack, hl-nl+1, hl) == needle)
  }
}

create_meaningful_message_concept_id<-function(message,field_name,con,config)
{
  # store concept identifiers in a list
  concept_id_list <- unlist(strsplit(unlist(strsplit(message,":"))[2],","))
  in_clause <- paste("(",concept_id_list[1],
                   ",",concept_id_list[2],
                   ",",concept_id_list[3],
                   ",",concept_id_list[4],
                   ",",concept_id_list[5],
                   ")",sep="")
  #df_concept_id_names<-retrieve_dataframe_clause(con,config,"concept","concept_id,concept_name",paste("CONCEPT_ID in",in_clause))
  new_message <- paste("The most frequent values for",field_name,"are:")
  for(i in 1:5)
  {
    # flog.info(concept_id_list[i])
    # flog.info(new_message)
    if(unlist(strsplit(concept_id_list[i],"\\|count="))[1]=="\n NA ")
      break;

    new_message <- paste(new_message,unlist(strsplit(concept_id_list[i],"\\|count="))[1],
                         "(",get_concept_name(unlist(strsplit(concept_id_list[i],"\\|count="))[1],con, config),")"
                         ,unlist(strsplit(concept_id_list[i],"\\|"))[2]
                         ,sep="")
    if(i<5)
      new_message<- paste(new_message,",\n")
  }
    return(new_message)
}

create_meaningful_message_concept_code<-function(message,field_name,con,config)
{
  # store concept codes in a list
  concept_code_list <- unlist(strsplit(unlist(strsplit(message,":"))[2],","))
  in_clause <- paste("(",concept_code_list[1],
                     ",",concept_code_list[2],
                     ",",concept_code_list[3],
                     ",",concept_code_list[4],
                     ",",concept_code_list[5],
                     ")",sep="")
  #df_concept_id_names<-retrieve_dataframe_clause(con,config,"concept","concept_code,concept_name",paste("CONCEPT_CODE in",in_clause))
  new_message <- paste("The most frequent values for",field_name,"are:")
  for(i in 1:5)
  {

    new_message <- paste(new_message,unlist(strsplit(concept_code_list[i],"\\|count="))[1],
                         "(",get_concept_name_by_concept_code(unlist(strsplit(concept_code_list[i],"\\|count="))[1],con, config),")"
                         ,unlist(strsplit(concept_code_list[i],"\\|"))[2]
                         )
    # flog.info(new_message)
    if(i<5)
      new_message<- paste(new_message,",\n")
  }
  return(new_message)
}

extract_numeric_value<-function(test_message)
{
  return(
    as.numeric(
      trim(unlist(strsplit(trim(unlist(strsplit(test_message," is"))[2]),"%"))[1])
    )
  )
}

extract_start_range<-function(test_message)
{
  return(
    as.numeric(
      trim(unlist(strsplit(trim(unlist(strsplit(test_message,"range:"))[3]),"-"))[1])
    )
  )
}
extract_ni_missing_percent<-function(test_message)
{
  return(
    as.numeric(
      trim(unlist(strsplit(trim(unlist(strsplit(test_message," is"))[4]),"%"))[1])
    )
  )
}

extract_maximum_value<-function(test_message)
{
  return(
    as.numeric(
      trim(unlist(strsplit(trim(unlist(strsplit(test_message,"Maximum:"))[2]),"Minimum"))[1])
    )
  )
}

print_2d_dataframe<-function(df_table)
{
  message<-""
  index<-1;
  while (index<=nrow(df_table))
  {
    message<-c(message,
               paste(df_table[index,1],"(count=",
                     df_table[index,2],")\n"));
    index<-index+1;
  }
  return(message)
}

paste_image_name<-function(table_name,field_name)
{
    return(paste("![",field_name,"](.",get_image_name(table_name,field_name),")\n",sep=""))
}
get_image_name<-function(table_name,field_name)
{
    return(paste("./images/RPlot_",table_name,"_",field_name,".png",sep=""))

}

get_image_name_sorted<-function(table_name,field_name)
{
    return(paste("./images/RPlot_",table_name,"_",field_name,"_sorted.png",sep=""))

}

paste_image_name_sorted<-function(table_name,field_name)
{
    return(paste("![",field_name,"](.",get_image_name_sorted(table_name,field_name),")\n",sep=""))

}

get_report_header<-function(table_name,config)
{
    fileContent <-paste("#DQA Report for ",table_name," in the ", g_config$reporting$cdm," CDM at ",g_config$reporting$site)
    fileContent<-c(fileContent,paste("### Date:",Sys.Date()," * ETL script version: ",g_config$reporting$etl_script_version," * DQA script version: ",config$reporting$dqa_script_version))
    return(fileContent);
}

#if the parameter value does not end with a "/", append with "/"
normalize_directory_path<-function(directory)
{
  if (directory[length(directory)] != "/")
    directory <- paste(directory,"/",sep="")
  return(directory);
}

convert_to_comma_list<-function(df_visit)
{
  result<-"("
  for(i in 1:nrow(df_visit))
  {
    if(i==1)
    {
      result<-paste(result,df_visit[i,1])
    } else
    {
      result<-paste(result,",",df_visit[i,1])
    }
  }
  result<-paste(result,")")
  return(result)
}

get_previous_cycle_total_count<-function(site_name, table_name)
{
   flog.info(getwd())
  df_total_counts<-read.csv(g_total_counts_path, header = TRUE, sep = ",", quote = "\"",
                          dec = ".", fill = TRUE, comment.char = "")
  # flog.info(df_total_counts)
  column_index<-which(colnames(df_total_counts)==table_name)
  for(row_index in 1:nrow(df_total_counts))
  {
    if(df_total_counts[row_index,1]==site_name)
      return(df_total_counts[row_index, column_index])
  }
 return(0);
}
get_previous_cycle_total_fact_type_count<-function(site_name, col_name)
{
  #flog.info(getwd())
  df_total_counts<-read.csv(g_total_fact_type_counts_path, header = TRUE, sep = ",", quote = "\"",
                            dec = ".", fill = TRUE, comment.char = "")
  # flog.info(df_total_counts)
  column_index<-which(colnames(df_total_counts)==col_name)
  for(row_index in 1:nrow(df_total_counts))
  {
    if(df_total_counts[row_index,1]==site_name)
      return(df_total_counts[row_index, column_index])
  }
  return(0);
}
get_percentage_diff<-function(prev_total_count, current_total_count)
{
  return(round((current_total_count-prev_total_count)*100/prev_total_count,2))
}
get_percentage_diff_message<-function(percentage_diff)
{
  if(percentage_diff>0)
    return(paste("(increased by ",percentage_diff,"%","over the previous cycle)\n"));
  if(percentage_diff==0)
    return(paste("(no change with respect to the previous cycle)\n"));
  if(percentage_diff<0)
    return(paste("(decreased by ",abs(percentage_diff),"%","over the previous cycle)\n"));

}

write_total_counts<-function(table_name, current_total_count)
{
  total_counts_filename<-paste(normalize_directory_path(g_config$reporting$site_directory),
                               "./data/total_counts.csv",sep="")
  total_count_df<-read.csv(total_counts_filename)
  total_count_df$site<-as.character(total_count_df$site)
  
  #colnames(total_count_df)<-c("table","counts")
  total_count_df<-cbind(total_count_df, c(current_total_count))
  #print(total_count_df)
  colnames(total_count_df)[ncol(total_count_df)]<-table_name
  write.csv(total_count_df,file=total_counts_filename, row.names = FALSE)
  
}
write_total_fact_type_counts<-function(table_name, fact_type,current_total_count)
{
  if(length(current_total_count)==0)
    current_total_count<-0
  total_counts_filename<-paste(normalize_directory_path(g_config$reporting$site_directory),
                               "./data/total_fact_type_counts.csv",sep="")
  total_count_df<-read.csv(total_counts_filename)
  total_count_df$site<-as.character(total_count_df$site)
  
  #colnames(total_count_df)<-c("table","counts")
  total_count_df<-cbind(total_count_df, c(current_total_count))
  #print(total_count_df)
  colnames(total_count_df)[ncol(total_count_df)]<-paste0(table_name, ".",fact_type)
  write.csv(total_count_df,file=total_counts_filename, row.names = FALSE)
  
}