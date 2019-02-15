#primary key field
#functionName: reportTotalCount
#Description: generate the total number of unique values for the primary key field of a given table
#Input:an R dataframe containing a given database table
#Output: the total number of records in the table, or the total number of unique values of the primary key identifier
#reportTotalCount<-function(df_table)
#{
 # assuming the first field is the primary key field
 #return(length(unique(df_table[,1])))

#}

describeIdentifier<-function(table_df, field_name)
{
  total_unique <- table_df %>% 
    select_(field_name) %>%
    distinct() %>%
    tally() %>%
    mutate(total = as.numeric(n)) %>%
    select(total) %>%
    as.data.frame()
  
  if(total_unique == 1){
      if(is.na(total_unique)){ return(0);}}
	return (total_unique);
}


#functionName: reportMissingCount
#Description: count the number of records with no value for a given field
#Inputs:an R dataframe containing a given database table, the name of the field
#Output: number of rows with NA (missing) values for the input field
# reportMissingCount<-function(df_table,table_name,field_name)
reportMissingCount<-function(table_df, table_name, field_name, group_ret = 0){

  if(group_ret){
    total <- table_df %>%
      mutate(total = sum(freq)) %>%
      select(total) %>%
      distinct() %>%
      collect()
    missn <- table_df %>%
      mutate_(miss_field = field_name) %>%
      filter(is.na(miss_field)) %>%
      select(freq) %>%
      collect()
    if(nrow(missn) == 0) missn = 0
  }
  else{
    total <- table_df %>%
      select_(field_name) %>%
      summarize(n = n()) %>%
      collect()
    missn <- table_df %>%
      collect() %>%
      select_(field_name) %>%
      na.omit() %>%
      summarize(n = n()) %>%
      collect() 
    ##Switch from rate present to missing rate
    missn = total - missn 
  }

  if(missn > 0){
    label <- as.character(paste0(round(100*missn/total,digits=2), "%"))
  
    return(paste("\n\nPercentage of",table_name,"with missing values for ",field_name," is ",label));
  }
  else
    return(paste("\n\nPercentage of",table_name,"with missing values for ",field_name," is 0%"));
}


reportNullFlavors<-function(table_df,table_name,field_name,UN_code,OT_code,NI_code){
  table_df <- table_df %>%
    group_by_(field_name) %>%
    dplyr::summarize(freq = n()) %>%
    as.data.frame() %>%
    na.omit() 
  table_df[,2] <- as.numeric(table_df[,2])
  colnames(table_df) <- c("Var1", "Freq")

  if(nrow(table_df)>0)
  {
    table_df$Var1 <- as.factor(table_df$Var1)
    #Make percentage
    table_df$label <- as.character(paste(round(100 * table_df[,2]/sum(table_df[,2]),digits=2),'%'))  
    count_ni<-subset(table_df,Var1==NI_code)$label[1]
    if(is.na(count_ni)) count_ni<-"0%";
    count_un<-subset(table_df,Var1==UN_code)$label[1]
    if(is.na(count_un)) count_un<-"0%";
    count_ot<-subset(table_df,Var1==OT_code)$label[1]
    if(is.na(count_ot)) count_ot<-"0%";
    count_missing_values<-subset(table_df,is.na(Var1))$label[1]
    if(is.na(count_missing_values)) count_missing_values<-"0%";

    return(paste(
      "\nPercentage of",table_name,"with unknown value for ",field_name," is ",count_un,"\n",
      "\nPercentage of",table_name,"with other value for ",field_name," is ",count_ot,"\n",
      "\nPercentage of",table_name,"with no information for ",field_name," is ",count_ni,"\n",
      "\nPercentage of",table_name,"with missing values for ",field_name," is ",count_missing_values,"\n"));
  } 
  else{
    return(paste(
      "\nPercentage of",table_name,"with unknown value for ",field_name," is 0%\n",
      "\nPercentage of",table_name,"with other value for ",field_name," 0%\n",
      "\nPercentage of",table_name,"with no information for ",field_name," is 0%\n",
      "\nPercentage of",table_name,"with missing values for ",field_name," is 100%\n"));
  }
}

reportUnexpected<-function(table_df,field_name,permissible_values){
  table_df <- table_df %>%
    select_(field_name) %>%
    distinct() %>% 
    as.data.frame() %>%
    na.omit() 
  
  test_that("Testing Report Unexpected selects one column", expect_equal(ncol(table_df), 1))
  table_df <- table_df[!is.element(table_df[,1], permissible_values),1]
  return_message<-""
  return_message <- paste(return_message, table_df, sep = "invalid_value_found:", collapse = ";")
  return(return_message)
}

# Nominal Fields
#functionName: describeNominalField
#Description: generate a barplot for a nominal field
#Inputs:
	#1. an R dataframe containing a given database table
	#2. the name of the nominal field
	#3. label_bin: pre-defined labels for various bins
	#4. order_bins: a fixed order for various bins on the plot
	#5: color_bins: colors assigned to each bin
#Output: write the barplot to a file
describeNominalField<-function(table_df, table_name,field_name, group_ret = 1){
  flog.info(paste("Plotting for Field: ", field_name))
  ###Check for retrieve_dataframe_group_structure
  if(group_ret){
    table_df <- table_df %>% na.omit()
  }
  else{
  table_df <- table_df %>%
    group_by_(field_name) %>%
    dplyr::summarize(freq = n()) %>%
    as.data.frame() %>%
    na.omit() 
  table_df[,2] <- as.numeric(table_df[,2])
  }

  if(nrow(table_df) > 0){
    colnames(table_df) <- c("Var1", "Freq")
    table_df$Var1 <- as.factor(table_df$Var1)
    table_df$label <- as.character(paste0(round(100 * table_df[,2] / sum(table_df[,2]),digits=2), '%'))
    #creating barplot from dfTab dataframe
    p<-ggplot(table_df, aes(x = Var1, y = Freq, fill = Var1)) + geom_bar(stat = "identity") + ggtitle(paste(field_name,": Distribution"))
    # add axis labels
    p<-p+ylab(paste(table_name,"Count"))+xlab(field_name)
    #remove legend and set size and orientation of tick labels
    p<-p+theme(legend.position="none", text = element_text(size=10),
               axis.text.x = element_text(angle=90, vjust=1))
    # add the label to each bar (from the dfTab dataframe)
    p<-p+geom_text(data= table_df, aes(x=Var1,y=Freq,label=label), size=3)
    #save the barplot image (will be referenced by the final report)
    if(nrow(table_df)==1 & is.na(table_df[1,1])){ ### dont print the graph
      }
    else {
     ggsave(file=paste(normalize_directory_path( g_config$reporting$site_directory),
                       get_image_name(table_name,field_name),sep=""))
    }
  }
}


# Ordinal Fields
#functionName: describeOrdinalField
#Description: generate a barplot for an ordinal field
#Inputs:
	#1. an R dataframe containing a given database table
	#2. the name of the ordinal field
#Output: write the barplot to a file
##ggplotting, set to false for large datasets to avoid ggplot
describeOrdinalField<-function(table_df, table_name,field_name, group_ret = 1, ggplotting = T)
{
  flog.info(paste("Plotting for Field: ", field_name))
  ###Check for retrieve_dataframe_group_structure
  if(group_ret){
    table_df <- table_df %>% na.omit()
  }
  else{
  table_df = table_df %>% 
    group_by_(field_name) %>%
    dplyr::summarize(freq = n()) %>%
    as.data.frame() %>%
    na.omit() 
  }

  if(nrow(table_df) > 0){
      if(ggplotting){
      colnames(table_df) <- c("Var1", "Freq")
      table_df[,2] <- as.numeric(table_df[,2])
      #create a bar plot
      p<-ggplot(table_df, aes(x = Var1, y = Freq, fill = Var1)) + 
        geom_bar(stat = "identity") + ggtitle(paste(field_name,": Distribution"))
      # add axis labels
      p<-p+ylab(paste(table_name,"Count"))+xlab(field_name)
      #remove legend and set size and orientation of tick labels
      p<-p+theme(legend.position="none", text = element_text(size=6),
                 axis.text.x = element_text(angle=90, vjust=1), plot.background = element_blank() ,
                 panel.grid.major = element_blank() ,panel.grid.minor = element_blank() ,
                 panel.border = element_blank())
       ggsave(file=paste(normalize_directory_path( g_config$reporting$site_directory),
                         get_image_name(table_name,field_name),sep=""))
      }
      else{
      # counting the total number of unique values
      total_locations <- nrow(table_df)
      
      png(paste(normalize_directory_path( g_config$reporting$site_directory),
                get_image_name(table_name,field_name),sep=""))
      # not using ggplot here as it is very expensive for a large number of values
      barplot(as.numeric(table_df[,2]), names.arg = table_df[,1],
              main = paste(field_name,": Distribution"), 
              xlab = paste(field_name,"(Total: ",total_locations,")"), 
              ylab = paste(table_name,"Count"))
      }
    
    table_df<-table_df[order(table_df[,2], decreasing = TRUE),] 
    return_message<- paste("The most frequent values for",field_name,"are:","\n")
    minr <- min(nrow(table_df), 5)
    for(index in 1:minr){
        return_message<-paste(return_message,table_df[index,1], "|count=",table_df[index,2])
        if(index < minr){ return_message<-paste(return_message,",\n")}
    }
    if(!is.null(dev.list())) dev.off()
    return(return_message)
   }
}

#Date Function Check
describeDateField<-function(table_df, table_name, field_name, group_ret = 1, datetime = 0){
  flog.info(paste("Plotting for Field: ", field_name))
  ###Check for retrieve_dataframe_group_structure
  if(group_ret){
    table_df <- table_df %>% na.omit()
    table_df[,1] <- as.Date(table_df[,1])
    ###Aggregate datetimes to not treat different times as unique dates
    if(datetime & nrow(table_df) > 1){ 
      table_df <- aggregate(reformulate(termlabels = field_name,response = "freq"),
                                        table_df,FUN = sum)}
    date_max = max(table_df[,1], na.rm = T)
    date_min = min(table_df[,1], na.rm = T)
  }
  else{
  table_df <- table_df %>%
    na.omit() %>%
    group_by_(field_name) %>%
    dplyr::summarize(freq = n()) %>%
    as.data.frame()
  table_df[,2] <- as.numeric(table_df[,2])
  table_df[,1] = as.Date(table_df[,1])
  date_max <- max(table_df[,1], na.rm = T)
  date_min <- min(table_df[,1], na.rm = T)
  }

  if(nrow(table_df)>0){

    date_range <- paste("\n Date range: ",
                      date_min,"-", date_max)
    total_locations <- nrow(table_df)

    png(paste(normalize_directory_path( g_config$reporting$site_directory),
              get_image_name(table_name,field_name),sep=""))

    # not using ggplot here as it is very expensive for a large number of values
    barplot(table_df[order(table_df[,1], decreasing = F),2], names.arg = table_df[,1], 
              main = paste(field_name,": Distribution"),
              xlab = paste(field_name,"(Total: ",total_locations,")"), 
              ylab = paste(table_name,"Count"))

    table_df <- as.data.frame(table_df)
    table_df <- table_df[order(table_df[,2], decreasing = T),]

    return_message<-paste("The most frequent values for",field_name,"are:")
    for (index in 1:5)
    {
      return_message<-paste(return_message,(table_df[index,1]))
      if(index<5){return_message<-paste(return_message,",")}
    }

    return_message<-c(return_message, date_range)
    # check if future dates are included
    if(Sys.Date() < date_max)
      return_message<-c(return_message,"\nWARNING: includes future dates")
    
    if(!is.null(dev.list())) dev.off()
    return(return_message)  
  }
}


describeYYMMField<-function(df_table, table_name,field_name,fact_type)
{
    df_table<-subset(df_table,!is.na(df_table[,1]))
    if(nrow(df_table)>0)
    {
      # df table is actually a dataframe of two dataframes
      colnames(df_table)[1] <- "Var1"
      colnames(df_table)[2] <- "Freq"
      df_table$Freq<- as.numeric(df_table$Freq)
      total_values<- nrow(df_table)
      ggplot(data=df_table, aes(x=Var1, y=Freq, group=1)) + geom_line() + 
        xlab(field_name)+ ylab('counts') + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      
      if(is.null(fact_type)) 
      ggsave(file=paste(normalize_directory_path( g_config$reporting$site_directory),
                        get_image_name(table_name,paste0(field_name, "-yyyy-mm")),sep=""))
      else
        ggsave(file=paste(normalize_directory_path( g_config$reporting$site_directory),
                          get_image_name(table_name,paste0(field_name, "-yyyy-mm-", fact_type[2])),sep=""))
       
    }
}

###Note may fail to capture if events occur at the exact same time
###Original plot only looked at time, not at the associated date.
###This creates many more instances and taller bar graphs.
###Cannot be generated in same way as date field above.
###But still almost identical to non-dplyr version.
describeTimeField<-function(table_df, table_name,field_name){     
  flog.info(paste("Plotting for Field: ", field_name))
  table_df <- table_df %>%
    group_by_(field_name) %>%
    dplyr::summarize(freq = n()) %>%
    as.data.frame() %>%
    na.omit() 
  table_df[,1] <- as.character(table_df[,1])
  table_df[,2] <- as.numeric(table_df[,2])

  table_df <- substr(table_df[,1],12,19)
  time_max <- max(table_df, na.rm = T)
  time_min <- min(table_df, na.rm = T)
  table_df <- table(table_df)
  if(nrow(table_df)>0){
    total_locations <- nrow(table_df)
    png(paste(normalize_directory_path( g_config$reporting$site_directory),
              get_image_name(table_name,paste(field_name,"_time",sep="")),sep=""))
    # not using ggplot here as it is very expensive for a large number of values

    barplot(table_df, main = paste(field_name,": Distribution"),
            xlab = paste(field_name,"(Total: ",total_locations,")"), ylab = paste(table_name,"Count"))
    
    table_df <- as.data.frame(table_df)
    table_df <- table_df[order(table_df[,2], decreasing = T),]

    return_message<-paste("The most frequent values for",field_name,"are:")
    for (index in 1:min(5, nrow(table_df))){
      return_message <- paste(return_message,table_df[index,1]);
      if(index<5){return_message<-paste(return_message,",")}
    }
    return_message<-c(return_message, paste("\n Time range: ",time_min," - ",time_max))
    
    if(!is.null(dev.list())) dev.off()
    return(return_message)
  } 
}


# Ratio Fields
#functionName: describeRatioField
#Description: generate a histogram for a ratio field
#Inputs:
	#1. an R dataframe containing a given database table
	#2. the name of the ratio field
	#3. the unit associated with the field
#Output: write the histogram to a file
describeRatioField<-function(table_df,table_name,field_name, unit){
  table_df <- table_df %>%
    group_by_(field_name) %>%
    dplyr::summarize(freq = n()) %>%
    as.data.frame() %>%
    na.omit() 
  table_df[,2] <- as.numeric(table_df[,2])
  colnames(table_df) <- c("Var1", "Freq")
  table_df$Var1 = as.numeric(table_df$Var1)
  
  if(nrow(table_df) > 0){
    mean_Var1<-round(mean(table_df[,1], na.rm=TRUE),2)
    sd_Var1<-round(sd(table_df[,1], na.rm=TRUE),2)
    median_Var1<-median(mean(table_df[,1], na.rm=TRUE),2)
    
    max_Var1<-table_df[which.max(table_df[,1]),1]
    min_Var1<-table_df[which.min(table_df[,1]),1]
    
    #  #compute extreme outliers.
    lowerq <- quantile(table_df[,1],na.rm=TRUE)[2]
    upperq <- quantile(table_df[,1],na.rm=TRUE)[3]
    iqr <- upperq - lowerq # interquartile range
    extreme.threshold.upper <-(iqr * 3) + upperq
    extreme.threshold.lower <- lowerq - (iqr * 3)
    #cases outside the upper and lower threshold.
    total_data_points_upper_threshold<-length(table_df[table_df[,1]>extreme.threshold.upper[[1]],1])
    outlier_message<-""
    if(total_data_points_upper_threshold>0)
    {
      outlier_message<-paste("Investigate ",total_data_points_upper_threshold,
                             " data points above the upper threshold (",extreme.threshold.upper[[1]], ")"
                             ,sep="")
    }
    if(extreme.threshold.lower[[1]]<0)
    {
      extreme.threshold.lower[[1]]<-0
    }
    total_data_points_below_lower_threshold<-length(table_df[table_df[,1]<extreme.threshold.lower[[1]],1])
    
    if(total_data_points_below_lower_threshold>0){
      outlier_message<-paste(outlier_message," Investigate ",total_data_points_below_lower_threshold,
                             " data points below the lower threshold (", extreme.threshold.lower,")",sep="")
    }
    
    #create histogram
    p<-ggplot(data=table_df, aes(x=Var1, y=Freq, width=1, fill=Freq)) +
      geom_bar(stat="identity", position="identity") + ggtitle(paste(field_name,": Distribution\n(Mean=",mean_Var1," Std. Deviation=",sd_Var1,")"))
    p<-p+ylab(paste(table_name,"Count"))+xlab(paste(field_name,"(in",unit,")"))
    #remove legend and set size and orientation of tick labels
    p<-p+theme(legend.position="none", text = element_text(size=6),
               axis.text.x = element_text(angle=90, vjust=1), plot.background = element_blank() ,
               panel.grid.major = element_blank() ,panel.grid.minor = element_blank() ,
               panel.border = element_blank())
    ggsave(file=paste(normalize_directory_path( g_config$reporting$site_directory),
                      get_image_name(table_name,field_name),sep=""))
    return(paste("\n\nMaximum:",max_Var1 ,"Minimum:", min_Var1, "Mean:",mean_Var1,"Std Deviation:",
                 sd_Var1,"Median:",median_Var1,"\n\n" ))
  }
}

# Foreign Key Fields
#functionName: describeForeignKeyIdentifiers
#Description: generate a barplot for a foreignkey field in a table
#Inputs:
	#1. an R dataframe containing a given database table
	#2. the name of the foreign key field
#Output: write the barplot to a file

describeForeignKeyIdentifiers<-function(table_df, table_name, field_name, group_ret = 1)
{ 	flog.info(paste("Plotting for Field: ", field_name))
  ###Check for retrieve_dataframe_group_structure
  if(group_ret){
    table_df <- table_df %>% na.omit()
  }
  else{
    table_df <- table_df %>%
      na.omit() %>%
      group_by_(field_name) %>%
      dplyr::summarize(freq = n()) %>%
      as.data.frame()
    table_df[,2] <- as.numeric(table_df[,2])
    table_df <- table_df[order(table_df[,1]),]
  }
    if(nrow(table_df)>0){
      total_values<- nrow(table_df)
      png(paste(normalize_directory_path( g_config$reporting$site_directory),
                get_image_name(table_name,field_name),sep=""))
      # not using ggplot here as it is very expensive for a large number of values
      barplot(height = table_df[,2], names.arg = table_df[,1],
                main = paste(field_name,": Distribution"), 
                xlab = paste(field_name,"(Total: ",total_values,")"), 
                ylab = paste(table_name,"Count"), xaxt='n')
      #also plot in decreasing order of frequency (to compare distribution with source data)
      png(paste(normalize_directory_path( g_config$reporting$site_directory),get_image_name_sorted(table_name,field_name),sep=""))
      table_df <- table_df[order(-table_df[,2]),]
      barplot(height = table_df[,2], names.arg = table_df[,1],
                main = paste(field_name,": Distribution"),
                xlab = paste(field_name,"(Total: ",total_values,")"), 
                ylab = paste(table_name,"Count"), xaxt='n')
      
      return_message<-paste("The most frequent values for",field_name,"are:")
      for (index in 1:5)
      {
        return_message<-paste(return_message,table_df[index,1]);
        if(index<5) {return_message<-paste(return_message,",")}
      }
      if(!is.null(dev.list())) dev.off()
      if(!is.null(dev.list())) dev.off()
      return(return_message)
    }
}


#update values of concept_id fields to include concept names  
#- to improve readability for plotting purposes
EnhanceFieldValues<-function(table_df,field_name,df_ref)
{
  table_df <- table_df %>% as.data.frame()
  column_index <- which(colnames(table_df)==field_name)
  
  for(i in 1:nrow(df_ref)){
    table_df[,column_index][table_df[,column_index]== df_ref[i,1]] <- paste(
    df_ref[i,2]," (",df_ref[i,1],")",sep="");
  }
  return(table_df);
}


