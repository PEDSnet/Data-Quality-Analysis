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

describeIdentifier<-function(df_table, field_name)
{
	column_index <- which(colnames(df_table)==field_name)
  df_table <-df_table[,column_index]
  total_distinct_values <-length(unique(df_table))
  if(total_distinct_values == 1)
      if(is.na(unique(df_table)))
        return (0);
	return (total_distinct_values);
}


#functionName: reportMissingCount
#Description: count the number of records with no value for a given field
#Inputs:an R dataframe containing a given database table, the name of the field
#Output: number of rows with NA (missing) values for the input field
reportMissingCount<-function(df_table,table_name,field_name, big_data_flag)
{
  if(big_data_flag==FALSE)
  {
    #retrieve the index of the field in the dataframe
    column_index <- which(colnames(df_table)==field_name)

    if(nrow(df_table)>0)
    {
      # saving the frequencies in a separate dataframe (including NA values)
      dfTab <-as.data.frame(table(df_table[,column_index], exclude=NULL))
      #add a new column to this new dataframe containing the percentage frequency information rounded to 2 digits
      if(nrow(dfTab)>0)
      {
        dfTab$label <- as.character(
          paste(
            round(100 * dfTab$Freq / sum(dfTab$Freq),digits=2)
            ,'%')	# add percentage
        )
        # the last row contains the frequency for the NA value
        return(paste("\n\nPercentage of",table_name,"with missing values for ",field_name," is ",dfTab[nrow(dfTab),3]));
      }
      else
        return(paste("\n\nPercentage of",table_name,"with missing values for ",field_name," is 100%"));
    }
    else
      return(paste("\n\nPercentage of",table_name,"with missing values for ",field_name," is 100%"));
  } else # big data with dplyr or query wise that captures frequency
    {

      colnames(df_table)[2] <- "Freq"
      # identify row with null value
      new_df_table<-subset(df_table, is.na(df_table[1]))
      if(nrow(new_df_table)>0) # if there is a null value
      {
        #add a new column to this new dataframe containing the percentage frequency information rounded to 2 digits

        df_table$label <- as.character(
            paste(
              round(100 * df_table$Freq / sum(df_table$Freq),digits=2)
              ,'%')	# add percentage
          )
	# find the row that contains the frequency for the NA value
	na_df_table<-subset(df_table, is.na(df_table[1]))

        return(paste("\n\nPercentage of",table_name,"with missing values for ",field_name," is ",na_df_table[1,3]));

   }
        else
          return(paste("\n\nPercentage of",table_name,"with missing values for ",field_name," is 0%"));

    }
}

#functionName: reportNoMatchingCount
#Description: count the number of records with mo matching concepts (concept_id=0)
#Inputs:an R dataframe containing a given database table, the name of the field
#Output: number of rows with 0 value for the input field
reportNoMatchingCount<-function(df_table,table_name,field_name, big_data_flag)
{
  if(big_data_flag==FALSE)
  {
    #retrieve the index of the field in the dataframe
    column_index <- which(colnames(df_table)==field_name)

    if(nrow(df_table)>0)
    {
      # saving the frequencies in a separate dataframe (including NA values)
      dfTab <-as.data.frame(table(df_table[,column_index], exclude=NULL))
      #add a new column to this new dataframe containing the percentage frequency information rounded to 2 digits
      if(nrow(dfTab)>0)
      {
        dfTab$label <- as.character(
          paste(
            round(100 * dfTab$Freq / sum(dfTab$Freq),digits=2)
            ,'%')	# add percentage
        )
        # the last row contains the frequency for the NA value
        return(paste("\n\nPercentage of",table_name,"with missing values for ",field_name," is ",dfTab[nrow(dfTab),3]));
      }
      else
        return(paste("\n\nPercentage of",table_name,"with missing values for ",field_name," is 100%"));
    }
    else
      return(paste("\n\nPercentage of",table_name,"with missing values for ",field_name," is 100%"));
  }
  else # big data with dplyr or query wise that captures frequency
  {

    colnames(df_table)[2] <- "Freq"
    #add a new column to this new dataframe containing the percentage frequency information rounded to 2 digits
    df_table$label <- as.character(
      paste(
        round(100 * df_table$Freq / sum(df_table$Freq),digits=2)
        ,'%')	# add percentage
    )

    # identify row with no matching concept
    new_df_table<-subset(df_table,df_table[1]==0)
    if(nrow(new_df_table)>0) # if there is a 0 value
    {
      # the last row contains the frequency for the NA value
      return(paste("\n\nPercentage of",table_name,"with no matching concepts in ",field_name," is ",new_df_table[1,3]));
    }
    if(nrow(new_df_table)==0)
      return(paste("\n\nPercentage of",table_name,"with no matching concepts in ",field_name," is 0%"));

  }
}

reportNullFlavors<-function(df_table,table_name,field_name,UN_code,OT_code,NI_code,big_data_flag)
{
  if(big_data_flag==FALSE)
  {
    #retrieve the index of the field in the dataframe
    column_index <- which(colnames(df_table)==field_name)

  if(nrow(df_table)>0)
  {
    # saving the frequencies in a separate dataframe (including NA values)
    dfTab <-as.data.frame(table(df_table[,column_index], exclude=NULL))
    #add a new column to this new dataframe containing the percentage frequency information rounded to 2 digits
    dfTab$label <- as.character(
      paste(
        round(100 * dfTab$Freq / sum(dfTab$Freq),digits=2)
        ,'%')  # add percentage
    )

    # flog.info(dfTab)
    count_ni<-subset(dfTab,Var1==NI_code)$label[1]
    if(is.na(count_ni)) count_ni<-"0%";
    count_un<-subset(dfTab,Var1==UN_code)$label[1]
    if(is.na(count_un)) count_un<-"0%";
    count_ot<-subset(dfTab,Var1==OT_code)$label[1]
    if(is.na(count_ot)) count_ot<-"0%";
    # flog.info(count_ni[1])
    # the last row contains the frequency for the NA value
    count_missing_values<-dfTab[nrow(dfTab),3];
    return(paste(
      "\nPercentage of",table_name,"with unknown value for ",field_name," is ",count_un,"\n",
      "\nPercentage of",table_name,"with other value for ",field_name," is ",count_ot,"\n",
      "\nPercentage of",table_name,"with no information for ",field_name," is ",count_ni,"\n",
      "\nPercentage of",table_name,"with missing values for ",field_name," is ",count_missing_values,"\n"
    ));
  }

}    else # using dplyr or query wise - when big data flag is true
    {
      #retrieve the index of the field in the dataframe
      colnames(df_table)[1] <- "Var1"
      colnames(df_table)[2] <- "Freq"
      df_table<-subset(df_table,!is.na(Var1))

      if(nrow(df_table)>0)
      {
        df_table$Var1 <- as.factor(df_table$Var1)

        df_table$label <- as.character(
          paste(
            round(100 * df_table$Freq / sum(df_table$Freq),digits=2)
            ,'%')  # add percentage
        )

        # flog.info(dfTab)
        #print(df_table)
      
        count_ni<-subset(df_table,Var1==NI_code)$label[1]
        #print(count_ni)
        if(is.na(count_ni)) count_ni<-"0%";
        count_un<-subset(df_table,Var1==UN_code)$label[1]
        if(is.na(count_un)) count_un<-"0%";
        count_ot<-subset(df_table,Var1==OT_code)$label[1]
        if(is.na(count_ot)) count_ot<-"0%";
        # flog.info(count_ni[1])
        count_missing_values<-subset(df_table,is.na(Var1))$label[1]
        if(is.na(count_missing_values)) count_missing_values<-"0%";

        return(paste(
          "\nPercentage of",table_name,"with unknown value for ",field_name," is ",count_un,"\n",
          "\nPercentage of",table_name,"with other value for ",field_name," is ",count_ot,"\n",
          "\nPercentage of",table_name,"with no information for ",field_name," is ",count_ni,"\n",
          "\nPercentage of",table_name,"with missing values for ",field_name," is ",count_missing_values,"\n"
        ));
      } # end of if
      else
      {
        return(paste(
          "\nPercentage of",table_name,"with unknown value for ",field_name," is 0%\n",
          "\nPercentage of",table_name,"with other value for ",field_name," 0%\n",
          "\nPercentage of",table_name,"with no information for ",field_name," is 0%\n",
          "\nPercentage of",table_name,"with missing values for ",field_name," is 100%\n"
        ));
      }
    }  # end else
}

reportUnexpected<-function(df_table,table_name,field_name,permissible_values,big_data_flag)
{
     return_message<-""

    if(big_data_flag==FALSE)
    {

    #retrieve the index of the field in the dataframe
    column_index <- which(colnames(df_table)==field_name)
    # flog.info(column_index)
    current_values<-unique(df_table[,column_index])
         for( value in current_values)
        {
        if(!is.element(value,permissible_values))
          return_message<-paste(return_message, "invalid value found: ",value,";")
        }
    }

    else # with dplyr package or query wise scripts
    {
           current_values<-c(df_table[,1])

           for(i in 1:nrow(df_table))
            {
               value <-df_table[i,1]
               # flog.info(df_table[i,1])
               if(!is.element(value,permissible_values) && !is.na(value))
                   return_message<-paste(return_message, "invalid value found: ",value,";")

           }



    }

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
describeNominalField<-function(df_table, table_name,field_name, label_bins, order_bins, color_bins, big_data_flag)
{

	 flog.info(paste("Plotting for Field: ", field_name))
  # flog.info()

    if(big_data_flag==FALSE)
    {
	# retrieve the column index for the field
	#column_index<-(grep(field_name, colnames(df_table))
	column_index <- which(colnames(df_table)==field_name)
	# flog.info(c("columns index is ",column_index))

	# saving the frequencies and percentage in a separate dataframe including NA values
	if(nrow(df_table)>0)
	{
		dfTab <-as.data.frame(table(df_table[,column_index], exclude=NULL))
		dfTab$label <- as.character(
		paste(
			round(100 * dfTab$Freq / sum(dfTab$Freq),digits=2)
			,'%')	# add percentage
		)
    # flog.info(dfTab)

		#creating barplot from dfTab dataframe
		p<-ggplot(dfTab, aes(x = Var1, y = Freq, fill = Var1)) + geom_bar(stat = "identity") + ggtitle(paste(field_name,": Distribution"))
		# flog.info(p)
    # add axis labels
 		p<-p+ylab(paste(table_name,"Count"))+xlab(field_name)
		# specify the order of catgories and also the labels for x-axis
		p<-p+scale_x_discrete(labels=label_bins, limits= order_bins)
		# specify the color for each category
		p<-p+ scale_fill_manual(values=color_bins,na.value="grey64")
		#remove legend and set size and orientation of tick labels
		p<-p+theme(legend.position="none", text = element_text(size=10),
        	axis.text.x = element_text(angle=90, vjust=1))
		# add the label to each bar (from the dfTab dataframe)
		p<-p+geom_text(data=dfTab, aes(x=Var1,y=Freq,label=label), size=3)
		# flog.info(p)
		#save the barplot image (will be referenced by the final report)
    ggsave(file=paste(normalize_directory_path(g_config$reporting$site_directory),get_image_name(table_name,field_name),sep=""))

	 }
    }

    else #using dplyr
    {
        if(nrow(df_table)>0)
        {
            #dfTab <-as.data.frame(table(df_table[,column_index], exclude=NULL))
            #adding new columns

            colnames(df_table)[1] <- "Var1"
            colnames(df_table)[2] <- "Freq"
            df_table$Var1 <- as.factor(df_table$Var1)

            df_table$label <- as.character(
            paste(
            round(100 * df_table$Freq / sum(df_table$Freq),digits=2)
            ,'%')	# add percentage
            )

            # flog.info(df_table)
            #creating barplot from dfTab dataframe
            p<-ggplot(df_table, aes(x = Var1, y = Freq, fill = Var1)) + geom_bar(stat = "identity") + ggtitle(paste(field_name,": Distribution"))
            # add axis labels

            p<-p+ylab(paste(table_name,"Count"))+xlab(field_name)

            # specify the order of catgories and also the labels for x-axis
            p<-p+scale_x_discrete(labels=label_bins, limits= order_bins)

            # specify the color for each category
            p<-p+ scale_fill_manual(values=color_bins,na.value="grey64")
            #remove legend and set size and orientation of tick labels
            p<-p+theme(legend.position="none", text = element_text(size=10),
            axis.text.x = element_text(angle=90, vjust=1))
            # add the label to each bar (from the dfTab dataframe)
            p<-p+geom_text(data=df_table, aes(x=Var1,y=Freq,label=label), size=3)
            #save the barplot image (will be referenced by the final report)
            # flog.info(p)
            #if(!is.null(p$layers$position_stack))
             # {
              ggsave(file=paste(normalize_directory_path(g_config$reporting$site_directory),get_image_name(table_name,field_name),sep=""))
            # flog.info(s)

            #}
        }
    }


}


#updated nominal field
describeNominalField_basic<-function(df_table, table_name,field_name,big_data_flag)
{

     flog.info(paste("Plotting for Field: ", field_name))


    if(big_data_flag==FALSE)
    {
    # retrieve the column index for the field
    #column_index<-(grep(field_name, colnames(df_table))
    column_index <- which(colnames(df_table)==field_name)
    # flog.info(column_index)

    # saving the frequencies and percentage in a separate dataframe including NA values
    #df_table<-subset(df_table,!is.na(Var1))
    if(nrow(df_table)>0)
    {
        dfTab <-as.data.frame(table(df_table[,column_index], exclude=NULL))
        # commenting the next line as we do want to include the NA values in this field
        #dfTab<-subset(dfTab,!is.na(Var1))

        if(nrow(dfTab)>0)
        {

        dfTab$label <- as.character(
        paste(
        round(100 * dfTab$Freq / sum(dfTab$Freq),digits=2)
        ,'%')	# add percentage
        )

        if(nrow(dfTab)==1 && is.na(dfTab[1]))
          return("");

        #creating barplot from dfTab dataframe
        p<-ggplot(dfTab, aes(x = Var1, y = Freq, fill = Var1)) + geom_bar(stat = "identity") + ggtitle(paste(field_name,": Distribution"))
        # add axis labels
        p<-p+ylab(paste(table_name,"Count"))+xlab(field_name)
        #remove legend and set size and orientation of tick labels
        p<-p+theme(legend.position="none", text = element_text(size=10),
        axis.text.x = element_text(angle=90, vjust=1))
        # add the label to each bar (from the dfTab dataframe)
        p<-p+geom_text(data=dfTab, aes(x=Var1,y=Freq,label=label), size=3)
        # flog.info(p)
        #save the barplot image (will be referenced by the final report)
        ggsave(file=paste(normalize_directory_path( g_config$reporting$site_directory),get_image_name(table_name,field_name),sep=""))

        }
    }
    }
    else
    {
        colnames(df_table)[1] <- "Var1"
        colnames(df_table)[2] <- "Freq"
        #df_table<-subset(df_table,!is.na(Var1))

        if(nrow(df_table)>0)
        {
            #dfTab <-as.data.frame(table(df_table[,column_index], exclude=NULL))
            #adding new columns

            df_table$Var1 <- as.factor(df_table$Var1)
            
            df_table$label <- as.character(
            paste0(
            round(100 * df_table$Freq / sum(df_table$Freq),digits=2)
            ,'%')	# add percentage
            )

           #  flog.info(df_table)
            #creating barplot from dfTab dataframe
            p<-ggplot(df_table, aes(x = Var1, y = Freq, fill = Var1)) + geom_bar(stat = "identity") + ggtitle(paste(field_name,": Distribution"))
            
            # add axis labels
            p<-p+ylab(paste(table_name,"Count"))+xlab(field_name)
            
            #remove legend and set size and orientation of tick labels
            p<-p+theme(legend.position="none", text = element_text(size=10),
            axis.text.x = element_text(angle=90, vjust=1))
            
            # add the label to each bar (from the dfTab dataframe)
            p<-p+geom_text(data=df_table, aes(x=Var1,y=Freq,label=label), size=3)
            #save the barplot image (will be referenced by the final report)
            # flog.info(p)
            if(nrow(df_table)==1 & is.na(df_table[1,1]))
            {
              ### dont print the graph
            }
            else {
            ggsave(file=paste(normalize_directory_path( g_config$reporting$site_directory),get_image_name(table_name,field_name),sep=""))
            }  
            
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
describeOrdinalField<-function(df_table, table_name,field_name,big_data_flag)
{


	 flog.info(paste("Plotting for Field: ", field_name))
    if(big_data_flag==FALSE)
    {
	column_index <- which(colnames(df_table)==field_name)

	if(nrow(df_table)>0)
	{

	# saving the frequencies in a separate dataframe
	dfTab <-as.data.frame(table(df_table[,column_index]))
       if(nrow(dfTab)>0)
       {
		#create a bar plot
		p<-ggplot(dfTab, aes(x = Var1, y = Freq, fill = Var1)) + geom_bar(stat = "identity") + ggtitle(paste(field_name,": Distribution"))
		# add axis labels
		p<-p+ylab(paste(table_name,"Count"))+xlab(field_name)
		#remove legend and set size and orientation of tick labels
		p<-p+theme(legend.position="none", text = element_text(size=6),axis.text.x = element_text(angle=90, vjust=1), plot.background = element_blank() ,panel.grid.major = element_blank() ,panel.grid.minor = element_blank() ,panel.border = element_blank())
        ggsave(file=paste(normalize_directory_path( g_config$reporting$site_directory),get_image_name(table_name,field_name),sep=""))

       }
	}
    }
    else #if TRUE using dplyr
    {
        colnames(df_table)[1] <- "Var1"
        colnames(df_table)[2] <- "Freq"
        df_table<-subset(df_table,!is.na(Var1))

        if(nrow(df_table)>0)
        {
             df_table$Var1 <- as.factor(df_table$Var1)
            #adding new columns
             #creating barplot from dfTab dataframe
            #p<-ggplot(df_table, aes(x = Var1, y = Freq, fill = Var1)) + geom_bar(stat = "identity") + ggtitle(paste(field_name,": Distribution"))
            p<-ggplot(df_table, aes(x = Var1, y = Freq, fill = Var1)) + geom_bar(stat = "identity") + ggtitle(paste(field_name,": Distribution"))
            # add axis labels
            p<-p+ylab(paste(table_name,"Count"))+xlab(field_name)
            #remove legend and set size and orientation of tick labels
            p<-p+theme(legend.position="none", text = element_text(size=6),axis.text.x = element_text(angle=90, vjust=1), plot.background = element_blank() ,panel.grid.major = element_blank() ,panel.grid.minor = element_blank() ,panel.border = element_blank())

            ggsave(file=paste(normalize_directory_path( g_config$reporting$site_directory),get_image_name(table_name,field_name),sep=""))
            # flog.info(p)

        }

    }
}

# specialized date function
describeDateField<-function(df_table, table_name,field_name,big_data_flag)
{
   #  flog.info(paste("Plotting for Field: ", field_name))

    if(big_data_flag==FALSE)
    {

       flog.info(paste("Plotting for Field: ", field_name))

        column_index <- which(colnames(df_table)==field_name)
        # flog.info(nrow(df_table))
        if(nrow(df_table)>0)
        {
          df_table<-df_table[,column_index]
          #df_table<-substring(df_table,nchar(df_table)-7) #extracting the date portion of the datetime field
          df_table<-as.Date(df_table)
          #df_table<-as.Date(format(df_table)," "), "[[", 1)
          dfTab <- table(df_table)
          if(nrow(dfTab)>0)
          {
            #calculate the total number of locations
            # flog.info(colnames(dfTab))
            newdata <- df <- as.data.frame(dfTab)
            # flog.info(newdata)
            ordered_data<-newdata[order(newdata[,2], decreasing = TRUE),]
            # flog.info(ordered_data)

            # counting the total number of unique values
            total_locations <- nrow(dfTab)

            png(paste(normalize_directory_path( g_config$reporting$site_directory),get_image_name(table_name,field_name),sep=""))
            # not using ggplot here as it is very expensive for a large number of values
           # barplot(dfTab, main = paste(field_name,": Distribution"), xlab = paste(field_name,"(Total: ",total_locations,")"), ylab = paste(table_name,"Count"), xaxt='n')
           barplot(dfTab, main = paste(field_name,": Distribution"), xlab = paste(field_name,"(Total: ",total_locations,")"), ylab = paste(table_name,"Count"))



            return_message<-paste("The most frequent values for",field_name,"are:")
            index<-1;
            while (index<=5)
            {
              return_message<-paste(return_message,ordered_data[index,1]);
              if(index<5)
                return_message<-paste(return_message,",")
              index<-index+1;
            }
            return_message<-c(return_message, paste("\n Date range: ",min(df_table,na.rm=TRUE),"-",max(df_table,na.rm=TRUE)))
            # check if future dates are included
            if(Sys.Date()<max(df_table,na.rm=TRUE))
               return_message<-c(return_message,"\nWARNING: includes future dates")

            dev.off()
            return(return_message)
          }
        }
    }
    else #using dplyr or query wise
    {
      df_table<-subset(df_table,!is.na(df_table[,1]))
    if(nrow(df_table)>0)
    {
      # df table is actually a dataframe of two dataframes
      colnames(df_table)[1] <- "Var1"
      colnames(df_table)[2] <- "Freq"

      # remove NA values - we dont want to plot missing values
      #df_table<-subset(df_table,!is.na(Var1))

      df_table$Var1<-as.Date(df_table[,1])
      #df_table$Var1 <- as.factor(df_table$Var1)
      df_table$Var1 <- as.character(df_table$Var1)

      # aggregate df_table again by summing frequency for all equivalent dates
      df_table<-aggregate(df_table$Freq,FUN=sum, by = list(df_table$Var1))
      colnames(df_table)[1] <- "Var1"
      colnames(df_table)[2] <- "Freq"
      # creare a raw vector and then create a table

      new_vector<-rep.int(df_table$Var1,df_table$Freq)
      # creating a table out of it so that we can use in the barplot function
      dfTab<-table(new_vector)

      ordered_data<-df_table[order(-df_table[,2]), ]

      total_values<- length(unique(df_table$Var1))

      png(paste(normalize_directory_path( g_config$reporting$site_directory),get_image_name(table_name,field_name),sep=""))
      # not using ggplot here as it is very expensive for a large number of values
      #vector_table <- as.vector(as.matrix(df_table$Freq))
      # flog.info("here")
      #barplot(df_table$Freq, df_table$Var1, main = paste(field_name,": Distribution"), xlab = paste(field_name,"(Total: ",total_values,")"), ylab = paste(table_name,"Count"), xaxt='n')
      barplot(dfTab, main = paste(field_name,": Distribution"), xlab = paste(field_name,"(Total: ",total_values,")"), ylab = paste(table_name,"Count"))


      return_message<-paste("The most frequent values for",field_name,"are:")

      index<-1;
      while (index<=5 && nrow(ordered_data)>=index)
      {
        #if(!is.na(ordered_data[index,1])) # if NA then dont include as the top concept
        return_message<-paste(return_message,ordered_data[index,1]);

        if(index<5)
          return_message<-paste(return_message,",")
        index<-index+1;
      }
      return_message<-c(return_message, paste("\n Date range: ",min(df_table$Var1)," - ",max(df_table$Var1)))
      if(Sys.Date()<max(df_table$Var1))
        return_message<-c(return_message,"\nWARNING: includes future dates")

      dev.off()
       return(return_message)
        }


    }

}

describeYYMMField<-function(df_table, table_name,field_name,fact_type)
{
  #  flog.info(paste("Plotting for Field: ", field_name))
  
    df_table<-subset(df_table,!is.na(df_table[,1]))
    if(nrow(df_table)>0)
    {
      # df table is actually a dataframe of two dataframes
      colnames(df_table)[1] <- "Var1"
         # creare a raw vector and then create a table
      colnames(df_table)[2] <- "Freq"
      df_table$Freq<- as.integer(df_table$Freq)
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
     
       
      
      #dev.off()
    }
  
}


# Ordinal Fields (with large number of values)
#functionName: describeOrdinalField_large
#Description: generate a barplot for an ordinal field
#Inputs:
	#1. an R dataframe containing a given database table
	#2. the name of the ordinal field
#Output: write the barplot to a file
describeOrdinalField_large<-function(df_table, table_name,field_name,big_data_flag)
{

	 flog.info(paste("Plotting for Field: ", field_name))

    if(big_data_flag==FALSE)
    {
	column_index <- which(colnames(df_table)==field_name)
    # flog.info(nrow(df_table))
	if(nrow(df_table)>0)
	{

		dfTab <- table(df_table[,column_index])
		if(nrow(dfTab)>0)
		{
		#calculate the total number of locations
        # flog.info(colnames(dfTab))
        newdata <- df <- as.data.frame(dfTab)
        # flog.info(newdata)
        ordered_data<-newdata[order(newdata[,2], decreasing = TRUE),]
        # flog.info(ordered_data)

        # counting the total number of unique values
        total_locations <- nrow(dfTab)

        png(paste(normalize_directory_path( g_config$reporting$site_directory),get_image_name(table_name,field_name),sep=""))
		# not using ggplot here as it is very expensive for a large number of values
		barplot(dfTab, main = paste(field_name,": Distribution"), xlab = paste(field_name,"(Total: ",total_locations,")"), ylab = paste(table_name,"Count")) #, xaxt='n')



        return_message<-paste("The most frequent values for",field_name,"are:")
        index<-1;
        while (index<=5)
        {
            return_message<-paste(return_message,ordered_data[index,1]);
            if(index<5)
                return_message<-paste(return_message,",")
            index<-index+1;
        }
        #return_message<-c(return_message, paste("Range: ",min(df_table)," - ",max(df_table)))

        dev.off()
        return(return_message)
		}
    }
    }
    else # for handling bigdata with dplyr
    {
        colnames(df_table)[1] <- "Var1"
        colnames(df_table)[2] <- "Freq"
        df_table<-subset(df_table,!is.na(Var1))

        if(nrow(df_table)>0)
        {

          # creare a raw vector and then create a table
          new_vector<-rep.int(df_table$Var1,df_table$Freq)
          # creating a table out of it so that we can use in the barplot function
          dfTab<-table(new_vector)

            ordered_data<-df_table[order(df_table[,2], decreasing = TRUE),]
            # flog.info(ordered_data)

            # counting the total number of unique values
            total_values <- nrow(df_table)

            png(paste(normalize_directory_path( g_config$reporting$site_directory),get_image_name(table_name,field_name),sep=""))
            # not using ggplot here as it is very expensive for a large number of values
          barplot(dfTab, main = paste(field_name,": Distribution"), xlab = paste(field_name,"(Total: ",total_values,")"), ylab = paste(table_name,"Count"))
          #barplot(df_table$Freq, df_table$Var1, main = paste(field_name,": Distribution"), xlab = paste(field_name,"(Total: ",total_values,")"), ylab = paste(table_name,"Count")) #, xaxt='y')



            return_message<-paste("The most frequent values for",field_name,"are:","\n")
            index<-1;
            while (index<=5)
            {
              #return_message<-paste(return_message,ordered_data[index,1]);
              # adding information on total counts also
              return_message<-paste(return_message,ordered_data[index,1],"|count=",ordered_data[index,2]);
              if(index<5)
                return_message<-paste(return_message,",\n")
              index<-index+1;
            }
            #return_message<-c(return_message, paste("Range: ",min(df_table)," - ",max(df_table)))

            dev.off()
            return(return_message)
          }

    }
}

describeTimeField<-function(df_table, table_name,field_name,big_data_flag)
{

     flog.info(paste("Plotting for Field: ", field_name))

    if(big_data_flag==FALSE)
    {
        column_index <- which(colnames(df_table)==field_name)
        # flog.info(nrow(df_table))
        if(nrow(df_table)>0)
        {
            # all time fields are now datetime fields
            #handling datetime fields in Oracle
                df_table<-df_table[,column_index]
                #df_table<-lapply(strsplit(format(df_table)," "), "[[", 2)
                # check if contains timesize information
                # if contains timezone information
                #if(length(strsplit(df_table[1],"-")[[1]])==4)
                 # {
                   # remove timezone information
                  df_table<-substr(df_table,12,19)
                #  }
                #df_table<- paste((as.POSIXlt(df_table,format="%H:%M:%S"))$hour,":",(as.POSIXlt(df_table,format="%H:%M:%S"))$min,":",(as.POSIXlt(df_table,format="%H:%M:%S"))$sec,sep="")
                dfTab <- table(df_table)


            if(nrow(dfTab)>0)
            {

                #calculate the total number of locations
                # flog.info(colnames(dfTab))
                newdata <- as.data.frame(dfTab)
                # flog.info(newdata)
                ordered_data<-newdata[order(newdata[,2], decreasing = TRUE),]
                # flog.info(ordered_data)

                # counting the total number of unique values
                total_values <- nrow(dfTab)

                png(paste(normalize_directory_path( g_config$reporting$site_directory),get_image_name(table_name,paste(field_name,"_time",sep="")),sep=""))
                # not using ggplot here as it is very expensive for a large number of values
                barplot(dfTab, main = paste(field_name,": Time Distribution"), xlab = paste(field_name,"(Total: ",total_values,")"), ylab = paste(table_name,"Count"))
                #, xaxt='n')



                return_message<-paste("The most frequent values for",field_name,"are:")
                index<-1;
                while (index<=5 && nrow(ordered_data)>=index)
                {
                    return_message<-paste(return_message,ordered_data[index,1]);
                    if(index<5)
                      return_message<-paste(return_message,",")
                    index<-index+1;
                }
                return_message<-c(return_message, paste("\n Time range: ",min(df_table)," - ",max(df_table)))

                dev.off()
                return(return_message)
            }
        }
    }
    else # for handling bigdata with dplyr and query wise
    {
        colnames(df_table)[1] <- "Var1"
        colnames(df_table)[2] <- "Freq"
        df_table<-subset(df_table,!is.na(Var1))

        if(nrow(df_table)>0)
        {
            #if(length(strsplit(df_table[,1],"-")[[1]])==4)
            #{
            # remove timezone information and extract time
              df_table$Var1<-substr(df_table$Var1,12,19)
            #}
            # remove NA values
            #df_table$Var1 <- as.factor(df_table$Var1)
            df_table<-aggregate(df_table$Freq,FUN=sum, by = list(df_table$Var1))
            colnames(df_table)[1] <- "Var1"
            colnames(df_table)[2] <- "Freq"

            new_vector<-rep.int(df_table$Var1,df_table$Freq)
            # creating a table out of it so that we can use in the barplot function
            dfTab<-table(new_vector)


            ordered_data<-df_table[order(-df_table[,2]), ]

            total_values<- length(unique(df_table$Var1))

            png(paste(normalize_directory_path( g_config$reporting$site_directory),get_image_name(table_name,paste(field_name,"_time",sep="")),sep=""))
            # not using ggplot here as it is very expensive for a large number of values
            #vector_table <- as.vector(as.matrix(df_table$Freq))
            #vector_table_y <- as.vector(as.matrix(df_table$Freq))
            # flog.info("here")
            #barplot(df_table$Freq, df_table$Var1, main = paste(field_name,": Distribution"), xlab = paste(field_name,"(Total: ",total_values,")"), ylab = paste(table_name,"Count"), xaxt='n')
            barplot(dfTab, main = paste(field_name,": Time Distribution"), xlab = paste(field_name,"(Total: ",total_values,")"), ylab = paste(table_name,"Count"))


            return_message<-paste("The most frequent values for",field_name,"are:")

            index<-1;
            while (index<=5 && nrow(ordered_data)>=index)
            {
                #if(!is.na(ordered_data[index,1])) # if NA then dont include as the top concept
                  return_message<-paste(return_message,ordered_data[index,1]);

                if(index<5)
                return_message<-paste(return_message,",")
                index<-index+1;
            }
            return_message<-c(return_message, paste(" \n Time range: ",min(df_table$Var1)," - ",max(df_table$Var1)))

            dev.off()
            return(return_message)

        }
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
describeRatioField<-function(df_table,table_name,field_name, unit,big_data_flag)
{
	 flog.info(paste("Plotting for Field: ", field_name))

    if(big_data_flag==FALSE)
    {
	column_index <- which(colnames(df_table)==field_name)

	if(nrow(df_table)>0)
	{


		# saving the frequencies in a separate dataframe
		dfTab <-as.data.frame(table(df_table[,column_index]))

    if(nrow(dfTab)>0) # if all values are NULL
        {
		#caluclating mean and standard deviation
        #mean_Var1<-round(mean(df_table[,column_index]), digits=2)
        mean_Var1<-round(mean(df_table[,column_index],trim=0,na.rm=TRUE), digits=2)
        #sd_Var1<-round(sd(df_table[,column_index]), digits=2)
        sd_Var1<-round(sd(df_table[,column_index],na.rm=TRUE), digits=2)
        max_Var1<-df_table[which.max(df_table[,column_index]),column_index]
        min_Var1<-df_table[which.min(df_table[,column_index]),column_index]
        median_Var1<-round(median(df_table[,column_index],na.rm=TRUE), digits=2)

        #compute extreme outliers.
        lowerq <- quantile(df_table[,column_index],na.rm=TRUE)[2]
        upperq <- quantile(df_table[,column_index],na.rm=TRUE)[3]
        iqr <- upperq - lowerq # interquartile range
        extreme.threshold.upper <-(iqr * 3) + upperq
        extreme.threshold.lower <- lowerq - (iqr * 3)
        #cases outside the upper and lower threshold.
        total_data_points_upper_threshold<-nrow(subset(df_table,df_table[,column_index]>extreme.threshold.upper[[1]]) )
        total_data_points_below_lower_threshold<-nrow(subset(df_table,df_table[,column_index]<extreme.threshold.lower[[1]]) )
        outlier_message<-paste("Investigate ",total_data_points_upper_threshold," data points above the upper threshold (",extreme.threshold.upper, ") and ",total_data_points_below_lower_threshold,
                               " data points below the lower threshold (", extreme.threshold.lower,")",sep="")


        #create histogram
		p<-ggplot(data=dfTab, aes(x=Var1, y=Freq, width=1, fill=Freq)) +
		geom_bar(stat="identity", position="identity") + ggtitle(paste(field_name,": Distribution\n(Mean=",mean_Var1," Std. Deviation=",sd_Var1,")"))
		p<-p+ylab(paste(table_name,"Count"))+xlab(paste(field_name,"(in",unit,")"))
		#remove legend and set size and orientation of tick labels
		p<-p+theme(legend.position="none", text = element_text(size=6),axis.text.x = element_text(angle=90, vjust=1), plot.background = element_blank() ,panel.grid.major = element_blank() ,panel.grid.minor = element_blank() ,panel.border = element_blank())
		# flog.info(p)
        ggsave(file=paste(normalize_directory_path( g_config$reporting$site_directory),get_image_name(table_name,field_name),sep=""))


        return(paste("\n\nMaximum:",max_Var1 ,"Minimum:", min_Var1, "Mean:",mean_Var1,"Std Deviation:",sd_Var1,"Median:",median_Var1,"\n\n" ))
        }
	}
    }
    else #using dplyr
    {
     colnames(df_table)[1] <- "Var1"
     colnames(df_table)[2] <- "Freq"
     df_table<-subset(df_table,!is.na(Var1))

        if(nrow(df_table)>0)
        {


          #caluclating mean and standard deviation, etc.
          raw_vector<-rep.int(df_table$Var1, df_table$Freq)
          mean_Var1<-round(mean(raw_vector, na.rm=TRUE),2)
          sd_Var1<-round(sd(raw_vector, na.rm=TRUE),2)
          median_Var1<-median(mean(raw_vector, na.rm=TRUE),2)

          max_Var1<-df_table[which.max(df_table[,1]),1]
          min_Var1<-df_table[which.min(df_table[,1]),1]

          #  #compute extreme outliers.
          lowerq <- quantile(raw_vector,na.rm=TRUE)[2]
          upperq <- quantile(raw_vector,na.rm=TRUE)[3]
          iqr <- upperq - lowerq # interquartile range
          extreme.threshold.upper <-(iqr * 3) + upperq
          extreme.threshold.lower <- lowerq - (iqr * 3)
          #cases outside the upper and lower threshold.
          total_data_points_upper_threshold<-length(raw_vector[raw_vector>extreme.threshold.upper[[1]]])
          outlier_message<-""
          if(total_data_points_upper_threshold>0)
          {
            outlier_message<-paste("Investigate ",total_data_points_upper_threshold," data points above the upper threshold (",extreme.threshold.upper[[1]], ")"
                                   ,sep="")
          }
          if(extreme.threshold.lower[[1]]<0)
          {
            extreme.threshold.lower[[1]]<-0
          }
          total_data_points_below_lower_threshold<-length(raw_vector[raw_vector<extreme.threshold.lower[[1]]])

          if(total_data_points_below_lower_threshold>0)
          {
            outlier_message<-paste(outlier_message," Investigate ",total_data_points_below_lower_threshold,
                                   " data points below the lower threshold (", extreme.threshold.lower,")",sep="")
          }

         #create histogram
         p<-ggplot(data=df_table, aes(x=Var1, y=Freq, width=1, fill=Freq)) +
           geom_bar(stat="identity", position="identity") + ggtitle(paste(field_name,": Distribution\n(Mean=",mean_Var1," Std. Deviation=",sd_Var1,")"))
         p<-p+ylab(paste(table_name,"Count"))+xlab(paste(field_name,"(in",unit,")"))
         #remove legend and set size and orientation of tick labels
         p<-p+theme(legend.position="none", text = element_text(size=6),axis.text.x = element_text(angle=90, vjust=1), plot.background = element_blank() ,panel.grid.major = element_blank() ,panel.grid.minor = element_blank() ,panel.border = element_blank())
         # flog.info(p)
         ggsave(file=paste(normalize_directory_path( g_config$reporting$site_directory),get_image_name(table_name,field_name),sep=""))


         return(paste("\n\nMaximum:",max_Var1 ,"Minimum:", min_Var1, "Mean:",mean_Var1,"Std Deviation:",sd_Var1,"Median:",median_Var1,"\n\n" ))
        }
    }


}

# Foreign Key Fields
#functionName: describeForeignKeyIdentifiers
#Description: generate a barplot for a foreignkey field in a table
#Inputs:
	#1. an R dataframe containing a given database table
	#2. the name of the foreign key field
#Output: write the barplot to a file
describeForeignKeyIdentifiers<-function(df_table, table_name, field_name,big_data_flag)
{
    # flog.info(field_name)
	 flog.info(paste("Plotting for Field: ", field_name))

    if(big_data_flag==FALSE)
    {
	column_index <- which(colnames(df_table)==field_name)

	if(nrow(df_table)>0)
	{
		dfTab <- table(df_table[,column_index])

        if(nrow(dfTab)>0)
        {
		#calculate the total number of locations
		total_locations <- nrow(dfTab)
        # flog.info(total_locations)

        png(paste(normalize_directory_path( g_config$reporting$site_directory),get_image_name(table_name,field_name),sep=""))
		# not using ggplot here as it is very expensive for a large number of values
		barplot(dfTab, main = paste(field_name,": Distribution"), xlab = paste(field_name,"(Total: ",total_locations,")"), ylab = paste(table_name,"Count"), xaxt='n')


        #also plot in decreasing order of frequency (to compare distribution with source data)
        df_dfTab<-data.frame(dfTab)
        ordered_dfTab<-df_dfTab[order(-df_dfTab[,2]),]
        png(paste(normalize_directory_path( g_config$reporting$site_directory),get_image_name_sorted(table_name,field_name),sep=""))
        barplot(ordered_dfTab$Freq, main = paste(field_name,": Distribution"), xlab = paste(field_name,"(Total: ",total_locations,")"), ylab = paste(table_name,"Count"), xaxt='n')



        return_message<-paste("The most frequent values for",field_name,"are:")
        index<-1;
        while (index<=5)
        {
            if(is.na(ordered_dfTab[index,1]))
                return(return_message);
            return_message<-paste(return_message,ordered_dfTab[index,1]);
            if(index<5)
            return_message<-paste(return_message,",")
            index<-index+1;
        }

        # Turn off both png devices
        dev.off()
        dev.off()
        return(return_message);
        }
	}
    }

    #handling big data using dplyr
    else
    {

    colnames(df_table)[1] <- "Var1"
    colnames(df_table)[2] <- "Freq"
    # remove NA values
    df_table<-subset(df_table,!is.na(Var1))
    if(nrow(df_table)>0)
    {


        #df_table$Var1 <- as.factor(df_table$Var1)

        ordered_data<-df_table[order(-df_table[,2]), ]

        total_values<- length(unique(df_table$Var1))

        png(paste(normalize_directory_path( g_config$reporting$site_directory),get_image_name(table_name,field_name),sep=""))
        # not using ggplot here as it is very expensive for a large number of values
        barplot(df_table$Freq, main = paste(field_name,": Distribution"), xlab = paste(field_name,"(Total: ",total_values,")"), ylab = paste(table_name,"Count"), xaxt='n')


        #also plot in decreasing order of frequency (to compare distribution with source data)
        png(paste(normalize_directory_path( g_config$reporting$site_directory),get_image_name_sorted(table_name,field_name),sep=""))

        #ordered_vector_table <- as.vector(as.matrix(ordered_data$Freq))
        barplot(ordered_data$Freq, main = paste(field_name,": Distribution"), xlab = paste(field_name,"(Total: ",total_values,")"), ylab = paste(table_name,"Count"), xaxt='n')



        return_message<-paste("The most frequent values for",field_name,"are:")

        index<-1;
        while (index<=5)
        {
            return_message<-paste(return_message,ordered_data[index,1]);

            if(index<5)
            return_message<-paste(return_message,",")
            index<-index+1;
        }

        dev.off()
        dev.off()
        return(return_message)

    }
    }
}

#update values of concept_id fields to include concept names  - to improve readability for plotting purposes
EnhanceFieldValues<-function(df_table,field_name,df_ref)
{
  df_table_enhanced<-df_table
  column_index <- which(colnames(df_table_enhanced)==field_name)
  outer_loop_index =1;inner_loop_index =1;
  while(inner_loop_index<=nrow(df_ref))
  {
    #    df_table[,column_index][df_table[,column_index]==df_ref[inner_loop_index,1]] <- "replaced"
    df_table_enhanced[,column_index][df_table_enhanced[,column_index]==df_ref[inner_loop_index,1]] <- paste(df_ref[inner_loop_index,2]," (",df_ref[inner_loop_index,1],")",sep="");
    #df$concept_id[df==""]<-NA
    inner_loop_index<-inner_loop_index+1;

  }

      #  df_table[outer_loop_index,column_index]<-paste(df_ref[inner_loop_index,2]," (",df_ref[inner_loop_index,1],")",sep="");
     #  break;

  return(df_table_enhanced);
}
