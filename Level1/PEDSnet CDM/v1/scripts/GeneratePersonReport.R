library(DBI)
library(yaml)

config = yaml.load_file(g_config_path)

#establish connection to database
con <- establish_database_connection_OHDSI(config)

# read a database table into an R dataframe
table_name<-"person"
df_person <- retrieve_dataframe_OHDSI(con, config,table_name)

#writing to the final DQA Report
fileConn<-file(paste(normalize_directory_path(config$reporting$site_directory),"./reports/",table_name,"_Report_Automatic.md",sep=""))
fileContent <-get_report_header(table_name,config)

#setting up big data flag
big_data_flag<-FALSE

#PRIMARY FIELD
field_name<-"person_id"
fileContent<-c(fileContent,paste("The total number of",field_name,"is: ", describeIdentifier(df_person,field_name),"\n"))
field_name<-"person_source_value"
fileContent<-c(fileContent,paste("The total number of",field_name,"is: ", describeIdentifier(df_person,field_name),"\n"))

#NOMINAL Fields

#Gender Concept Id
field_name = "gender_concept_id"
order_bins <-c("8507","8532","8551","8521","44814667",NA)
label_bins<-c("Male (8507)","Female (8532)","Unknown (8551)","Other (8521)","No Information (44814667)","NULL")
color_bins <-c("8507"="lightcoral","8532"="steelblue1","8551"="grey64","8521"="grey64","44814667"="grey64")
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
null_message<-reportNullFlavors(df_person,table_name,field_name,8551,8521,44814667, big_data_flag)
# flog.info(null_message)
unexpected_message<- reportUnexpected(df_person,table_name,field_name,order_bins,big_data_flag)
# flog.info(unexpected_message)
fileContent<-c(fileContent,unexpected_message)
describeNominalField(df_person,table_name,field_name, label_bins, order_bins,color_bins, big_data_flag)
fileContent<-c(fileContent,null_message,paste_image_name(table_name,field_name));

#Gender Source Value
field_name="gender_source_value"
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
missing_message<-reportMissingCount(df_person,table_name,field_name,big_data_flag);
fileContent<-c(fileContent,missing_message)
describeNominalField_basic(df_person,table_name,field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name));

#Race Concept Id
## reading specific subset of the concept table to retrieve race concepts
df_race <-retrieve_dataframe_clause(con,config,"concept","concept_id,concept_name","CONCEPT_CLASS='Race' and VOCABULARY_ID in (13,60)")
order_bins <-c(df_race$concept_id,NA)

field_name="race_concept_id"
null_message<-reportNullFlavors(df_person,table_name,field_name,8552,8522,44814661, big_data_flag)
# flog.info(null_message)
unexpected_message<- reportUnexpected(df_person,table_name,field_name,order_bins,big_data_flag)
# flog.info(unexpected_message)
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,unexpected_message)
#update values of the field before plotting
df_person_race_enhanced<-EnhanceFieldValues(df_person,field_name,df_race);
describeNominalField_basic(df_person_race_enhanced,table_name,field_name,big_data_flag);
fileContent<-c(fileContent,null_message,paste_image_name(table_name,field_name));


#Race Source Value
field_name="race_source_value"
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
missing_message<-reportMissingCount(df_person,table_name,field_name,big_data_flag);
fileContent<-c(fileContent,missing_message)
describeNominalField_basic(df_person,table_name,field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name));

#Ethnicity Concept Id
order_bins <-c("38003563","38003564","44814653","44814649","44814650",NA)
label_bins<-c("HISPANIC OR LATINO(38003563)","NOT HISPANIC OR LATINO(38003564)","UNKNOWN(44814653)","OTHERS(44814649)","NO INFORMATION (44814650)","NULL")
color_bins <-c("38003563"="lightcoral","38003564"="steelblue1","44814653"="grey64","44814649"="grey64","44814650"="grey64")
field_name="ethnicity_concept_id"
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))

null_message<-reportNullFlavors(df_person,table_name,field_name,44814653,44814649,44814650, big_data_flag)
# flog.info(null_message)
unexpected_message<- reportUnexpected(df_person,table_name,field_name,order_bins,big_data_flag)
# flog.info(paste("Unexpected Values:",unexpected_message))
fileContent<-c(fileContent,unexpected_message)
describeNominalField(df_person,table_name,field_name, label_bins, order_bins,color_bins,big_data_flag)
fileContent<-c(fileContent,null_message,paste_image_name(table_name,field_name));


#Ethnicity Source Value
field_name="ethnicity_source_value"
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_person,table_name,field_name,big_data_flag))
describeNominalField_basic(df_person,table_name,field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name));

# ORDINAL Fields
#Year of Birth
field_name="year_of_birth"
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_person,table_name,field_name,big_data_flag))
describeOrdinalField(df_person, table_name, field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name));

#Month of Birth
field_name="month_of_birth"
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_person,table_name,field_name,big_data_flag))
describeOrdinalField(df_person, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name));

#Day of Birth
field_name="day_of_birth"
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_person,table_name,field_name,big_data_flag))
describeOrdinalField(df_person,table_name, field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name));

#Time of Birth --
field_name<-"pn_time_of_birth"
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_person,table_name,field_name,big_data_flag))
message<-describeTimeField(df_person, table_name,field_name,big_data_flag)
fileContent<-c(fileContent,message,paste_image_name(table_name,paste(field_name,"_time",sep="")));

#RATIO Fields

#pn_gestational_field
field_name<-"pn_gestational_age"
unit<-"weeks"
fileContent <-c(fileContent,paste("## Histogram for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_person,table_name,field_name,big_data_flag))
message<-describeRatioField(df_person, table_name, field_name, unit,big_data_flag)
fileContent<-c(fileContent,message,paste_image_name(table_name,field_name));

#FOREIGN KEY fields
#LocationID
field_name<-"location_id"
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_person,table_name,field_name,big_data_flag))
message<-describeForeignKeyIdentifiers(df_person,table_name,field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

#provider_id
field_name="provider_id"
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_person,table_name,field_name,big_data_flag))
message<-describeForeignKeyIdentifiers(df_person,table_name,field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);

#Care site id
field_name="care_site_id"
fileContent <-c(fileContent,paste("## Barplot for",field_name,"","\n"))
fileContent<-c(fileContent,reportMissingCount(df_person,table_name,field_name,big_data_flag))
message<-describeForeignKeyIdentifiers(df_person,table_name,field_name,big_data_flag)
fileContent<-c(fileContent,paste_image_name(table_name,field_name),paste_image_name_sorted(table_name,field_name),message);


#write all contents to the report file and close it.
writeLines(fileContent, fileConn)
close(fileConn)

#close the connection
close_database_connection_OHDSI(con,config)
