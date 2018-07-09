###Test frame for fixed number of colors in patch 1_5

x = data.frame(cbind((sample.int(9,size=100,replace=TRUE)),
                     (sample.int(9,size=100,replace=TRUE)), 
                     (sample.int(9,size=100,replace=TRUE))))
colnames(x) = c("operator_concept_id", "holder", "second holder")
x[,1] = ifelse(x[,1] == 1,2000000033, x[,1])
x[,1] = ifelse(x[,1] == 2,2000000032, x[,1])
x[,1] = ifelse(x[,1] == 3,44818702, x[,1])
x[,1] = ifelse(x[,1] == 4,44818703, x[,1])
x[,1] = ifelse(x[,1] == 5,44818704, x[,1])
x[,1] = ifelse(x[,1] == 6,45754907, x[,1])
x[,1] = ifelse(x[,1] == 7,8876, x[,1])
x[,1] = ifelse(x[,1] == 8,8554, x[,1])
x[,1] = ifelse(x[,1] == 9, NA, x[,1])


order_bins <-c("2000000033","2000000032","44818702","44818703","44818704","45754907",NA)
label_bins<-c("Vital Sign from healthcare delivery setting (2000000033)","Vital Sign from healthcare device (2000000032)",
              "Lab result (44818702)","Pathology finding (44818703)","Patient reported value (44818704)",
              "Derived Value (45754907)","NULL")
color_bins <-c("2000000033"="lightcoral","2000000032"="steelblue1","44818702"="red","44818703"="grey64","44818704"="grey64","45754907"="grey64")

describeNominalField(x,table_name = "Operator_concept",field_name = "operator_concept_id",label_bins = 
                     label_bins,order_bins =  order_bins, color_bins = color_bins, big_data_flag = F, 
                     expected_levels = c("2000000032", "2000000033"))


