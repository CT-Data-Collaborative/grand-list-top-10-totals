library(dplyr)
library(datapkg)
library(readxl)
library(stringi)

##################################################################
#
# Processing Script for Grand List Top 10 Totals
# Created by Jenna Daly
# On 05/22/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
data_location <- grep("raw", sub_folders, value=T)
path_to_raw_data <- (paste0(getwd(), "/", data_location))
gl_xlsx <- dir(path_to_raw_data, recursive=T, pattern = "Grand") 
gl_data <- (read_excel(paste0(path_to_raw_data, "/", gl_xlsx), sheet=1, skip=0)) 

#omit rows where is.na(Town)
gl_data <- gl_data[!is.na(gl_data$Town),]

#only grab columns we need
gl_data <- gl_data[,c(3,4,25,27,28)]

#rename column names
colnames(gl_data) <- c("Town", "Year", "Top 10 Total Grand List", "Total Grand List", "Net Grand List")

#Convert to long format
options(scipen=999)
cols_to_stack <- c("Top 10 Total Grand List", 
                   "Total Grand List", 
                   "Net Grand List")

long_row_count = nrow(gl_data) * length(cols_to_stack)

gl_data_long <- reshape(gl_data,
                              varying = cols_to_stack,
                              v.names = "Value",
                              timevar = "Variable",
                              times = cols_to_stack,
                              new.row.names = 1:long_row_count,
                              direction = "long"
)

gl_data_long$id <- NULL

#NOT WORKING
gl_na <- unique(gl_data_long[is.na(gl_data_long$`Value`),]$Town)

old_gl_data <- (read_excel(paste0(path_to_raw_data, "/", gl_xlsx), sheet=2, skip=0)) 
replace_gl_data <- old_gl_data[old_gl_data$Town %in% gl_na,]
replace_gl_data <- replace_gl_data[,c(3,4,25,27,28)]
colnames(replace_gl_data) <- c("Town", "Year", "Top 10 Total Grand List", "Total Grand List", "Net Grand List")

#remove towns that need to get replaced
gl_data_sub <- gl_data[!gl_data$Town %in% gl_na,]

#add in replacement df
gl_data_complete <- rbind(gl_data_sub, replace_gl_data)

