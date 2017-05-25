library(dplyr)
library(datapkg)
library(readxl)
library(stringi)
library(lubridate)

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
gl_xlsx_2016 <- dir(path_to_raw_data, recursive=T, pattern = "Grand") 
#gl_data <- (read_excel(paste0(path_to_raw_data, "/", gl_xlsx), sheet=1, skip=0)) 

#First bring in 2014 data set
gl_2014 <- (read_excel(paste0(path_to_raw_data, "/", gl_xlsx_2016), sheet=2, skip=0)) 

#omit rows where is.na(Town)
gl_2014 <- gl_2014[!is.na(gl_2014$Town),]

#only grab columns we need
gl_2014 <- gl_2014[,c(3,25,26,27,28,29)]

#Check to make sure years make sense (should return FALSE)

gl_2014$`Date Submitted` <- year(gl_2014$`Date Submitted`)
gl_2014$`Year-GL-TotalValue` <- as.numeric(gl_2014$`Year-GL-TotalValue`)
min_gl_year <- min(gl_2014$`Year-GL-TotalValue`, na.rm=T)
min_submit_year <- min(gl_2014$`Date Submitted`, na.rm=T)
max_gl_year <- max(gl_2014$`Year-GL-TotalValue`, na.rm=T)
max_submit_year <- max(gl_2014$`Date Submitted`, na.rm=T)

#Fix any towns with incorrect years
gl_2014$"Year-GL-TotalValue"[which(gl_2014$Town %in% c("Killingworth"))] <- 2014
gl_2014$"Date Submitted"[which(gl_2014$Town %in% c("North Branford"))] <- 2014

#rename column names
colnames(gl_2014) <- c("Town", "Top 10 Total Grand List", "Grand List Total Year", "Total Grand List", "Net Grand List", "Year Submitted")

#Convert to long format
options(scipen=999)
cols_to_stack <- c("Top 10 Total Grand List", 
                   "Total Grand List", 
                   "Net Grand List")

long_row_count = nrow(gl_2014) * length(cols_to_stack)

gl_2014_long <- reshape(gl_2014,
                              varying = cols_to_stack,
                              v.names = "Value",
                              timevar = "Variable",
                              times = cols_to_stack,
                              new.row.names = 1:long_row_count,
                              direction = "long"
)

gl_2014_long$id <- NULL

#Convert town names to correct case
gl_2014_long$Town <- stri_trans_totitle(gl_2014_long$Town)

#Merge in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

gl_2014_long_fips <- merge(gl_2014_long, fips, by = "Town", all=T)
#remove CT
gl_2014_long_fips <- gl_2014_long_fips[gl_2014_long_fips$Town != "Connecticut",]

#Assign Measure Type
gl_2014_long_fips$"Measure Type" <- "Number"

#Set profiles year
gl_2014_long_fips$"Town Profile Year" <- 2016

#Order columns
gl_2014_long_fips <- gl_2014_long_fips %>% 
  select(`Town`, `FIPS`, `Grand List Total Year`, `Year Submitted`, `Town Profile Year`, `Variable`, `Measure Type`, `Value`) %>% 
  arrange(Town, Variable)

#Set towns with blank years
#Three scenarios:
#1) GL year blank, submitted year present - set GL year = submitted year (Thompson, Lisbon)
blank1 <- gl_2014_long_fips[is.na(gl_2014_long_fips$`Grand List Total Year`),]
blank1 <- unique(blank1[!is.na(blank1$`Year Submitted`),]$Town)

#2) GL year present, submitted year blank - set submitted year to GL year (Voluntown)
blank2 <- gl_2014_long_fips[is.na(gl_2014_long_fips$`Year Submitted`),]
blank2 <- unique(blank2[!is.na(blank2$`Grand List Total Year`),]$Town)

#3) GL year blank, submitted year blank - set both years to cutoff year (Morris, Naugatuck, New Canaan)
blank3 <- gl_2014_long_fips[is.na(gl_2014_long_fips$`Year Submitted`),]
blank3 <- unique(blank3[is.na(blank3$`Grand List Total Year`),]$Town)

#Set years accordingly
latest_year <- max(gl_2014_long_fips$`Town Profile Year`, na.rm=T)
cutoff_year <- latest_year - 3
gl_2014_long_fips <- gl_2014_long_fips %>% 
  mutate(`Grand List Total Year` = ifelse(Town %in% blank1, `Year Submitted`, `Grand List Total Year`), 
         `Year Submitted` = ifelse(Town %in% blank2, `Grand List Total Year`, `Year Submitted`), 
         `Grand List Total Year` = ifelse(Town %in% blank3, cutoff_year, `Grand List Total Year`), 
         `Year Submitted` = ifelse(Town %in% blank3, cutoff_year, `Year Submitted`))

#Code "Old" data to -6666 (data before cutoff year)
gl_2014_long_fips$Value[gl_2014_long_fips$`Grand List Total Year` < cutoff_year] <- -6666

# Write to File
write.table(
  gl_2014_long_fips,
  file.path(getwd(), "data", "grand_list_top_10_totals_2014.csv"),
  sep = ",",
  na = "-6666",
  row.names = F
)


