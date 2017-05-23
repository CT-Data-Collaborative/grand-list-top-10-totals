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

#Convert town names to correct case
gl_data_long$Town <- stri_trans_totitle(gl_data_long$Town)

#Merge in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

gl_data_long_fips <- merge(gl_data_long, fips, by = "Town", all=T)
#remove CT
gl_data_long_fips <- gl_data_long_fips[gl_data_long_fips$Town != "Connecticut",]

#Assign Measure Type
gl_data_long_fips$"Measure Type" <- "Number"

#Order columns
gl_data_long_fips <- gl_data_long_fips %>% 
  select(`Town`, `FIPS`, `Year`, `Variable`, `Measure Type`, `Value`) %>% 
  arrange(Town, Variable)

#Set New Canaan year one less than cutoff year (blank data)
gl_data_long_fips$Year <- as.numeric(gl_data_long_fips$Year)
gl_years <- unique(gl_data_long_fips$Year)
latest_year <- max(gl_years[!is.na(gl_years)])
cutoff_year <- latest_year - 2
gl_data_long_fips$Year[gl_data_long_fips$Town == "New Canaan"] <- (cutoff_year - 1)

#Code "Old" data to -6666 (data before 2014)
gl_data_long_fips$Value[gl_data_long_fips$Year < cutoff_year] <- -6666

# Write to File
write.table(
  gl_data_long_fips,
  file.path(getwd(), "data", "grand_list_top_10_totals_2016.csv"),
  sep = ",",
  na = "-6666",
  row.names = F
)


