library(dplyr)
library(datapkg)
library(readxl)
library(stringi)
library(lubridate)
library(stringr)
library(tidyr)

##################################################################
#
# Processing Script for Grand List Top 10 Totals
# Created by Jenna Daly
# On 06/01/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
data_location <- grep("raw", sub_folders, value=T)
path_to_raw_data <- (paste0(getwd(), "/", data_location))
gl_xlsx <- dir(path_to_raw_data, recursive=T, pattern = "^Grand") 

#Isolate years used for profiles
gl_2017df <- (read_excel(paste0(path_to_raw_data, "/", gl_xlsx), sheet=1, skip=0)) 
gl_2016df <- (read_excel(paste0(path_to_raw_data, "/", gl_xlsx), sheet=2, skip=0)) 
gl_2014df <- (read_excel(paste0(path_to_raw_data, "/", gl_xlsx), sheet=3, skip=0)) 

tp_years <- c("2016", "2017")
#dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
gl_data <- c("gl_2014df", "gl_2016df")

#First process 2017, then use 2016 and 2014 to backfill if necessary
gl_2017 <- gl_2017df %>% 
  select(`What's your Town name?`, `Top 10 Total Grand List`, `Total Grand List`, `Net Grand List`, `Grand List Notes`)
gl_2017 <- gl_2017[!is.na(gl_2017$`What's your Town name?`),]


x2017_towns <- unique(gl_2017df$`What's your Town name?`)[!is.na(unique(gl_2017df$`Top 10 Total Grand List`))]
backfill_towns <- gl_2017df$Town[is.na(gl_2017df$`What's your Town name?`)]
backfill_towns2 <- gl_2017df$Town[is.na(gl_2017df$`Top 10 Business Names`)]

gl_2017 <- gl_2017df[gl_2017df$Town %in% x2017_towns,]

#format years
gl_2017$Year <- NA
gl_2017$Year <- str_sub(gsub("[^0-9.]", "", gl_2017$`Grand List Notes`), -4, -1)
gl_2017$Year[!grepl("201", gl_2017$Year)] <- NA
gl_2017$Year[is.na(gl_2017$Year)] <- "2017"
gl_2017$`Year Submitted` <- "2018"
gl_2017$`Town Profile Year` <- "2018"

#reformat numeric columns 
#function to gsub multiple characters in string
mgsub <- function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}

#apply function
gl_2017$`Top 10 Total Grand List` <- mgsub(c("\\$"," ",","), c("","",""), gl_2017$`Top 10 Total Grand List`)
gl_2017$`Total Grand List` <- mgsub(c("\\$"," ",","), c("","",""), gl_2017$`Total Grand List`)
gl_2017$`Net Grand List` <- mgsub(c("\\$"," ",","), c("","",""), gl_2017$`Net Grand List`)


#preliminary formatting
gl_2017 <- gl_2017 %>% 
  select(`What's your Town name?`, FIPS, Year, `Year Submitted`, `Town Profile Year`, `Top 10 Total Grand List`, `Total Grand List`, `Net Grand List`) %>% 
  rename(Town = `What's your Town name?`) %>% 
  arrange(Town)

#remove towns with no totals
towns3 <- gl_2017$Town[is.na(gl_2017$`Top 10 Total Grand List`) & is.na(gl_2017$`Total Grand List`) & is.na(gl_2017$`Net Grand List`)]
gl_2017 <- gl_2017[!gl_2017$Town %in% towns3,]

gl_2017_long <- gather(gl_2017, Variable, Value, 6:8, factor_key=F)
gl_2017_long$`Measure Type` <- "Number"

#################################################################################################################################
#Get data from previous years to backfill 2017 gl
#omit rows where is.na(Town)
#only grab columns we need
#rename column names
#set profile year

complete_gl <- data.frame(stringsAsFactors = F)
for (i in 1:length(gl_data)) {
  current_year <- get(gl_data[i])
  current_year <- current_year[!is.na(current_year$Town),]
  current_year <- current_year[,c(3,25,26,27,28,29)]
  current_year$`Date Submitted` <- year(current_year$`Date Submitted`)
  colnames(current_year) <- c("Town", "Top 10 Total Grand List", "Year", "Total Grand List", "Net Grand List", "Year Submitted")
  current_year$`Town Profile Year` <- tp_years[i]
  complete_gl <- rbind(complete_gl, current_year)
}

#Check to make sure years make sense
complete_gl$`Year` <- as.numeric(complete_gl$`Year`)
complete_gl$`Town Profile Year` <- as.numeric(complete_gl$`Town Profile Year`)
min_gl_year <- min(complete_gl$`Year`, na.rm=T)
min_submit_year <- min(complete_gl$`Year Submitted`, na.rm=T)
max_gl_year <- max(complete_gl$`Year`, na.rm=T)
max_submit_year <- max(complete_gl$`Year Submitted`, na.rm=T)

#Fix any towns with incorrect years
complete_gl$"Year"[which(complete_gl$Town %in% c("Killingworth"))] <- 2014
complete_gl$"Year Submitted"[which(complete_gl$Town %in% c("North Branford"))] <- 2014
complete_gl$"Year"[which(complete_gl$Town %in% c("Harwinton"))] <- 2016

#Convert to long format
options(scipen=999)
cols_to_stack <- c("Top 10 Total Grand List", 
                   "Total Grand List", 
                   "Net Grand List")

long_row_count = nrow(complete_gl) * length(cols_to_stack)

complete_gl_long <- reshape(complete_gl,
                              varying = cols_to_stack,
                              v.names = "Value",
                              timevar = "Variable",
                              times = cols_to_stack,
                              new.row.names = 1:long_row_count,
                              direction = "long"
)

complete_gl_long$id <- NULL

#Convert town names to correct case
complete_gl_long$Town <- stri_trans_totitle(complete_gl_long$Town)

#Merge in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

complete_gl_long_fips <- merge(complete_gl_long, fips, by = "Town", all=T)
#remove CT
complete_gl_long_fips <- complete_gl_long_fips[complete_gl_long_fips$Town != "Connecticut",]

#Assign Measure Type
complete_gl_long_fips$"Measure Type" <- "Number"

#Order columns
complete_gl_long_fips <- complete_gl_long_fips %>% 
  select(`Town`, `FIPS`, `Year`, `Year Submitted`, `Town Profile Year`, `Variable`, `Measure Type`, `Value`) %>% 
  arrange(Town, `Town Profile Year`, Variable)

#Set towns with blank years
#Three scenarios:
#1) GL year blank, submitted year present - set GL year = submitted year (Ansonia, Thompson, Lisbon)
blank1 <- complete_gl_long_fips[is.na(complete_gl_long_fips$`Year`),]
blank1 <- unique(blank1[!is.na(blank1$`Year Submitted`),]$Town)

#2) GL year present, submitted year blank - set submitted year to GL year (Voluntown)
blank2 <- complete_gl_long_fips[is.na(complete_gl_long_fips$`Year Submitted`),]
blank2 <- unique(blank2[!is.na(blank2$`Year`),]$Town)

#3) GL year blank, submitted year blank - set both years to cutoff year (Morris, Naugatuck, New Canaan)
blank3 <- complete_gl_long_fips[is.na(complete_gl_long_fips$`Year Submitted`),]
blank3 <- unique(blank3[is.na(blank3$`Year`),]$Town)

#Set years accordingly
#this accounts for when towns may only have blanks in one year, but not another year, don't want to overwrite the correct year
complete_gl_long_fips <- complete_gl_long_fips %>%
  mutate(`Year` = ifelse((Town %in% blank1) & (is.na(`Year`)), (`Year Submitted` - 1), `Year`), 
         `Year Submitted` = ifelse((Town %in% blank2) & (is.na(`Year Submitted`)), `Year`, `Year Submitted`),
         `Year` = ifelse((Town %in% blank3) & (is.na(`Year`)) & (is.na(`Year Submitted`)), (`Town Profile Year` - 3), `Year`), #fills it in only if both years are blank
         `Year Submitted` = ifelse((Town %in% blank3) & (is.na(`Year Submitted`)), (`Town Profile Year` - 3), `Year Submitted`))  #now that year is filled in, only need to check on year submitted is blank

##Special cases where year comes from individual gl col not total gl col
#set Naugatuck Year and Year Submitted to 2009 for 2016 profiles
complete_gl_long_fips$"Year"[which(complete_gl_long_fips$Town %in% c("Naugatuck") & (complete_gl_long_fips$`Town Profile Year` == 2016))] <- 2009
complete_gl_long_fips$"Year Submitted"[which(complete_gl_long_fips$Town %in% c("Naugatuck") & (complete_gl_long_fips$`Town Profile Year` == 2016))] <- 2009
#set Ansonia Year and Year Submitted to 2015 for 2017 profiles
complete_gl_long_fips$"Year"[which(complete_gl_long_fips$Town %in% c("Ansonia") & (complete_gl_long_fips$`Town Profile Year` == 2017))] <- 2015
complete_gl_long_fips$"Year Submitted"[which(complete_gl_long_fips$Town %in% c("Ansonia") & (complete_gl_long_fips$`Town Profile Year` == 2017))] <- 2015

#################################################################################################################################
#Backfill 2017 gl values with data from 2016 gl
bf_towns <- fips$Town[!fips$Town %in% gl_2017$Town]
bf_towns <- bf_towns[-1] #remove ct
bf_from_2016 <- complete_gl_long_fips[complete_gl_long_fips$Town %in% bf_towns & complete_gl_long_fips$`Town Profile Year` == "2017",]
bf_from_2016$`Town Profile Year` <- "2018"

final_gl <- rbind(gl_2017_long, bf_from_2016, complete_gl_long_fips)

final_gl <- final_gl %>% 
  select(Town, FIPS, Year, `Year Submitted`, `Town Profile Year`, Variable, `Measure Type`, Value) %>% 
  arrange(Town, Variable, `Town Profile Year`) 

# Write to File
write.table(
  final_gl,
  file.path(getwd(), "data", "grand_list_top_10_totals_2014_2017.csv"),
  sep = ",",
  na = "-666666",
  row.names = F
)
