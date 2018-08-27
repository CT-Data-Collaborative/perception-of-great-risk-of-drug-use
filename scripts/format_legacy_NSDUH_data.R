library(dplyr)
library(datapkg)
library(tidyr)
library(writexl)

##Creating legacy NSDUH data in same format as new data

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))
raw_sub_folders <- list.dirs(path = path_to_raw, recursive=F)

table_master_list <- data.frame(stringsAsFactors = F)
for (i in 1:length(raw_sub_folders)) {
  table_list_file <- grep(".txt", list.files(raw_sub_folders[i]), value=T)
  table_list <- read.delim(paste0(raw_sub_folders[i], "/", table_list_file), header=F)
  table_list <- table_list %>% separate(V1, c("Table Number", "Table Description"), sep = " –")
  table_list$`Table Number` <- gsub(" ", "_", tolower(table_list$`Table Number`))
  get_year <- as.numeric(substr((unlist(gsub("[^0-9]", "", unlist(raw_sub_folders[i])), "")), 1, 4))
  get_year <- paste0(get_year, "-", get_year+2)
  table_list$Year <- get_year
  table_master_list <- rbind(table_master_list, table_list)
}

#table_master_list <- table_master_list[grep("Perception", table_master_list$`Table Description`),]

#get list all al table/description pairs for each year, use these pairs to assign tables as they are read in
get_years <- unique(table_master_list$Year)

for (i in 1:length(get_years)) {
  subfolder <- grep(get_years[i], raw_sub_folders, value=T)
  all_tables <- grep(".csv", list.files(subfolder), value=T)
  for (j in 1:length(all_tables)) {
    if (get_years[i] == "2004-2006") {
      current_table <- read.csv(paste0(subfolder, "/", all_tables[j]), stringsAsFactors = F, header=F, check.names = F)
    } else {
      current_table <- read.csv(paste0(subfolder, "/", all_tables[j]), stringsAsFactors = F, header=T, check.names = F)
    }
    #assign year and table number
    get_year <- as.numeric(substr((unlist(gsub("[^0-9]", "", unlist(subfolder)), "")), 1, 4))
    get_year2 <- paste0(get_year, "-", get_year+2)
    get_year3 <- paste0(get_year, "_", get_year+2)
    current_table$Year <- get_year2
    tbl_number <- gsub(".csv", "", all_tables[j])
    current_table$`Table Number` <- tbl_number
    current_table <- merge(current_table, table_master_list, by = c("Year", "Table Number"), all.x=T)
    assign(paste0(tbl_number, "_", get_year3), current_table)
  }
}

#assign excel sheets by year
sheets06 <- list("2004-2006 Table 1" = table_1_2004_2006, 
               "2004-2006 Table 2" = table_2_2004_2006,
               "2004-2006 Table 3" = table_3_2004_2006, 
               "2004-2006 Table 4" = table_4_2004_2006,
               "2004-2006 Table 5" = table_5_2004_2006, 
               "2004-2006 Table 6" = table_6_2004_2006,
               "2004-2006 Table 7" = table_7_2004_2006, 
               "2004-2006 Table 8" = table_8_2004_2006,
               "2004-2006 Table 9" = table_9_2004_2006, 
               "2004-2006 Table 10" = table_10_2004_2006,
               "2004-2006 Table 11" = table_11_2004_2006, 
               "2004-2006 Table 12" = table_12_2004_2006,
               "2004-2006 Table 13" = table_13_2004_2006, 
               "2004-2006 Table 14" = table_14_2004_2006,
               "2004-2006 Table 15" = table_15_2004_2006, 
               "2004-2006 Table 16" = table_16_2004_2006,
               "2004-2006 Table 17" = table_17_2004_2006, 
               "2004-2006 Table 18" = table_18_2004_2006,
               "2004-2006 Table 19" = table_19_2004_2006, 
               "2004-2006 Table 20" = table_20_2004_2006,
               "2004-2006 Table 21" = table_21_2004_2006,
               "2004-2006 Table 22" = table_22_2004_2006,
               "2004-2006 Table 23" = table_23_2004_2006)
write_xlsx(sheets06, paste0(path_to_raw, "/", "NSDUHsubstateAgeGroupTabs2006.xlsx"))

sheets08 <- list("2006-2008 Table 1" = table_1_2006_2008, 
               "2006-2008 Table 2" = table_2_2006_2008,
               "2006-2008 Table 3" = table_3_2006_2008, 
               "2006-2008 Table 4" = table_4_2006_2008,
               "2006-2008 Table 5" = table_5_2006_2008, 
               "2006-2008 Table 6" = table_6_2006_2008,
               "2006-2008 Table 7" = table_7_2006_2008, 
               "2006-2008 Table 8" = table_8_2006_2008,
               "2006-2008 Table 9" = table_9_2006_2008, 
               "2006-2008 Table 10" = table_10_2006_2008,
               "2006-2008 Table 11" = table_11_2006_2008, 
               "2006-2008 Table 12" = table_12_2006_2008,
               "2006-2008 Table 13" = table_13_2006_2008, 
               "2006-2008 Table 14" = table_14_2006_2008,
               "2006-2008 Table 15" = table_15_2006_2008, 
               "2006-2008 Table 16" = table_16_2006_2008,
               "2006-2008 Table 17" = table_17_2006_2008, 
               "2006-2008 Table 18" = table_18_2006_2008,
               "2006-2008 Table 19" = table_19_2006_2008, 
               "2006-2008 Table 20" = table_20_2006_2008,
               "2006-2008 Table 21" = table_21_2006_2008,
               "2006-2008 Table 22" = table_22_2006_2008)
write_xlsx(sheets08, paste0(path_to_raw, "/", "NSDUHsubstateAgeGroupTabs2008.xlsx"))

sheets10 <- list("2008-2010 Table 1" = table_1_2008_2010, 
               "2008-2010 Table 2" = table_2_2008_2010,
               "2008-2010 Table 3" = table_3_2008_2010, 
               "2008-2010 Table 4" = table_4_2008_2010,
               "2008-2010 Table 5" = table_5_2008_2010, 
               "2008-2010 Table 6" = table_6_2008_2010,
               "2008-2010 Table 7" = table_7_2008_2010, 
               "2008-2010 Table 8" = table_8_2008_2010,
               "2008-2010 Table 9" = table_9_2008_2010, 
               "2008-2010 Table 10" = table_10_2008_2010,
               "2008-2010 Table 11" = table_11_2008_2010, 
               "2008-2010 Table 12" = table_12_2008_2010,
               "2008-2010 Table 13" = table_13_2008_2010, 
               "2008-2010 Table 14" = table_14_2008_2010,
               "2008-2010 Table 15" = table_15_2008_2010, 
               "2008-2010 Table 16" = table_16_2008_2010,
               "2008-2010 Table 17" = table_17_2008_2010, 
               "2008-2010 Table 18" = table_18_2008_2010,
               "2008-2010 Table 19" = table_19_2008_2010, 
               "2008-2010 Table 20" = table_20_2008_2010,
               "2008-2010 Table 21" = table_21_2008_2010,
               "2008-2010 Table 22" = table_22_2008_2010,
               "2008-2010 Table 23" = table_23_2008_2010)
write_xlsx(sheets10, paste0(path_to_raw, "/", "NSDUHsubstateAgeGroupTabs2010.xlsx"))

sheets12 <- list("2010-2012 Table 1" = table_1_2010_2012, 
               "2010-2012 Table 2" = table_2_2010_2012,
               "2010-2012 Table 3" = table_3_2010_2012, 
               "2010-2012 Table 4" = table_4_2010_2012,
               "2010-2012 Table 5" = table_5_2010_2012, 
               "2010-2012 Table 6" = table_6_2010_2012,
               "2010-2012 Table 7" = table_7_2010_2012, 
               "2010-2012 Table 8" = table_8_2010_2012,
               "2010-2012 Table 9" = table_9_2010_2012, 
               "2010-2012 Table 10" = table_10_2010_2012,
               "2010-2012 Table 11" = table_11_2010_2012, 
               "2010-2012 Table 12" = table_12_2010_2012,
               "2010-2012 Table 13" = table_13_2010_2012, 
               "2010-2012 Table 14" = table_14_2010_2012,
               "2010-2012 Table 15" = table_15_2010_2012, 
               "2010-2012 Table 16" = table_16_2010_2012,
               "2010-2012 Table 17" = table_17_2010_2012, 
               "2010-2012 Table 18" = table_18_2010_2012,
               "2010-2012 Table 19" = table_19_2010_2012, 
               "2010-2012 Table 20" = table_20_2010_2012,
               "2010-2012 Table 21" = table_21_2010_2012,
               "2010-2012 Table 22" = table_22_2010_2012,
               "2010-2012 Table 23" = table_23_2010_2012)
write_xlsx(sheets12, paste0(path_to_raw, "/", "NSDUHsubstateAgeGroupTabs2012.xlsx"))
