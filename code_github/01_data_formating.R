
library(readxl)
library(dplyr)
library(jalcal)
library(lubridate)


Balkh_2010 <- read_excel("D:/Phenology/Raw_data/Balkh/2010_mazar.xlsx")
Balkh_2017 <- read_excel("D:/Phenology/Raw_data/Balkh/2017_mazar.xlsx")
Balkh_2018 <- read_excel("D:/Phenology/Raw_data/Balkh/2018_mazar.xlsx")



# for subset/chosing columns, column renames is required (file names had unusual space and dots)
process_data <- function(file_path, Year) {
  
Balkh_2021 <- read_excel(file_path)
Balkh_2021 <- Balkh_2021 %>% 
  rename(Plot_no = `plot no.`,
         Clone_no = `clone no.`,
         Variety_name = `variety name`,
         Bud_burst = `bud burst date`,
         Flower_10 = `0.1`,
         Flower_50 = `0.5`,
         Flower_90 = `0.9`,
         Petal_fall = `petal fall`)

# Subset now
Balkh_2021 <- subset(Balkh_2021, select = c(Plot_no, Clone_no, Variety_name, Bud_burst,
                                            Flower_10, Flower_50, Flower_90, Petal_fall))

# delete row number two
delete_row_1 <- 1
Balkh_2021 <- Balkh_2021[-delete_row_1,]

# add 1399 to each value in the date columns
Balkh_2021$Flower_10 <- paste(Balkh_2021$Flower_10, Year, sep = ".")
Balkh_2021$Flower_50 <- paste(Balkh_2021$Flower_50, Year, sep = ".")
Balkh_2021$Flower_90 <- paste(Balkh_2021$Flower_90, Year, sep = ".")
Balkh_2021$Petal_fall <- paste(Balkh_2021$Petal_fall, Year, sep = ".")

# Make all date columns in same format
Balkh_2021$Flower_10 <- gsub("\\.", "/", Balkh_2021$Flower_10)
Balkh_2021$Flower_50 <- gsub("\\.", "/", Balkh_2021$Flower_50)
Balkh_2021$Flower_90 <- gsub("\\.", "/", Balkh_2021$Flower_90)
Balkh_2021$Petal_fall <- gsub("\\.", "/", Balkh_2021$Petal_fall)

# covert dates into Gregorian dates format
# Function to convert Hijri Shamsi date to Gregorian date
convert_to_gregorian <- function(date_str) {
  # Split the date string into day, month, and year components
  components <- as.numeric(unlist(strsplit(date_str, "/")))
  day <- components[1]
  month <- components[2]
  year <- components[3]
  
  # Convert the date to Gregorian using jal2greg() function
  greg_date <- jal2greg(year, month, day)
  
  # Format the Gregorian date as a character string in "dd-mm-yyyy" format
  greg_date_formatted <- format(as.Date(greg_date, origin = "01-01-1970"), "%d-%m-%Y")
  
  # Return the formatted Gregorian date
  return(greg_date_formatted)
}

# Apply the conversion function to each date in my columns and get new columns 
Balkh_2021$bud_burst <- sapply(Balkh_2021$Bud_burst, convert_to_gregorian)
Balkh_2021$bloom_10 <- sapply(Balkh_2021$Flower_10, convert_to_gregorian)
Balkh_2021$bloom_50 <- sapply(Balkh_2021$Flower_50, convert_to_gregorian)
Balkh_2021$bloom_90 <- sapply(Balkh_2021$Flower_90, convert_to_gregorian)
Balkh_2021$petal_fall <- sapply(Balkh_2021$Petal_fall, convert_to_gregorian)

# Delete the colmns with shamsi dates
Balkh_2021 <- subset(Balkh_2021, select = c(Plot_no, Clone_no,  Variety_name, bud_burst,
                     bloom_10,  bloom_50, bloom_90, petal_fall))

return(Balkh_2021)

}

# Apply function to my data
path_2021_balkh <- "D:/Phenology/Raw_data/Balkh/2021_mazar.xlsx"
year <- 1399
Balkh_2021 <- process_data(path_2021_balkh, Year = year)
Balkh_2021 <- subset(Balkh_2021, select = -Plot_no)

path_2023_balkh <- "D:/Phenology/Raw_data/Balkh/2023_mazar.xlsx"
year <- 1401
Balkh_2023 <- process_data(path_2023_balkh, Year = year)
Balkh_2023 <- subset(Balkh_2023, select = -Plot_no)


#############################Kunduz Kunduz Kunduz############################
#############################Kunduz Kunduz Kunduz############################
#############################Kunduz Kunduz Kunduz#############################

# Set the working directory to the specified path
path <- "D:/Phenology/Raw_data/PHDC/Kunduz/"


kunduz_2011 <- read_excel("D:/Phenology/Raw_data/Kunduz/2011_kunduz.xlsx")
kunduz_2012 <- read_excel("D:/Phenology/Raw_data/Kunduz/2012_kunduz.xlsx")
kunduz_2013 <- read_excel("D:/Phenology/Raw_data/Kunduz/2013_kunduz.xlsx")
kunduz_2014 <- read_excel("D:/Phenology/Raw_data/Kunduz/2014_kunduz.xlsx")
kunduz_2015 <- read_excel("D:/Phenology/Raw_data/Kunduz/2015_kunduz.xlsx")
kunduz_2016 <- read_excel("D:/Phenology/Raw_data/Kunduz/2016_kunduz.xlsx")
kunduz_2017 <- read_excel("D:/Phenology/Raw_data/Kunduz/2017_kunduz.xlsx")
kunduz_2018 <- read_excel("D:/Phenology/Raw_data/Kunduz/2018_kunduz.xlsx")
kunduz_2019 <- read_excel("D:/Phenology/Raw_data/Kunduz/2019_kunduz.xlsx")
kunduz_2020 <- read_excel("D:/Phenology/Raw_data/Kunduz/2020_kunduz.xlsx")
kunduz_2021 <- read_excel("D:/Phenology/Raw_data/Kunduz/2021_kunduz.xlsx")
kunduz_2022 <- read_excel("D:/Phenology/Raw_data/Kunduz/2022_kunduz.xlsx")
kunduz_2023 <- read_excel("D:/Phenology/Raw_data/Kunduz/2023_kunduz.xlsx")

# change the date format of 2023 (because it was different having comma)
kunduz_2023 <- kunduz_2023 %>%
  mutate(`Bud burst` = as.Date(`Bud burst`, format = "%d,%m,%Y"),
         `0.1` = as.Date(`0.1`, format = "%d,%m,%Y"),
         `0.5` = as.Date(`0.5`, format = "%d,%m,%Y"),
         `0.9` = as.Date(`0.9`, format = "%d,%m,%Y"),
         `Petal fall` = as.Date(`Petal fall`, format = "%d,%m,%Y"))


kunduz_list <- list(kunduz_2011, kunduz_2012, kunduz_2013, kunduz_2014, kunduz_2015,
                    kunduz_2016, kunduz_2017, kunduz_2018, kunduz_2019, kunduz_2020,
                    kunduz_2021, kunduz_2022, kunduz_2023)

# Column renaming for all the years and choosing desired columns
for (i in seq_along(kunduz_list)) {
  kunduz_list[[i]] <- kunduz_list[[i]] %>% 
    select(Plot_no = 1,
           Clone_no = 2,
           Variety_name = 3,
           Bud_burst = 5,
           Flower_10 = 6,
           Flower_50 = 7,
           Flower_90 = 8,
           Petal_fall = 9)
}

# Rename the dataframes
names(kunduz_list) <- paste0("kunduz_", 2011:2023)

# update the unlist dataframe in the environment
list2env(kunduz_list, .GlobalEnv)

# use common clone numbers for all the years

# to make them common, lets first add a zero '0' and then 'AFG' to all clone number 
# as some years does not have it

years <- 2017:2023

# Iterate over each year and modify the Clone_no column
for (year in years) {
  assign(paste0("kunduz_", year), 
         get(paste0("kunduz_", year)) %>%
           mutate(Clone_no = ifelse(nchar(Clone_no) == 3, paste0("0", Clone_no), Clone_no)))
  
  # I will also update my kunduz_list with the modified dataframe
  kunduz_list[[paste0("kunduz_", year)]] <- get(paste0("kunduz_", year))
}

# now add AFG
for (year in years) {
  assign(paste0("kunduz_", year), 
         get(paste0("kunduz_", year)) %>%
           mutate(Clone_no = paste0("AFG", Clone_no)))
  
  kunduz_list[[paste0("kunduz_", year)]] <- get(paste0("kunduz_", year))
}


# Convert Clone_no column to character type in all dataframes
kunduz_list <- lapply(kunduz_list, function(df) {
  df$Clone_no <- as.character(df$Clone_no)
  return(df)
})

# Get the Clone_no values for the year 2011
common_clones_2011 <- kunduz_list[[1]]$Clone_no

# Filter each dataframe in kunduz_list to keep only the Clone_no values from 2011
kunduz_list <- kunduz_list %>%
  map(~ filter(.x, Clone_no %in% common_clones_2011))

# Finally I have got 92 common clones for different varieties for all years

saveRDS(kunduz_list, file = "kunduz_list.rds")
#readRDS(kunduz_list, file = "kunduz_list.rds")

# I should check the clone number and variety column if they are following a similar order for each year




---------------------------------------------------------------------------------------------------


# After checking clones and varieties names, I found many issues, repetitions, wrong names, spellings etc.
# Now I will clean the data to make it similar for all years

library(chillR)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)

# read phenology data
almonds_phenology <- readRDS(file = "kunduz_list.rds")
almonds_phenology <- almonds_phenology[!names(almonds_phenology) %in% "kunduz_2013"]

# filter varieties by alphabetic order 
almonds_phenology <- lapply(almonds_phenology, function(df) {
  df %>%
    arrange(Variety_name)
})

# filter varieties by alphabetic order 
almonds_phenology <- lapply(almonds_phenology, function(df) {
  df %>%
    arrange(Variety_name)
})

#save_temperature_scenarios(almonds_phenology, path = "D:/Phenology/Kunduz_clean/", prefix = "Phen_")

# Delete some rows for specific varieties and clones (either common or unique)
# detele these from 2011-2016
# List of data frames to process
years_to_process <- c("kunduz_2011", "kunduz_2012", "kunduz_2014", "kunduz_2015", "kunduz_2016")

# Criteria for deletion
criteria <- data.frame(
  Clone_no = c("AFG0780", "AFG1003", "AFG4022", "AFG0772", "AFG2040", "AFG2042", "AFG0776", "AFG1004", "AFG0160", "AFG2043", 
               "AFG0775", "AFG0143", "AFG2044", "AFG0142", "AFG0168", "AFG0771", "AFG1001", "AFG2008", "AFG2012", "AFG2006", 
               "AFG0847", "AFG0164", "AFG1006", "AFG0779", "AFG0848", "AFG2007", "AFG0149", "AFG2041", "AFG0160"),
  Variety_name = c("Abdul Wahidi", "Abdul Wahidi", "Kaghazi", "Khairodini", "Lashmak", "Qahar Bai", "Qaharbai", "Qaharbai", 
                   "Qaharbai", "Qambari", "Qambari", "Qambari", "Satar Bai", "Sattarbai", "Sattarbai", "Sattarbai", "Sattarbai", 
                   "Sattarbai Bakhmali", "Sattarbai Bakhmali", "Sattarbai Guldar", "Sattarbai Sais", "Shakh-i-Buz", "Shakh-i-Buz", 
                   "Shokorbai", "Shokorbai", "Shokorbai", "Zang Kafter", "Zarir Bai", "Qaharbai -")
)

# Function to remove rows based on criteria
remove_rows <- function(df, criteria) {
  df <- df %>%
    anti_join(criteria, by = c("Clone_no", "Variety_name"))
  return(df)
}

# Apply the function to the specified data frames
for (year in years_to_process) {
  almonds_phenology[[year]] <- remove_rows(almonds_phenology[[year]], criteria)
}


# delete from 2017-2023
# List of data frames to process for the new criteria
new_years_to_process <- c("kunduz_2017", "kunduz_2018", "kunduz_2019", "kunduz_2020", "kunduz_2021", "kunduz_2022", "kunduz_2023")

# New criteria for deletion
new_criteria <- data.frame(
  Clone_no = c("AFG4022", "AFG0847", "AFG2041", "AFG0151", "AFG1003", "AFG1006", "AFG0780", "AFG2042", "AFG0776", "AFG1004", 
               "AFG0160", "AFG0772", "AFG0142", "AFG2043", "AFG0143", "AFG2044", "AFG2008", "AFG2012", "AFG0149", "AFG2006", 
               "AFG2040", "AFG0168", "AFG0771", "AFG1001", "AFG0164", "AFG0774", "AFG0779", "AFG0848", "AFG2007"),
  Variety_name = c("Ferragnes", "Kajak Samangan", "Nonpareil", "Pista Badam", "Qaharbai Allah Mir", "Qaharbai Aykhanum", "Qaharbai Hazratan", 
                   "Qahar Bai", "Qaharbai", "Qaharbai", "Qaharbai", "Qambari Kunduz", "Qambari", "Qambari", "Qambari", "Sattarbai", 
                   "Sattarbai Bakhmali", "Sattarbai Bakhmali", "Sattarbai Bakhmali", "Sattarbai Guldar", "Sattarbai Lashmak", 
                   "Sattarbai Mumtaz", "Sattarbai Mumtaz", "Sattarbai Mumtaz", "Sattarbai Sais Kunduz", "Sattarbai Yaqubi", 
                   "Shokorbai", "Shokorbai", "Shokorbai")
)

# Apply the same function to the new list of years
for (year in new_years_to_process) {
  almonds_phenology[[year]] <- remove_rows(almonds_phenology[[year]], new_criteria)
}

# # correct some names
# correct_names <- function(df) {
#   df %>%
#     mutate(Variety_name = case_when(
#       Variety_name == "Sattarbai No.4" ~ "Sattarbai No 4",
#       Variety_name == "Shakh-i-Buz" ~ "Shakh i Buz",
#       Variety_name == "Shakh-i-Buz Safid" ~ "Shakh i Buz Safid",
#       
#       TRUE ~ Variety_name
#     ))
# }
# 
# # Apply the function to each data frame in the list
# almonds_phenology <- lapply(almonds_phenology, correct_names)

# Save and make manuals corrections for names spellings and repetitions
save_temperature_scenarios(almonds_phenology, path = "D:/Phenology/Kunduz_clean/", prefix = "Phen_")

# read back the corrected version 
almonds_phenology <- load_temperature_scenarios("D:/Phenology/Kunduz_clean/kunduz_phen/", prefix = "Phen_")

# filter varieties by alphabetic order 
almonds_phenology <- lapply(almonds_phenology, function(df) {
  df %>%
    arrange(Variety_name)
})

# filter varieties by alphabetic order 
almonds_phenology <- lapply(almonds_phenology, function(df) {
  df %>%
    arrange(Variety_name)
})

# check varieties and clones names for ease in comparsion
result_df <- map_dfc(almonds_phenology, ~ .x %>% select(Variety_name))

# If you want to name the columns uniquely, you can use set_names
result_df <- map_dfc(almonds_phenology, ~ .x %>% select(Variety_name)) %>%
  set_names(paste0("Variety_name_", seq_along(almonds_phenology)))


# I have got 57 different almond varieties


--------------------------------------------------------------------------------------------------



# Formatting temperature data #
# Formatting temperature data #
# Formatting temperature data #

years <- c(2010:2023)
datasets <- paste("PuliAlchin_AT", years, sep = "_")

for (i in 1:length(years)) {
  
  PuliAlchin_AT <- read_excel("temp_data/Chahar_Dara_AT.xlsx",range = "A7:AK38",.name_repair = tolower, sheet = as.character(years[i]))
  
  colnames(PuliAlchin_AT)[1] <- "Day"
  
  jan <-
    data.frame(Month = 1,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[2],
               Tmax = PuliAlchin_AT[3],
               Tmean = PuliAlchin_AT[4])
  feb <-
    data.frame(Month = 2,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[5],
               Tmax = PuliAlchin_AT[6],
               Tmean = PuliAlchin_AT[7])
  mar <-
    data.frame(Month = 3,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[8],
               Tmax = PuliAlchin_AT[9],
               Tmean = PuliAlchin_AT[10])
  apr <-
    data.frame(Month = 4,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[11],
               Tmax = PuliAlchin_AT[12],
               Tmean = PuliAlchin_AT[13])
  mai <-
    data.frame(Month = 5,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[14],
               Tmax = PuliAlchin_AT[15],
               Tmean = PuliAlchin_AT[16])
  jun <-
    data.frame(Month = 6,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[17],
               Tmax = PuliAlchin_AT[18],
               Tmean = PuliAlchin_AT[19])
  jul <-
    data.frame(Month = 7,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[20],
               Tmax = PuliAlchin_AT[21],
               Tmean = PuliAlchin_AT[22])
  aug <-
    data.frame(Month = 8,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[23],
               Tmax = PuliAlchin_AT[24],
               Tmean = PuliAlchin_AT[25])
  sep <-
    data.frame(Month = 9,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[26],
               Tmax = PuliAlchin_AT[27],
               Tmean = PuliAlchin_AT[28])
  okt <-
    data.frame(Month = 10,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[29],
               Tmax = PuliAlchin_AT[30],
               Tmean = PuliAlchin_AT[31])
  nov <-
    data.frame(Month = 11,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[32],
               Tmax = PuliAlchin_AT[33],
               Tmean = PuliAlchin_AT[34])
  dez <-
    data.frame(Month = 12,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[35],
               Tmax = PuliAlchin_AT[36],
               Tmean = PuliAlchin_AT[37])
  
  PuliAlchin <- rbind(jan,feb,mar,apr,mai,jun,jul,aug,sep,okt,nov,dez)
  
  PuliAlchin <- 
    PuliAlchin %>% 
    mutate(Year = years[i]) %>% 
    rename(Tmin = min,
           Tmax = max,
           Tmean = avg) 
  
  
  PuliAlchin$Date <- as.Date(paste0(PuliAlchin$Year,"-",PuliAlchin$Month,"-",PuliAlchin$Day))
  PuliAlchin <- dplyr::filter(PuliAlchin, !is.na(Date))
  
  PuliAlchin$Prec <- NA 
  
  PuliAlchin <- PuliAlchin[c(7,6,1,2,3,4,5,8)]#Re ordering the columns
  
  assign(datasets[i],
         PuliAlchin)
  
}

PuliAlchin <- rbind(PuliAlchin_AT_2010,
                    PuliAlchin_AT_2011,
                    PuliAlchin_AT_2012,
                    PuliAlchin_AT_2013,
                    PuliAlchin_AT_2014,
                    PuliAlchin_AT_2015,
                    PuliAlchin_AT_2016,
                    PuliAlchin_AT_2017,
                    PuliAlchin_AT_2018,
                    PuliAlchin_AT_2019,
                    PuliAlchin_AT_2020,
                    PuliAlchin_AT_2021,
                    PuliAlchin_AT_2022,
                    PuliAlchin_AT_2023)
rm(PuliAlchin_AT,
   PuliAlchin_AT_2010,
   PuliAlchin_AT_2011,
   PuliAlchin_AT_2012,
   PuliAlchin_AT_2013,
   PuliAlchin_AT_2014,
   PuliAlchin_AT_2015,
   PuliAlchin_AT_2016,
   PuliAlchin_AT_2017,
   PuliAlchin_AT_2018,
   PuliAlchin_AT_2019,
   PuliAlchin_AT_2020,
   PuliAlchin_AT_2021,
   PuliAlchin_AT_2022,
   PuliAlchin_AT_2023,
   jan,feb,mar,apr,mai,jun,jul,aug,sep,okt,nov,dez,
   years,datasets,
   i)

#dir.create("D:/Chill R Tutorial/ChillR-basic/AllBasin/PuliAlchin_AT")
write.csv(PuliAlchin,"temp_data/ChaharDara_AT_clean.csv", row.names = FALSE)
#write.csv(PuliAlchin,"D:/Phenology/Raw_data/Kunduz/PuliAlchin_AT_Obs.csv", row.names = FALSE)

# PuliAlchin_AT <- read.csv("D:/Phenology/Raw_data/Kunduz/PuliAlchin_AT_Obs.csv")
# ChaharDara_AT <- read.csv("D:/Phenology/Raw_data/Kunduz/ChaharDara_AT_Obs.csv")


