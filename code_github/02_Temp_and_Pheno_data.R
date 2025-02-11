########################################## Chapter 3 ##################################
################################ Phenology and Temperature data #######################


# Packages
library(chillR)
library(dplyr)
library(tidyr)
library(lubridate)

#--------------------------------------------------------------------------------#
# Temperature data
ChaharDara_AT <- read.csv("temp_data/ChaharDara_AT_clean.csv")
ChaharDara_AT <- ChaharDara_AT %>%
  make_JDay() %>%
  mutate(Date = format(ymd(Date), '%d-%m-%Y')) %>%
  rename(DATE = Date) %>%
  select(-Prec) #%>%
  #filter(Year != 2013)

#--------------------------------------------------------------------------------#
# Phenology data

# I have got 57 different almond varieties I will read them
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

#1 Arrange with one method
# filter varieties by alphabetic order 
almonds_phenology <- lapply(almonds_phenology, function(df) {
  df %>%
    arrange(Variety_name)
})

# combine into one data frame
almonds_phenology <- bind_rows(almonds_phenology)

# rename
almonds_phenology <- almonds_phenology %>%
  rename(variety = Variety_name,
         budbreak = Bud_burst,
         firstbloom = Flower_10,
         bloom_50 = Flower_50,
         fullbloom = Flower_90)

# change data format
almonds_phenology <- almonds_phenology %>%
  mutate(budbreak = dmy(budbreak),
         firstbloom = dmy(firstbloom),
         bloom_50 = dmy(bloom_50),
         fullbloom = dmy(fullbloom),
         Petal_fall = dmy(Petal_fall))

# arrange by variety
almonds_phenology <- almonds_phenology %>% 
  arrange(variety)

# add year column
almonds_phenology <- almonds_phenology %>% 
  mutate(year = year(ymd(firstbloom)))

write.csv(almonds_phenology, "D:/Phenology/almonds_phenology.csv", row.names = FALSE)

#2 Arrange by other method
# I will change the data arrangement

pivot_longer_for_variety <- function(df) {
  # Convert columns to date type
  df$Bud_burst <- as.Date(df$Bud_burst, format = "%d/%m/%Y")
  df$Flower_10 <- as.Date(df$Flower_10, format = "%d/%m/%Y")
  df$Flower_50 <- as.Date(df$Flower_50, format = "%d/%m/%Y")
  df$Flower_90 <- as.Date(df$Flower_90, format = "%d/%m/%Y")
  df$Petal_fall <- as.Date(df$Petal_fall, format = "%d/%m/%Y")
  
  # Pivot longer
  pivot_longer(df, 
               cols = c(Bud_burst:Petal_fall),
               names_to = "Stage",
               values_to = "YEARMODA")
}
almonds_phenology_long <- lapply(almonds_phenology, pivot_longer_for_variety)

add_dates <- function(df) {
  df <- df %>%
    mutate(Year = as.numeric(substr(YEARMODA, 1, 4)),
           Month = as.numeric(substr(YEARMODA, 6, 7)),
           Day = as.numeric(substr(YEARMODA, 9, 10))) %>%
    make_JDay() %>%
    #filter(Stage == "Flower_10")
    return(df)
}
almonds_phenology_long <- lapply(almonds_phenology_long, add_dates)

# Combine all the years in a single long dataframe
combine_phen <- bind_rows(almonds_phenology_long)

# remove variety "Sattarbai Doum"
combine_phen <- combine_phen[combine_phen$Variety_name != "Sattarbai Doum", ]

write.csv(combine_phen, "D:/Phenology/Kunduz_clean/kunduz_phen/almonds_phenology.csv")
#--------------------------------------------------------------------------------#

# Identify which stage to work with
# I will filter to chose Flower_10%
Pheno_10 <- combine_phen %>% 
  filter(Stage == "Flower_10")

# I will only keep Year and Pheno_10
Pheno <- Pheno_10 %>% 
  mutate(Year = 'Year',
         )

# convert into hourly temperature
hourtemps <- ChaharDara_AT %>%
  # mutate(DATE = YEARMODA) %>%
  stack_hourly_temps(latitude = 36.7)

hourtemps <- hourtemps$hourtemps

# rename
hourtemps <- hourtemps %>%
  rename(Date = DATE)

hourtemps$Date <- dmy(hourtemps$Date)

write.csv(hourtemps, "temp_data/hourtemps_kunduz.csv", row.names = FALSE)

# # Define a vector of calibration and validation seasons. V1 includes the marginal seasons
# calibration_seasons <- sort(sample(Pheno_10$Year, 9, replace = FALSE))
# calibration_seasons_v2 <- calibration_seasons
# 
# # Common validation seasons
# validation_seasons <- sort(Pheno_10[!(Pheno_10$Year %in% calibration_seasons), "Year"])
# 
# # Define the list of seasons (weather data)
# season_list_v2 <- genSeasonList(data, mrange = c(9, 7), years = calibration_seasons_v2)

