
#------------------ Future: Bloom prediction for future ssp scenarios---------------------------#
#------------------ Future: Bloom prediction for future ssp scenarios---------------------------#
#------------------ Future: Bloom prediction for future ssp scenarios---------------------------#


# Necessary libraries
library(tidyverse)
library(chillR)

#read optimization function
source('util/custom_ess.R')

#read future temperature for different scenarios
SSPs<-c("ssp126","ssp245", "ssp585")
Times <- c(2050, 2085)
GCMs <- c("ACCESS-CM2","AWI-CM-1-1-MR","CMCC-ESM2","CNRM-CM6-1-HR","CNRM-ESM2-1", "EC-Earth3-Veg-LR", "FGOALS-g3",
                "FIO-ESM-2-0", "GFDL-ESM4", "INM-CM4-8", "INM-CM5-0", "MIROC6", "MPI-ESM1-2-LR", "MRI-ESM2-0", "NESM3")

# Scenario settings for future temperature data
# SSPs <- c("ssp126")  # Running for a single SSP to test
# Times <- c(2050)  # Running for a single timepoint to test
# GCMs <- c("ACCESS-CM2")  # Running for a single GCM to test

Latitude <- 36.70

#read the save parameters from 3rd round to get starting points for chill, heat, slope and shared parameters values from there
par_df <- read.csv("data/parameter_combined_fitting_round_3.csv")
par_df <- reformat_combined_fit_parameters(par_df)

#number of cultivars I want to fit jointly
ncult <- 57

#fixed parameters
Tc <- 36
theta_star <- 279

#name of the file to save the fitted model parameters
#fname <- paste0('data/parameter_combined_fitting_future_', SSP, '_', Time, '_', GCM, '.csv')

future_bloom <- data.frame()

for (SSP in SSPs) {
  cat('SSP: ', SSP, '\n') # For debug the process
  for (Time in Times) {
    cat('Time: ', Time, '\n') # debug
    # Load future temperature data
    Temps <- load_temperature_scenarios("temp_data/future_climate", 
                                        paste0("Chahardara_futuretemps_", SSP, "_", Time)) 
    
    # Assigning names to the data frames
    names(Temps) <- GCMs

    # Loop through each GCM in Temps to calculate hourly temperature
    for (GCM in names(Temps)) {
      cat('GCM: ', which(GCM == GCMs), ' of ', length(GCMs), '\n') # debug
      print(paste("Processing GCM:", GCM))  # Debug statement
      
      Temps[[GCM]]$JDay <- as.numeric(format(as.Date(Temps[[GCM]]$DATE), "%j"))

      hourtemps <- Temps[[GCM]] %>% 
        stack_hourly_temps(latitude = 36.70)
      
      hourtemps <- hourtemps$hourtemps # Extract hourtemps
      
      hourtemps <- hourtemps %>%
        rename(Date = DATE)
      
      hourtemps$Date <- as.Date(hourtemps$Date, format = "%Y-%m-%d %H:%M:%S")
      
      # keep the relevant columns (keeping hourly temp)
      hourtemps <- hourtemps[,c("Date", "Year", "Month", "Day", "JDay", "Hour", "Temp")]
      
      # Creating a list for seasons based on future years (based on the temperature data this time, previously bloom dates)
      future_years <- unique(hourtemps$Year)
      future_SeasonList <- hourtemps %>% #temperature data is already hourly with stack_hourly_temperatures(), so I split into seasons (each future year)
        genSeasonList(years = future_years) %>% 
        set_names(future_years)
      
      # we the data from calibration par_df to predict data for future
      
      pred_obs_df <- purrr::map(1:nrow(par_df), function(i){
        pred <- par_df[i, par_names] %>% #take model parameters
          unlist() %>%  #make it a vector
          unname() %>%  #get rid of annoying names
          convert_parameters() %>% #convert to 'old' parameter format
          return_predicted_days(SeasonList = future_SeasonList) #predict bloomdates
        }, .progress = TRUE) 
      
      pred_obs_df <- pred_obs_df %>% 
        bind_rows() %>% 
        cbind(par_df) %>% 
        mutate(ssp = SSP,
               time = Time,
               gcm = GCM) %>% 
        pivot_longer(col = as.character(2001:2100), names_to = 'year', values_to = 'predicted')
      
      #append to future bloom
      future_bloom <- rbind(future_bloom, pred_obs_df)
        
      
        
    }

  }
  cat('SSP:', SSP, ' finished.')
  cat('--------------')

}

#save future bloom dates to one file
write.csv(future_bloom, "data/future/combined_fitting_future.csv", row.names = FALSE)

future_bloom <- read.csv("data/future/combined_fitting_future.csv") # this is predicted future bloom

future_bloom <- future_bloom %>% 
  mutate(id = paste(cultivar, ssp, time, gcm, sep = '-'))




#read evaluation function
source('util/evaluation_function.R')











