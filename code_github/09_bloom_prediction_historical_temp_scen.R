
#------------------ Historic: Bloom prediction for historical baseline scenario---------------------------#
#------------------ Historic: Bloom prediction for historical baseline scenario---------------------------#
#------------------ Historic: Bloom prediction for historical baseline scenario---------------------------#


# Necessary libraries
library(tidyverse)
library(chillR)


Latitude <- 36.70

#read the save parameters from 3rd round to get starting points for chill, heat, slope and shared parameters values from there
par_df <- read.csv("data/parameter_combined_fitting_round_3.csv")
par_df <- reformat_combined_fit_parameters(par_df)

#number of cultivars I want to fit jointly
ncult <- 57

#fixed parameters
Tc <- 36
theta_star <- 279

# Read historical scenario
historical_scen <- load_temperature_scenarios("temp_data/historic_scen", "Chahardara_hist_scenarios")
    
# Loop through each GCM in Temps to calculate hourly temperature

historical_bloom <- data.frame()


      for (hists in names(historical_scen)) {
        
      historical_scen[[hists]]$JDay <- as.numeric(format(as.Date(historical_scen[[hists]]$DATE), "%j")) 
        
      hourtemps <- historical_scen[[hists]] %>% 
        stack_hourly_temps(latitude = 36.70)
      
      hourtemps <- hourtemps$hourtemps # Extract hourtemps
      
      hourtemps <- hourtemps %>%
        rename(Date = DATE)
      
      hourtemps$Date <- as.Date(hourtemps$Date, format = "%Y-%m-%d %H:%M:%S")
      
      # keep the relevant columns (keeping hourly temp)
      hourtemps <- hourtemps[,c("Date", "Year", "Month", "Day", "JDay", "Hour", "Temp")]
      
      # Creating a list for seasons based on historical years (based on the temperature data this time, previously bloom dates)
      historic_years <- unique(hourtemps$Year)
      Historic_SeasonList <- hourtemps %>% #temperature data is already hourly with stack_hourly_temperatures(), so I split into seasons (each historic year)
        genSeasonList(years = historic_years) %>% 
        set_names(historic_years)
      
      # we the data from calibration par_df to predict data for historical scenarios
            pred_obs_df <- purrr::map(1:nrow(par_df), function(i){
        pred <- par_df[i, par_names] %>% #take model parameters
          unlist() %>%  #make it a vector
          unname() %>%  #get rid of annoying names
          convert_parameters() %>% #convert to 'old' parameter format
          return_predicted_days(SeasonList = Historic_SeasonList) #predict bloomdates
      }, .progress = TRUE) 
      
      pred_obs_df <- pred_obs_df %>% 
        bind_rows() %>% 
        cbind(par_df) %>% 
        mutate(scenario = hists) %>% 
        pivot_longer(col = as.character(2001:2100), names_to = 'year', values_to = 'predicted')
      
      #append to historical bloom
      historical_bloom <- rbind(historical_bloom, pred_obs_df)
      
    }

#save historical bloom dates to one file
write.csv(historical_bloom, "data/historical/combined_fitting_historical.csv") # this is predicted historical bloom

historical_bloom <- read.csv("data/historical/combined_fitting_historical.csv")

historical_bloom <- historical_bloom %>% 
  mutate(id = paste(cultivar, scenario, sep = '-'))

  
