#-------------- Weighted mean approach for predicted data for 10 repetitions- Historic + Future ----------------#


#-------------- Historic ----------------#

# The function have two major arguments 1. predicted, 2. confidence. So lets prepare them
library(dplyr)
library(tidyr)

historical_bloom <- read.csv("data/historical/combined_fitting_historical.csv")

pred_obs_df <- read.csv('data/pred_obs_df_round_3.csv')
# 2. confidence

iqr_df <- pred_obs_df %>% 
  filter(repetition == 1) %>% 
  group_by(cultivar) %>% 
  summarise(iqr = IQR(observed))

#calculate RPIQ as confidence score (we can do confidence by RPIQ or RMSE, so use RPIQ)
confidence_3 <- pred_obs_df %>% 
  filter(split == 'Validation') %>% 
  group_by(cultivar, repetition) %>% 
  summarise(rmse = chillR::RMSEP(predicted = predicted, observed = observed)) %>% 
  ungroup() %>% 
  merge(iqr_df, by = 'cultivar') %>% 
  mutate(rpiq = iqr / rmse,
         id = cultivar) %>% 
  select(id, rpiq, repetition) %>% 
  pivot_wider(names_from = repetition, 
              values_from = rpiq,
              names_prefix = "R")




# 1. predicted (we have the predicted data from 2020 temperature scenario)

chill_results_list <- list()

# Define the scenario years you want to iterate over
scenario_years <- c(1980, 1990, 2000, 2010, 2020)

for (yr in scenario_years) {
  
chill_hist <- historical_bloom %>% 
  
  dplyr::select(-X, -zc, -s1, -Tu, -theta_c, -tau, -pie_c, -Tf, -Tb, -slope, -Tc, -theta_star, -predicted) %>% # remove unwanted columns
  
  # I delete some accessions that has problem of missing data for 2011 mostly
  filter(!cultivar %in% c("Kaghazi Chahar Dara", "Kheraji", "Quruti", "Said Bai", "Sangak", "Sattarbai Chobak")) %>% 
  
  filter(scenario == yr) %>% # I filter for 2020 data which I use to calculate the change with future scenarios
  
  # we group the data by Cultivar and year
  group_by(cultivar, year) %>%
  
  # we spread the data into a wide format using repetition as new columns
  pivot_wider(names_from = repetition, 
              values_from = yc,
              names_prefix = "R") %>%
  ungroup() %>% 
  
  # add a new ID column
  mutate(id = cultivar) 


# 2. confidence
# Since for confidence using RMSE we need observed and predicted data and we do not have observed here. 
# So we use the confidence from calibration period (model fitting) round 3 as such, we had already saved it.
confidence_3 <- read.csv("D:/Phenology/data/confidence_3.csv")

# My question: we used confidence and RMSE from obs and pred from calibration part, how can we use that for future comparison?

# For example, random weights between 0 and 1 for simplicity

set.seed(123)  # For reproducibility

# Now I will apply the function to calculate the weighted mean
weighted_mean_chill_hist <- weighted_mean_with_fail(
  predicted = chill_hist,
  confidence = confidence_3,
  return_se = TRUE, # OR false if we do not need standard error
  n_fail = 5, # How many repetitions need to indicate a failure with the weighted mean also indicating a failure. Otherwise the failures are ignored, because the skew the predictions. 
  max_weight = 0.5, # max_weight allows you to assign a maximum weight a single repetition can gets assigned to. If I remember this 
  # is scaled between 0 and 1, so you could for example say max_weight = 0.5 and it means that no matter how confident,
  # a single prediction can get no more than 50% influence on the weighted mean. You can use this if you have inflated RPIQ values, 
  # for instance. If set to NULL then this will ignored and weighted mean is simply calculated proportional to the 
  # confidence score. From 0.5 we mean that failures in 50% repetition are allowed. 
  .progress = TRUE # This is for progress bar
)

# Store the result in the list, labeled by the scenario year
chill_results_list[[as.character(year)]] <- weighted_mean_chill_hist

}

# Combine all results into one dataframe
final_chill_results <- bind_rows(chill_results_list, .id = "scenario_year")




# Save weighted mean for ease
write.csv(weighted_mean_pred_hist, "data/historical/weighted_mean_pred_hist_1980.csv", row.names = FALSE) # 1980
# write.csv(weighted_mean_pred_hist, "data/historical/weighted_mean_pred_hist_1990.csv", row.names = FALSE) # 1990
# write.csv(weighted_mean_pred_hist, "data/historical/weighted_mean_pred_hist_2000.csv", row.names = FALSE) # 2000
# write.csv(weighted_mean_pred_hist, "data/historical/weighted_mean_pred_hist_2010.csv", row.names = FALSE) # 2010
write.csv(weighted_mean_pred_hist, "data/historical/weighted_mean_pred_hist_2020.csv", row.names = FALSE) # 2020
