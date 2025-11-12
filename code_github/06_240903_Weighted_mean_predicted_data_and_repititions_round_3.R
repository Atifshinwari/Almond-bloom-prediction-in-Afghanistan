
# Application of weighted mean approach: Many values for predicted and observed bloom dates for each almond cultivar for different years
#                                        different repetitions, so we apply weighted mean approach to consolidate our data.


# Loading the data from the best try:
#read the save parameters
fname <- 'data/parameter_combined_fitting_round_3.csv'

par_df3 <- read.csv(fname)

source('util/convenience_functions.R')

par_df3 <- reformat_combined_fit_parameters(par_df3)


#tidy-up the calibration data.frame, only keep pheno-stage of interest
cal_clean <- calibration_df %>% 
  mutate(observed = lubridate::yday(firstbloom), # get the Jday from bloom date
         split = 'Calibration',
         cultivar = variety,
         pheno_stage = 'firstbloom') %>% 
  select(cultivar, repetition, year, observed, pheno_stage, split)

#same for validation df
val_clean <- validation_df %>% 
  mutate(observed = lubridate::yday(firstbloom), # get the Jday from bloom date
         split = 'Validation',
         cultivar = variety,
         pheno_stage = 'firstbloom') %>% 
  select(cultivar, repetition, year, observed, pheno_stage, split)
pred_obs_df <- rbind(cal_clean, val_clean)
rm(cal_clean, val_clean)

#predict bloom dates: It add gets and add the predicted bloom dates to our df pred_obs_df
pred_obs_df <- purrr::map(1:nrow(par_df3), function(i){
  pred <- par_df3[i, par_names] %>% #take model parameters
    unlist() %>%  #make it a vector
    unname() %>%  #get rid of annoying names
    convert_parameters() %>% #convert to 'old' parameter format
    return_predicted_days(SeasonList = SeasonList) #predict bloomdates
  
  data.frame(cultivar = par_df3$cultivar[i],
             repetition = par_df3$repetition[i],
             year = names(SeasonList),
             predicted = pred) %>% 
    merge(pred_obs_df, by = c('cultivar', 'year', 'repetition')) %>% 
    return()
}, .progress = TRUE) %>% 
  bind_rows()

# Here I will delete some accessions that has problem of missing data for 2011 mostly
pred_obs_df <- pred_obs_df %>%
  filter(!cultivar %in% c("Kaghazi Chahar Dara", "Kheraji", "Quruti", "Said Bai", "Sangak", "Sattarbai Chobak"))

# I save pred_obs_df for convenience in later use
write.csv(pred_obs_df, 'data/pred_obs_df_round_3.csv', row.names = FALSE)

# Now I have to run the function for weighted mean approach: developed by Lars

# The function have two major arguments 1. predicted, 2. confidence. So lets prepare them
library(dplyr)
library(tidyr)

# 1. predicted
predicted_3 <- pred_obs_df

predicted_3 <- predicted_3 %>% 
  select(-split) %>% # split column was leading to create empty values in new columns (due to sequence of Calibration and Validation)

  # we group the data by Cultivar and year
  group_by(cultivar, year) %>%
  
  
  # we spread the data into a wide format using repetition as new columns
  pivot_wider(names_from = repetition, 
    values_from = predicted,
    names_prefix = "R") %>%
  ungroup() %>% 
  
  # add a new ID column
  mutate(id = cultivar)


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

# Save confidence for use in future scenarios predictions b/c it will be used as such without changing 
write.csv(confidence_3, "D:/Phenology/data/confidence_3.csv", row.names = FALSE)

# Since we are pooling both calibration and validation in weighted mean approach therefore, we are not getting them separately
# for each cultivar and repetition. 
  


# For example, random weights between 0 and 1 for simplicity

set.seed(123)  # For reproducibility

# runif(10, 0, 1)
# 
# confidence_3 <- data.frame(
#   id = predicted_3$id,  # Matching ID column with predicted_3
#   predicted_R1 = runif(nrow(predicted_3), 0, 1), # Here we get the real weight from 'predicted_3' for each repetition
#   predicted_R2 = runif(nrow(predicted_3), 0, 1),
#   predicted_R3 = runif(nrow(predicted_3), 0, 1),
#   predicted_R4 = runif(nrow(predicted_3), 0, 1),
#   predicted_R5 = runif(nrow(predicted_3), 0, 1),
#   predicted_R6 = runif(nrow(predicted_3), 0, 1),
#   predicted_R7 = runif(nrow(predicted_3), 0, 1),
#   predicted_R8 = runif(nrow(predicted_3), 0, 1),
#   predicted_R9 = runif(nrow(predicted_3), 0, 1),
#   predicted_R10 = runif(nrow(predicted_3), 0, 1))

# Now I will apply the function to calculate the weighted mean
source('util/ensemble_prediction_weighted_mean.R')

weighted_mean_pred <- weighted_mean_with_fail(
  predicted = predicted_3,
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

library(ggplot2)

# Assuming weighted_mean_pred has columns: ID, weighted_mean, standard_error, etc.

# Plot weighted means vs observed values
# Create a scatter plot with error bars
weighted_mean_plot_3 <- ggplot(weighted_mean_pred, aes(y = weighted_pred, x = observed, col = cultivar)) +
  geom_point(size = 3) + # increase size of points
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  geom_errorbar(aes(ymin = weighted_pred - sd, ymax = weighted_pred + sd), width = 0.4) +
  labs(#title = "Weighted Predictions vs Observed Values",
       y = "Predicted Bloom Dates",
       x = "Observed Bloom Dates", 
       color = 'Cultivar') +
  
  scale_x_continuous(breaks = seq(30, 90, by = 15)) +
  scale_y_continuous(breaks = seq(30, 90, by = 15)) +
  
  theme_bw() +
  
  theme(legend.title = element_text(size = 16),
        legend.text = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 16, margin = margin(t = 10)), # with margin add space between axis text and axis title
        axis.title.y = element_text(size = 16, margin = margin(r = 10))) + 
  
  guides(color = guide_legend(ncol = 2))  # Make the legend into 2 colums


ggsave(plot = weighted_mean_plot_3, filename = "plot/combine_fit_stat/combine_plot_R3_weighted.png", width = 14, height = 10, dpi = 600)


# Calculate statistics RMSE and RPIQ

rmse_df_3 <- weighted_mean_pred %>% 
  group_by(cultivar) %>% 
  summarise(rmse = chillR::RMSEP(predicted = weighted_pred, observed = observed, na.rm= TRUE)) # I found only 1 NA value for Abdulwahidi for 2023

#calculating rpiq for small sample sizes is a bit unfair, so we will calculate
#a variant of rpiq, where the interquartile range is calculate before 
#splitting into calibration / validation
iqr_df_3 <- weighted_mean_pred %>% 
  group_by(cultivar) %>% 
  summarise(iqr = IQR(observed))

rmse_ipiq_df_3 <- weighted_mean_pred %>% 
  group_by(cultivar) %>% 
  summarise(rmse = chillR::RMSEP(predicted = weighted_pred, observed = observed, na.rm= TRUE)) %>% # the same NA
  ungroup() %>% 
  merge(iqr_df_3, by = c('cultivar')) %>% 
  mutate(rpiq_adj = iqr / rmse)


# Plot the statistic RMSE and RPIQ: I could not plot RMSE and RPIQ with box plot due to only 2 values per cultivar now

# # Reshape the data
long_df_3 <- rmse_ipiq_df_3 %>%
  pivot_longer(cols = c(rmse, rpiq_adj), names_to = "metric", values_to = "value")

# I plot a combine plot for all rmse and rpiq for all varieties
# These RMSE and RPIQ are computed for the weighted mean predicted and observed bloom dates

custom_labels <- c(
  "rmse" = "Root Mean Squared Error of Prediction (RMSEP)",
  "rpiq_adj" = "Ratio of Performance to Inter-Quartile distance (RPIQ)")

RMSE_box_R3_combine <- ggplot(long_df_3, aes(x = metric, y = value, fill = metric)) +
  geom_boxplot() +
  theme_gray() +
  labs(#title = "Distribution of RMSE and RPIQ Across Cultivars",
       x = "Model Performance Metrics",
       y = "Metrics Value") +
  
  scale_y_continuous(breaks = seq(0, 10, by = 2)) +
  
  scale_x_discrete(labels = custom_labels) +
  theme(axis.title.x = element_text(size = 18, margin = margin(t = 10)),
        axis.title.y = element_text(size = 18, margin = margin(r = 10)),
        axis.text = element_text(size = 16),
        legend.position = "none") +
  scale_fill_manual(values = c('#B22222', 'skyblue'),
                    breaks = c(long_df_3$metric))

ggsave(plot = RMSE_box_R3_combine, filename = "plot/box plot/RMSE_box_R3_weight_mean.png", width = 14, height = 10, dpi = 600)

# I only save this to use it for editing it for early mid and late flowering
write.csv(long_df_3, "D:/Phenology/biological_meaning.csv")


