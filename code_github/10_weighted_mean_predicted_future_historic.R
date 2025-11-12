
#-------------- Weighted mean approach for predicted data for 10 repetitions- Historic + Future ----------------#


#-------------- Historic ----------------#

# The function have two major arguments 1. predicted, 2. confidence. So lets prepare them
library(dplyr)
library(tidyr)

historical_bloom <- read.csv("data/historical/combined_fitting_historical.csv")

# 1. predicted (we have the predicted data from 2020 temperature scenario)
predicted_hist <- historical_bloom %>% 

  dplyr::select(-X, -yc, -zc, -s1, -Tu, -theta_c, -tau, -pie_c, -Tf, -Tb, -slope, -Tc, -theta_star) %>% # remove unwanted columns
  
  # I delete some accessions that has problem of missing data for 2011 mostly
  filter(!cultivar %in% c("Kaghazi Chahar Dara", "Kheraji", "Quruti", "Said Bai", "Sangak", "Sattarbai Chobak")) %>% 
  
  filter(scenario == 1980) %>% # I filter for 2020 data which I use to calculate the change with future scenarios

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
# Since for confidence using RMSE we need observed and predicted data and we do not have observed here. 
# So we use the confidence from calibration period (model fitting) round 3 as such, we had already saved it.
confidence_3 <- read.csv("D:/Phenology/data/confidence_3.csv")

# My question: we used confidence and RMSE from obs and pred from calibration part, how can we use that for future comparison?

# For example, random weights between 0 and 1 for simplicity

set.seed(123)  # For reproducibility

# Now I will apply the function to calculate the weighted mean
weighted_mean_pred_hist <- weighted_mean_with_fail(
  predicted = predicted_hist,
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

# Save weighted mean for ease
# write.csv(weighted_mean_pred_hist, "data/historical/weighted_mean_pred_hist_1980.csv", row.names = FALSE) # 1980
# # write.csv(weighted_mean_pred_hist, "data/historical/weighted_mean_pred_hist_1990.csv", row.names = FALSE) # 1990
# # write.csv(weighted_mean_pred_hist, "data/historical/weighted_mean_pred_hist_2000.csv", row.names = FALSE) # 2000
# # write.csv(weighted_mean_pred_hist, "data/historical/weighted_mean_pred_hist_2010.csv", row.names = FALSE) # 2010
# write.csv(weighted_mean_pred_hist, "data/historical/weighted_mean_pred_hist_2020.csv", row.names = FALSE) # 2020

# Read data and prepare it
weighted_mean_pred_hist_1980 <- read.csv("data/historical/weighted_mean_pred_hist_1980.csv")
weighted_mean_pred_hist_1980 <- weighted_mean_pred_hist_1980 %>% # remove unwanted columns
  select(-R1, -R2, -R3, -R4, -R5, -R6, -R7, -R8, -R9, -R10, -id) %>% 
  group_by(cultivar) %>%
  mutate(median_weighted_pred = median(weighted_pred),
         median_date = format(as.Date(round(median_weighted_pred), origin = "2020-01-01"), "%b %d"))  

weighted_mean_pred_hist_2020 <- read.csv("data/historical/weighted_mean_pred_hist_2020.csv")
weighted_mean_pred_hist_2020 <- weighted_mean_pred_hist_2020 %>% # remove unwanted columns
  select(-R1, -R2, -R3, -R4, -R5, -R6, -R7, -R8, -R9, -R10, -id) %>% 
  group_by(cultivar) %>%
  mutate(median_weighted_pred = median(weighted_pred),
         median_date = format(as.Date(round(median_weighted_pred), origin = "2020-01-01"), "%b %d"))

weighted_mean_pred_hist <- bind_rows(weighted_mean_pred_hist_1980, weighted_mean_pred_hist_2020)



# # Plot weighted means
# weighted_mean_pred_hist
# 
# # I calculate date and median of it for annotating on ggplot boxes
# # Create a new column date column with the converted date (assuming the year is constant, e.g., 2020)
# weighted_mean_pred_hist$date <- as.Date(round(weighted_mean_pred_hist$weighted_pred), origin = "2020-01-01") # Calculating date from JDay and round it
# 
# # median date calculation
# weighted_mean_pred_hist <- weighted_mean_pred_hist %>%
#   group_by(cultivar) %>%
#   mutate(median_weighted_pred = median(weighted_pred),
#          median_date = format(as.Date(round(median_weighted_pred), origin = "2020-01-01"), "%b %d"))

library(ggplot2)

date_break <- c(32, 60, 91)
date_label <- c('01-Feb', '01-Mar', '01-Apr')

# Plotting
weighted_mean_plot_hist <- ggplot(weighted_mean_pred_hist, aes(y = reorder(cultivar,weighted_pred), x = weighted_pred, fill = factor(scenario))) +
  geom_boxplot(size = 0.65, width = 1.2, position = position_dodge(width = 0), alpha = 0.7) +
  #facet_grid(~ scenario)
  labs(#title = "Weighted Predictions vs Observed Values",
    y = "Almond cultivar",
    #x = "Predicted Bloom Dates for 2020 Historical Scenario (Weighted Mean)") +
  #x = "Predicted Bloom Dates for 1980 Historical Scenario (Weighted Mean)") +
  x = "Predicted bloom date",
  fill = 'Historical Climate\nScenarios') + # title for legend
  
  scale_x_continuous(breaks = date_break, labels = date_label) +
  
  scale_fill_manual(values = c("darkblue", "darkred"), 
                    breaks = unique(weighted_mean_pred_hist$scenario)) +
  theme_bw() +
  

  # Add text annotation for median date per cultivar
  geom_text(aes(label = median_date, x = median_weighted_pred), vjust = 0.5, hjust = 0.57, color = "gray100", size = 5.2) +
  #geom_text(aes(label = median_date, x = median_weighted_pred, color = factor(scenario)), vjust = 0.5, hjust = 0.6) +
  
  
  theme(legend.title = element_text(size = 25),
        legend.text = element_text(size = 23),
        legend.position = 'right',
        legend.key.size = unit(2, 'cm'),
        axis.text = element_text(size = 20), #10
        axis.title.x = element_text(size = 25, margin = margin(t = 10)), #14
        axis.title.y = element_text(size = 25, margin = margin(t = 10)), #14
        axis.ticks.length = unit(0.15, 'cm'),
        axis.ticks = element_line(linewidth = 1.4),
        axis.ticks.x = element_line(colour = 'black'),
        axis.ticks.y = element_line(colour = 'black'))

#ggsave(plot = weighted_mean_plot_hist, filename = "plot/combine_fit_stat/combine_plot_Hist1980_weighted.png", width = 16, height = 10, dpi = 600)
#ggsave(plot = weighted_mean_plot_hist, filename = "plot/combine_fit_stat/combine_plot_Hist2020_weighted.png", width = 16, height = 10, dpi = 600)
ggsave(plot = weighted_mean_plot_hist, filename = "plot/combine_fit_stat/combine_plot_Hist_weighted.png", width = 17, height = 14, dpi = 600)



#-------------- Future ----------------#




future_bloom <- read.csv("data/future/combined_fitting_future.csv")

# 1. predicted (we have the predicted data from 2020 temperature scenario)
predicted_future <- future_bloom %>% 
  
  select(-yc, -zc, -s1, -Tu, -theta_c, -tau, -pie_c, -Tf, -Tb, -slope, -Tc, -theta_star) %>% # remove unwanted columns
  
  # I delete some accessions that has problem of missing data for 2011 mostly
  filter(!cultivar %in% c("Kaghazi Chahar Dara", "Kheraji", "Quruti", "Said Bai", "Sangak", "Sattarbai Chobak")) %>% 
  
  
  # we group the data by Cultivar and year
  group_by(cultivar, year) %>%
  
  # we spread the data into a wide format using repetition as new columns
  pivot_wider(names_from = repetition, 
              values_from = predicted,
              names_prefix = "R") %>%
  ungroup() %>% 
  
  # add a new ID column
  #mutate(id = paste(cultivar, ssp, time, gcm, sep = '-')) 
   mutate(id = cultivar) 



# 2. confidence
# Since for confidence using RMSE we need observed and predicted data and we do not have observed here. 
# So we use the confidence from calibration period (model fitting) round 3 as such, we had already saved it.
confidence_3 <- read.csv("D:/Phenology/data/confidence_3.csv")

# My question: we used confidence and RMSE from obs and pred from calibration part, how can we use that for future comparison?

# For example, random weights between 0 and 1 for simplicity

set.seed(123)  # For reproducibility

# Now I will apply the function to calculate the weighted mean
weighted_mean_pred_future <- weighted_mean_with_fail(
  predicted = predicted_future,
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

#write.csv(weighted_mean_pred_future, "data/future/weighted_mean_pred_future.csv", row.names = FALSE)
weighted_mean_pred_future <- read.csv("data/future/weighted_mean_pred_future.csv")




#plotting future bloom prediction for different ssps, timepoints and gcms


# median date calculation
weighted_mean_pred_future <- weighted_mean_pred_future %>%
  mutate(weighted_pred_rounded = round(weighted_pred),
         # I calculate date based on the year column to adjust for leap years too
         date = format(as.Date(weighted_pred_rounded - 1, origin = paste0(year, "-01-01")), "%b %d")) 
         
         
# Plotting
# Load necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)

# Assuming the data is in a tibble named 'weighted_mean_pred_hist'
# Reshape the data for better plotting
weighted_mean_pred_future <- weighted_mean_pred_future %>%
  select(-R1, -R2, -R3, -R4, -R5, -R6, -R7, -R8, -R9, -R10, -id) 

#ssp126_data <- reshaped_data %>% filter(ssp == 'ssp126', time %in% c(2050, 2085))



# -------Future Change prediction-----------------#
# -------Future Change prediction-----------------#
# -------Future Change prediction-----------------#

# I get median of all 100 values of pred for historic and future scenarios
weighted_mean_pred_hist <- read.csv("data/historical/weighted_mean_pred_hist_2020.csv") # 2020

hist_2020 <- weighted_mean_pred_hist %>% 
  select(-R1, -R2, -R3, -R4, -R5, -R6, -R7, -R8, -R9, -R10, -id, -year) %>% 
  group_by(cultivar) %>%
  summarise(         year_h = 2020,
                     scenario_h = 2020,
                     median_pred_100_year_h = median(weighted_pred, na.rm = TRUE), # median_pred_100_year_f is the median of 100 prediction values for 100 simulated year
         median_sd_h = median(sd, na.rm = TRUE),
         na_count_h = sum(is.na(weighted_pred))  # Track NA count
  ) %>%
  mutate(date_h = format(as.Date(round(median_pred_100_year_h), origin = "2020-01-01"), "%b %d"))


future_2020_2085 <- weighted_mean_pred_future %>% 
  group_by(cultivar, ssp, time, gcm) %>%
  summarise(         median_pred_100_year_f = median(weighted_pred, na.rm = TRUE),
                     median_sd = median(sd, na.rm = TRUE),
                     na_count_f = sum(is.na(weighted_pred))  # Track NA count
  ) %>% 
  mutate(date = format(as.Date(round(median_pred_100_year_f), origin = "2020-01-01"), "%b %d"),
         year = time) # create an year column as well

# Merge hist_2020 with future_2020_2085 by "cultivar"
Future_change_predictions <- future_2020_2085 %>%
  left_join(hist_2020 %>% select(cultivar, year_h, scenario_h, median_pred_100_year_h, median_sd_h), by = "cultivar") %>% # join both
  mutate(pred_diff = median_pred_100_year_h - median_pred_100_year_f,
         na_count = na_count_f)


# early is represented by +ve while delay by -ve

# Also compute the sd of historic and future (wether difference or combine them, I have to research for standard approach)
# Calculate the combined SD for the difference
Future_change_predictions$sd_diff <- sqrt(Future_change_predictions$median_sd_h^2 + Future_change_predictions$median_sd^2)


# plotting (difference)
Future_change_predictions$cultivar<- factor(Future_change_predictions$cultivar, levels=rev(sort(unique(Future_change_predictions$cultivar)))) # alphabetic reorder

# The highest and lowest value of change
 # max_pred <- max(Future_change_predictions$pred_diff, na.rm = TRUE) # 16.61
 # min_pred <- min(Future_change_predictions$pred_diff, na.rm = TRUE) # -13.56

# # highest NA
# max_na_count <- max(Future_change_predictions$na_count) # 35 is maximum NA


# threshold for acceptable NAs
threshold_na <- 10  # the highest NA is 35, I check in the dataframe

weighted_mean_plot_future <- ggplot(Future_change_predictions, aes(x = gcm, y = cultivar, fill = pred_diff)) +
  geom_tile() +
  
  # for extremes-------------------------------------------------
  # add another color for highlighting extreme difference for specific cultivars and gcms
  geom_tile(data = subset(Future_change_predictions, pred_diff <= -5 ),
            color = "red", linewidth = 0.5) +

  geom_tile(data = subset(Future_change_predictions, pred_diff >= 7),
            color = "black", linewidth = 0.5) +
  # for extremes-------------------------------------------------
  

  # # for sd-------------------------------
  # geom_tile(data = subset(Future_change_predictions, sd_diff > 5),
  #           color = "red", linewidth = 0.5) +
  # # for sd-------------------------------


  # for add X mark for tiles with too many NAs-----------------------
# Add x symbols for NA counts below threshold
  geom_text(data = subset(Future_change_predictions, na_count > threshold_na), aes(label = "x"), color = "red", size = 6) +
  
  # for add X mark for tiles with too many NAs-----------------------


  facet_grid(. ~ time + ssp, labeller = labeller(ssp = toupper)) +  # Use . ~ ssp + time and switch to control strip positions, change to upper case as well
  
  scale_x_discrete(labels = paste('', 1:15)) + # replace x axis text with numbers for reference to the gcm names
  
  labs(
    y = "Cultivar",
    x = 
      '\nGCMs: 1: ACCESS-CM2,    2: AWI-CM-1-1-MR,    3: CMCC-ESM2,   4: CNRM-CM6-1-HR,   5: CNRM-ESM2-1,   6: EC-Earth3-Veg-LR,   7: FGOALS-g3,\n    8: FIO-ESM-2-0,   9: GFDL-ESM4,   10: INM-CM4-8,   11: INM-CM5-0,   12: MIROC6,   13: MPI-ESM1-2-LR,   14: MRI-ESM2-0, 15: NESM3',
    fill = "Bloom shift prediction\nrelative to 2020\nscenario",
    #caption = "x = NA count > Threshold"  # Add legend title for x marks
    caption = expression(bold(x) * " = Failure > 10%")) +  # Add legend title for x marks

  theme_bw() +
  theme(
    
    # Adjust grid lines
    #panel.grid.major = element_line(color = "grey90", size = 0.5),  # To Lighten major grid lines
    #panel.grid.minor = element_blank(),  # Remove minor grid lines
    
    # Add borders around facet panels
    #panel.border = element_rect(color = "black", fill = NA, size = 0.8),  # Border around each panel
    
    strip.text = element_text(size = 18),
    axis.text.x = element_text(angle = 60, hjust = 1, size = 13.5),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 18, hjust = 1.2),
    axis.title.y = element_text(size = 22),
    legend.position = 'top',
    legend.text = element_text(size = 16.5),
    legend.title = element_text(size = 18),
    legend.key.height = unit(1, 'cm'),
    legend.key.width = unit(1, 'cm'),
    plot.title = element_text(hjust = 0.5, size = 16), 
    plot.caption = element_text(hjust = 1, vjust = 220, size = 17.5, color = "black", face = "italic") # old plot.caption = element_text(hjust = 1.2, vjust = 60, size = 17.5, color = "black", face = "italic")
    
  ) +

# scale_fill_gradient(low = "lightyellow", high = "darkred",  # Adjust color gradient for fill
#                     breaks = c(-8, -4, 0, 4, 8, 12, 16), # adjust the values on the legend
#                     labels = c(-8, -4, 0, 4, 8, 12, 16))

scale_fill_gradient2(
  low = "darkblue",    # Color for values below 0
  mid = "white",         # Color for values around 0
  high = "darkred",      # Color for values above 0
  midpoint = 0,          # Define the midpoint at 0
  breaks = c(-12, -8, -4, 0, 4, 8, 12, 16),  # Adjust the values on the legend
  labels = c(-12, -8, -4, 0, 4, 8, 12, 16)
)  
  



#ggsave(plot = weighted_mean_plot_future, filename = "plot/combine_fit_stat/Future_change_weighted_version_1.png", width = 18, height = 12, dpi = 600)
ggsave(plot = weighted_mean_plot_future, filename = "plot/combine_fit_stat/Future_change_weighted_version_2_ExtemeV_2nd_250521.png", width = 18, height = 12, dpi = 600)
#ggsave(plot = weighted_mean_plot_future, filename = "plot/combine_fit_stat/Future_change_weighted_version_2_SD.png", width = 18, height = 12, dpi = 600)



# Plotting (full future predictions)----- Including all predictions and not change
future_2020_2085$cultivar<- factor(future_2020_2085$cultivar, levels=rev(sort(unique(future_2020_2085$cultivar)))) # alphabetic reorder

min_value <- min(future_2020_2085$median_pred_100_year_f, na.rm = TRUE)
max_value <- max(future_2020_2085$median_pred_100_year_f, na.rm = TRUE)

# Sample heat map plot
weighted_mean_plot_future <-  ggplot(future_2020_2085, aes(x = gcm, y = cultivar, fill = median_pred_100_year_f )) +
  geom_tile() +  # Create tiles for the heat map
  facet_grid(. ~ time + ssp) +  # Facet by time (columns) and SSP (rows)
  labs(
    x =  
    '\nACCESS-CM2,    AWI-CM-1-1-MR,    CMCC-ESM2,   CNRM-CM6-1-HR,   CNRM-ESM2-1,   EC-Earth3-Veg-LR,   FGOALS-g3,\nFIO-ESM-2-0,   GFDL-ESM4,   INM-CM4-8,   INM-CM5-0,   MIROC6,   MPI-ESM1-2-LR,   MRI-ESM2-0, NESM3',
    y = "Cultivar",
    fill = "Predicted\nbloom dates",
    #title = "Heat Map of Predictions by Cultivar, GCM, Time, and SSP",

  ) +
  theme_bw() +
  theme(
    
    #Adjust grid lines
    #panel.grid.major = element_line(color = "grey90", size = 0.5),  # Lighten major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines

    # Add borders around facet panels
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),  # Border around each panel
    
    
    strip.text = element_text(size = 16),
    axis.text.x = element_blank(), 
    axis.text.y = element_text(size = 14),  # Customize y-axis text size for Cultivars
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 16),
    legend.key.height = unit(1, 'cm'),
    legend.key.width = unit(1, 'cm'),
    plot.title = element_text(hjust = 0.5, size = 16)) +
    #strip.placement = "outside")   # Places SSP labels outside the plot for clarity
      
  scale_fill_gradient(low = "lightyellow", high = "darkred") # Adjust color gradient for fill

ggsave(plot = weighted_mean_plot_future, filename = "plot/combine_fit_stat/combine_plot_Future2_weighted.png", width = 18, height = 12, dpi = 600)







#-------- Shifts in bloom days of 10% blooming for observed data-------------------#

#library(dplyr)

# Calculate the slope (rate of change) for each cultivar
average_shift <- weighted_mean_pred %>%
  group_by(cultivar) %>%
  summarise(slope = coef(lm(observed ~ year))[2],  # Extracting the slope of the linear model
            bloom_shift_between_2011_2023 = observed[year == 2023] - observed[year == 2011])

# lm(observed ~ year): Performs linear regression of observed bloom dates over year.
# coef(lm(...)): Extracts the slope, which represents the average shift in bloom date per year for each cultivar.
# A positive slope indicates the bloom date is getting later (delayed), while a negative slope indicates it's happening earlier.
# Show the result

# Slope value shows shift/delay in bloom day each year (between each year)
# bloom_shift_between_2011_2023 shows between 2011 and 2023

# Average seems between 12 to 15 days


#---------------------------------------------------------------------------------------#
# calculate the number of predictions above 7 days advancement and their percentage % for different scenarios
#---------------------------------------------------------------------------------------#

# For 2085 

#ssp126
ssp126_above7 <- Future_change_predictions %>% 
  filter(ssp == 'ssp126', year == 2085) %>%
  ungroup() %>%  # Remove any grouping
  summarise(total = n(), 
            above_7 = sum(pred_diff > 7, na.rm = FALSE))

total <-   ssp126_above7$total
#  [1] 765
above_7 <- ssp126_above7$above_7
#  [1] 43
perc_above_7 <- (above_7 / total) * 100
# [1] 5.620915%


#ssp245
ssp245_above7 <- Future_change_predictions %>% 
  filter(ssp == 'ssp245', year == 2085) %>%
  ungroup() %>%  # Remove any grouping
  summarise(total = n(), 
            above_7 = sum(pred_diff > 7, na.rm = FALSE))

total <-   ssp245_above7$total
#  [1] 765
above_7 <- ssp245_above7$above_7
#  [1] 197
perc_above_7 <- (above_7 / total) * 100
# [1] 25.75163%


#ssp585
ssp585_above7 <- Future_change_predictions %>% 
  filter(ssp == 'ssp585', year == 2085) %>%
  ungroup() %>%  # Remove any grouping
  summarise(total = n(), 
            above_7 = sum(pred_diff > 7, na.rm = FALSE))
  
total <-   ssp585_above7$total
  #  [1] 765
above_7 <- ssp585_above7$above_7
  #  [1] 192
perc_above_7 <- (above_7 / total) * 100
#  [1] 25.09804%

#---------------------------------------------------------------------------------------#



# For 2050 

#ssp126
ssp126_above7 <- Future_change_predictions %>% 
  filter(ssp == 'ssp126', year == 2050) %>%
  ungroup() %>%  # Remove any grouping
  summarise(total = n(), 
            above_7 = sum(pred_diff > 7, na.rm = FALSE))

total <-   ssp126_above7$total
#  [1] 765
above_7 <- ssp126_above7$above_7
#  [1] 50
perc_above_7 <- (above_7 / total) * 100
# [1] 6.535948%


#ssp245
ssp245_above7 <- Future_change_predictions %>% 
  filter(ssp == 'ssp245', year == 2050) %>%
  ungroup() %>%  # Remove any grouping
  summarise(total = n(), 
            above_7 = sum(pred_diff > 7, na.rm = FALSE))

total <-   ssp245_above7$total
#  [1] 765
above_7 <- ssp245_above7$above_7
#  [1] 98
perc_above_7 <- (above_7 / total) * 100
# [1] 12.81046%


#ssp585
ssp585_above7 <- Future_change_predictions %>% 
  filter(ssp == 'ssp585', year == 2050) %>%
  ungroup() %>%  # Remove any grouping
  summarise(total = n(), 
            above_7 = sum(pred_diff > 7, na.rm = FALSE))

total <-   ssp585_above7$total
#  [1] 765
above_7 <- ssp585_above7$above_7
#  [1] 111
perc_above_7 <- (above_7 / total) * 100
#  [1] 14.5098%

#---------------------------------------------------------------------------------------#
