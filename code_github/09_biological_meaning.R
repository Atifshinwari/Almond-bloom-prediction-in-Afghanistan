###                    ###
### Biological meaning ###
###                    ###

library(dplyr)
library(lubridate)
library(chillR)
library(ggplot2)

# Early and late flowering varieties
almonds_phen_early_late <- read.csv("D:/Phenology/almonds_phenology.csv")


# Ensure that the Date column is in Date format
almonds_phen_early_late$firstbloom <- as.Date(almonds_phen_early_late$firstbloom)
almonds_phen_early_late$bloom_50 <- as.Date(almonds_phen_early_late$bloom_50)
almonds_phen_early_late$fullbloom <- as.Date(almonds_phen_early_late$fullbloom)

# Defining early and late (Source: according to the National Collection of Almonds)
# very early (before February 26), 
# early (from February 26 to March 2), 
# medium (from March 3 to March 6), 
# late (from March 7 to March 11), 
# very late (from March 12)

# Assuming 'firstbloom' is a date or date-time column
date_range <- almonds_phen_early_late %>%
  filter(year(firstbloom) == 2014) %>%  # Filter for the year 2011
  group_by(variety) %>%
  summarise(Earliest_first = min(firstbloom, na.rm = TRUE))



# Convert the 'Earliest_first' dates to numeric values (days since an origin date, e.g., "1970-01-01")
date_range_numeric <- date_range %>%
  mutate(Earliest_first_num = as.numeric(Earliest_first - as.Date("1970-01-01")))

# Calculate quantile thresholds in numeric format
thresholds_numeric <- date_range_numeric %>%
  summarise(
    Early_thresh_num = quantile(Earliest_first_num, probs = 0.33, na.rm = TRUE),
    Late_thresh_num = quantile(Earliest_first_num, probs = 0.67, na.rm = TRUE)
  )

# Convert numeric thresholds back to Date format
thresholds <- thresholds_numeric %>%
  mutate(
    Early_thresh = as.Date(Early_thresh_num, origin = "1970-01-01"),
    Late_thresh = as.Date(Late_thresh_num, origin = "1970-01-01")
  ) %>%
  select(Early_thresh, Late_thresh)

# Categorize varieties based on thresholds
date_range <- date_range_numeric %>%
  mutate(
    Category = case_when(
      Earliest_first <= thresholds$Early_thresh ~ "Early",
      Earliest_first <= thresholds$Late_thresh ~ "Mid",
      TRUE ~ "Late" # I exclude this to keep only early and mid flowering
    )
  ) %>%
  select(-Earliest_first_num)  # Drop the numeric column

print(date_range)




# Relating flowering times with predictions performance of cultivars
# Read flowering times
almond_early_late <- read.csv("D:/Phenology/biological_meaning.csv")

almond_early_late <- almond_early_late %>% 
  select(-X, -Not.available.in.N.collection,   -X.1,  -Notes.from.almond..national.collection) %>% # delete columns
  filter(!cultivar %in% c("Kaghazi Chahar Dara", "Kheraji", "Quruti", "Said Bai", "Sangak", "Sattarbai Chobak", "")) # delte rows

# I will read the 
long_df_round_3 <- read.csv("D:/Phenology/data/long_df_round_3.csv")

long_df_round_3 <- long_df_round_3 %>% 
  merge(almond_early_late, by = c("cultivar"))

# Custom facet labels (if needed)
custom_labels <- c(
  "rmse" = "Root Mean Squared Error of Prediction (RMSEP)",
  "rpiq_adj" = "Ratio of Performance to Inter-Quartile distance (RPIQ)")

# read rmse ipiq for scale x manual
rmse_ipiq_df_3 <- read.csv("D:/Phenology/data/rmse_ipiq_df_3.csv")

# Reorder the cultivar based on Flower.time
#long_df_round_3$cultivar <- factor(long_df_round_3$cultivar, levels = rev(sort(unique(long_df_round_3$Flower.time)))) # order by flowering time

#library(forcats)
# Ensure Flower.time has the desired order
long_df_round_3$Flower.time <- factor(long_df_round_3$Flower.time, 
                                      levels = c("V.Early", "Early", "Mid", "Late", "V.Late"))

# Plot with the cultivars grouped by Flower.time
# The steps below are performed to reorder the RMSE based on median rmse for each cultivar to get highest and lowest values for each flowertime group

# Filter the dataframe to include only rows where metric is 'rmse'
rmse_data <- long_df_round_3 %>%
  filter(metric == "rmse")

# Calculate the average RMSE for each cultivar within each Flower.time, across both Calibration and Validation
rmse_order <- rmse_data %>%
  group_by(Flower.time, cultivar) %>%
  summarize(avg_rmse = mean(value, na.rm = TRUE)) %>%
  arrange(Flower.time, desc(avg_rmse))  # Arrange by descending RMSE to get high values first

# Reorder the cultivar factor levels based on average RMSE within each Flower.time, reversing the order
long_df_round_3 <- long_df_round_3 %>%
  mutate(cultivar = factor(cultivar, levels = rev(rmse_order$cultivar)))  # Reverse the order

# Plotting
RMSE_box_A <- ggplot(data = long_df_round_3, aes(x = value, y = cultivar, fill = split)) +
  geom_boxplot() +
  #facet_wrap(metric ~ ., scales = 'free_x', strip.position = "bottom", labeller = labeller(metric = custom_labels)) +
  
  facet_grid(Flower.time ~ metric, scales = 'free', space = 'free_y', 
             labeller = labeller(metric = custom_labels), switch = 'x') +  # Adding Flower.time as a facet
  labs(
    x = NULL,
    y = "Almond Cultivars"
  ) +
  
  scale_x_continuous(breaks = seq(0, max(long_df_round_3$value, na.rm = TRUE), by = 5)) +
  
  scale_fill_manual(values = c("deepskyblue", "#FF6F80"), breaks = unique(rmse_ipiq_df_3$split)) +
  
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    strip.text = element_text(size = 14),
    strip.text.y.right = element_text(size = 10),
    strip.placement = "outside")
    #panel.spacing = unit(1, 'lines'))

# Save the plot
#ggsave(plot = RMSE_box_A, filename = "plot/box plot/RMSE_box_FlowerTime_3.png", width = 15, height = 10, dpi = 600)
ggsave(plot = RMSE_box_A, filename = "plot/box plot/RMSE_box_FlowerTime_3_reoderRMSE.png", width = 15, height = 10, dpi = 600)








