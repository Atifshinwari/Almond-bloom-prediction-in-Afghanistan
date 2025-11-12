


# Summarizing advances and delays %:
library(dplyr)

summary_df <- Future_change_predictions %>%
  filter(year %in% c(2050, 2085), ssp %in% c("ssp126", "ssp245", "ssp585")) %>%
  group_by(ssp, year) %>%
  summarise(
    total = n(),
    n_advance = sum(pred_diff > 0, na.rm = TRUE),
    n_delay = sum(pred_diff < 0, na.rm = TRUE),
    perc_advance = round((n_advance / total) * 100, 2),
    perc_delay = round((n_delay / total) * 100, 2),
    advance_above_7 = sum(pred_diff > 7, na.rm = TRUE),
    delays_below_5  = sum(pred_diff < -5, na.rm = TRUE),
    perc_advance_7  = round((advance_above_7 / total) * 100, 2),
    perc_delay_5    = round((delays_below_5 / total) * 100, 2),
    .groups = "drop"
  )

print(summary_df)

write.csv(summary_df, "data/future/advances_delays.csv")


#---------------------------------------------------------------------------------------#
# calculate the number of predictions above 7 days advancement and their percentage % for different scenarios
#---------------------------------------------------------------------------------------#

# In prediction_difference, the positive value shows advance and -ve value show delay compared to 2020

# Advances

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


# Delays 



# For 2085 

#ssp126
ssp126_below5 <- Future_change_predictions %>% 
  filter(ssp == 'ssp126', year == 2085) %>%
  ungroup() %>%  # Remove any grouping
  summarise(total = n(), 
            below_5 = sum(pred_diff < -5, na.rm = FALSE)) # below minus 5

total <-   ssp126_below5$total
#  [1] 765
below_5 <- ssp126_below5$below_5
#  [1] 0
perc_below_5 <- (below_5 / total) * 100
# [1] 0%


#ssp245
ssp245_below5 <- Future_change_predictions %>% 
  filter(ssp == 'ssp245', year == 2085) %>%
  ungroup() %>%  # Remove any grouping
  summarise(total = n(), 
            below_5 = sum(pred_diff < -5, na.rm = FALSE))

total <-   ssp245_below5$total
#  [1] 765
below_5 <- ssp245_below5$below_5
#  [1] 0
perc_below_5 <- (below_5 / total) * 100
# [1] 0%


#ssp585
ssp585_below5 <- Future_change_predictions %>% 
  filter(ssp == 'ssp585', year == 2085) %>%
  ungroup() %>%  # Remove any grouping
  summarise(total = n(), 
            below_5 = sum(pred_diff < -5, na.rm = FALSE))

total <-   ssp585_below5$total
#  [1] 765
below_5 <- ssp585_below5$below_5
#  [1] 26
perc_below_5 <- (below_5 / total) * 100
#  [1] 3.398693%

#---------------------------------------------------------------------------------------#



# For 2050 


#ssp126
ssp126_below5 <- Future_change_predictions %>% 
  filter(ssp == 'ssp126', year == 2050) %>%
  ungroup() %>%  # Remove any grouping
  summarise(total = n(), 
            below_5 = sum(pred_diff < -5, na.rm = FALSE)) # below minus 5

total <-   ssp126_below5$total
#  [1] 765
below_5 <- ssp126_below5$below_5
#  [1] 0
perc_below_5 <- (below_5 / total) * 100
# [1] 0.2614379%


#ssp245
ssp245_below5 <- Future_change_predictions %>% 
  filter(ssp == 'ssp245', year == 2050) %>%
  ungroup() %>%  # Remove any grouping
  summarise(total = n(), 
            below_5 = sum(pred_diff < -5, na.rm = FALSE))

total <-   ssp245_below5$total
#  [1] 765
below_5 <- ssp245_below5$below_5
#  [1] 0
perc_below_5 <- (below_5 / total) * 100
# [1] 0%


#ssp585
ssp585_below5 <- Future_change_predictions %>% 
  filter(ssp == 'ssp585', year == 2050) %>%
  ungroup() %>%  # Remove any grouping
  summarise(total = n(), 
            below_5 = sum(pred_diff < -5, na.rm = FALSE))

total <-   ssp585_below5$total
#  [1] 765
below_5 <- ssp585_below5$below_5
#  [1] 0
perc_below_5 <- (below_5 / total) * 100
#  [1] 0%



#Variables	                       2050			                   2085		
#                        ssp126	ssp245	ssp585	     ssp126	 ssp245	 ssp585
#Total prediction (days)	765	    765	    765	         765	   765	   765
#Advance (days)	          50	    98	    111	         43	     197     192
#Advance (%)	            7	      13	    15	         6	     26	     25

#Total prediction (days)	765	    765	    765	         765	   765	   765
#Delay (days)	            0	      0	      0	           0	     0	     26
#Delay (%)	              0	      0	      0	           0	     0	     3


