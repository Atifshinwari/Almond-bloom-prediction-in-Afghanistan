library(tidyverse)
# library(LarsChill)
library(chillR)
# library(MEIGOR)
#evaluation function where I says that at the day of budburst the chill requirement must be met, otherwise penalty
#or should I try to have three control points (so three heat requirements? that would mean a lot of optimization on the heat requirement part)


#read evaluation function
source('util/evaluation_function.R')
#read optimization function
source('util/custom_ess.R')

#read temperature and bloom data
hour_temps <- read.csv('temp_data/hourtemps_kunduz.csv')
bloom_dates <- read.csv('D:/Phenology/almonds_phenology.csv')

#keep specific columns
hour_temps <- hour_temps[,c("Date" , "Year",  "Month", "Day",   "JDay" , "Hour" , "Temp")]
hour_temps$Temp <- round(hour_temps$Temp, 2)


#split the blooming data into calibration and validation
share_val <- 0.25
set.seed(123456)
calibration_df <- data.frame()
validation_df <- data.frame()

for(i in 1:10){
  for(cult in unique(bloom_dates$variety)){
    #subset the phenology data for the cultivar
    sub <- bloom_dates %>% 
      filter(variety == cult)
    #randomly select rows for validation
    rows_val <- sample(1:nrow(sub),size = ceiling(nrow(sub)*share_val), replace = FALSE)
    
    #select phenology data for validation based on selected rows
    sub_val <- sub[rows_val,]
    sub_val$repetition <- i
    
    sub_cal <- sub[-rows_val,]
    sub_cal$repetition <- i
    
    
    
    calibration_df <- rbind(calibration_df, 
                            sub_cal)
    
    validation_df <- rbind(validation_df, sub_val)
  }
}


years <- unique(bloom_dates$year)

#temperature data is already hourly, so split into seasons (each year)
#otherwise you need to run stack_hourly_temperatures() first
SeasonList <- hour_temps %>% 
  genSeasonList(years = years) %>% 
  set_names(years)

#number of cultivars I want to fit jointly
ncult <- 57

#fixed parameters
Tc <- 36
theta_star <- 279

#name of the file to save the fitted model parameters
fname <- 'data/parameter_combined_fitting_round_3.csv'

#read the save parameters from 1st round to get starting points for chill, heat, slope and shared parameters values from there
par_df <- read.csv("data/parameter_combined_fitting_round_1.csv")
par_df <- reformat_combined_fit_parameters(par_df)

#loop for the optimization
#I do ten repetitions because I had the feeling that the split greatly affects the outcome

#fit_list <- list()
for(i in 1:10){
  
  #take the years (we need them for the seasonlist)
  year_list <- calibration_df %>% 
    filter(repetition == i) %>% 
    split(f = ~ variety) %>% 
    purrr::map('year')
  
  #take the phenology
  pheno_list <- calibration_df %>% 
    filter(repetition == i) %>% 
    mutate(pheno = lubridate::yday(firstbloom)) %>% 
    split(f = ~ variety) %>% 
    purrr::map('pheno')
  
  
  
  #limits for the inequality constraints
  #         #gdh parameters   #q10 for E0 and E1
  c_L <- c(  0,   0,   0,     1.5, 1.5)
  c_U <- c(Inf, Inf, Inf,     3.5, 3.5)
  
  
  #options for fitter
  opts<-list(#maxeval = 1000,
    maxtime = 60 * 30, 
    maxeval = 50000,
    local_solver = 'DHC', 
    local_bestx = 1,
    inter_save = 0,
    iterprint = 1,
    plot = 1)
  
  par_sub <- par_df %>% 
    filter(repetition == i) %>% 
    arrange(cultivar)
  
  
  # We are using values from our first round, in addition, we using first values for each of shared parameters for each repitition 
  #       yc (CP)                zc (GDH)           s1  (no unit)   Tu (°C)      theta_c (°K)       tau (hours)        pie_c (hours)     Tf (°C)        Tb (°C)         slope (no unit)
  x_0 <- c(rep(45, ncult),        par_sub$zc,  rep(0.10, ncult),  par_sub$Tu[1], par_sub$theta_c[1], par_sub$tau[1],   par_sub$pie_c[1], par_sub$Tf[1],  par_sub$Tb[1],  par_sub$slope[1])
  x_U <- c(rep(80,ncult),    rep(700,ncult),    rep(1.0,ncult),    30,       287,         48,           50,            10,        10,          5.00)
  x_L <- c(rep(30,ncult),    rep(100,ncult),    rep(0.1,ncult),    15,         284,        16,           24,             2,        2,          1.2)
  #last value 5           # middle value 700  # middle value 1.2
  
  
  #x_0 <- almond_fit[[i]][[cult]]$xbest
  problem<-list(f="combined_eval_fixed", # getting data and performance check 
                x_0 = x_0,
                x_L = x_L,
                x_U = x_U,
                c_L = c_L, 
                c_U = c_U,
                vtr = 50)
  
  
  #read the parameters, check if the there are already results for the repetition
  if(file.exists(fname)){
    test <- read.csv(fname)
    if(i %in% test$repetition) next()
  }
  
  set.seed(123456789)
  fit_list <- custom_essr(problem = problem, #fit_list[[i]]
                          opts,
                          modelfn = custom_PhenoFlex_GDHwrapper, # making predictions and returns it to combined_eval_fixed
                          bloomJDays = pheno_list,
                          SeasonList = purrr::map(year_list, function(years){
                            SeasonList[as.character(years)]}), 
                          ncult = 57,
                          Tc = Tc, 
                          theta_star = theta_star 
  ) 
  
  #save outcome in a table, append the table
  data.frame(parameter = c(rep('yc', ncult),
                           rep('zc', ncult),
                           rep('s1', ncult),
                           'Tu', 'theta_c', 'tau', 'pie_c', 'Tf', 'Tb', 'slope', 'Tc', 'theta_star'),
             cultivar = c(rep(unique(calibration_df$variety),3),rep('all',9)),
             repetition = i,
             value = c(fit_list$xbest, Tc, theta_star)) %>%
    write.table(file = fname,
                append = TRUE,
                row.names = FALSE,
                sep = ',',
                col.names=!file.exists(fname))
  
}



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

#now we can do all sorts with that data:
#   -calculate model performance (RMSE, RPIQ, bias)
#   -make prediction vs observation plots
#   -see how different the model performed for the different splits or cultivars
#   -at this point you can be creative

rmse_df <- pred_obs_df %>% 
  group_by(cultivar, repetition, split) %>% 
  summarise(rmse = chillR::RMSEP(predicted = predicted, observed = observed, na.rm= TRUE)) # I found only 1 NA value for Abdulwahidi for 2023

#calculating rpiq for small sample sizes is a bit unfair, so we will calculate
#a variant of rpiq, where the interquartile range is calculate before 
#splitting into calibration / validation
iqr_df <- pred_obs_df %>% 
  group_by(cultivar, repetition) %>% 
  summarise(iqr = IQR(observed))

rmse_ipiq_df <- pred_obs_df %>% 
  group_by(cultivar, repetition, split) %>% 
  summarise(rmse = chillR::RMSEP(predicted = predicted, observed = observed, na.rm= TRUE)) %>% # the same NA
  ungroup() %>% 
  merge(iqr_df, by = c('cultivar', 'repetition')) %>% 
  mutate(rpiq_adj = iqr / rmse)

# I save long_df for reading lateron for biological meaning
write.csv(rmse_ipiq_df, "D:/Phenology/data/rmse_ipiq_df_3.csv", row.names = FALSE)

# plot: predicted vs observed plot
# first rename colnames for the purpose of legend
pred_obs_df <- pred_obs_df %>% 
  rename(Cultivar = cultivar,
         Split = split)

# in case I want to put combine values of rmse and rpiq for all varieties together for each repetition
summary_stats_df <- rmse_ipiq_df %>%
  group_by(repetition, split) %>%
  summarise(
    mean_rmse = mean(rmse),
    mean_rpiq = mean(rpiq_adj))


combine_perfo_plot <- pred_obs_df %>% 
  ggplot(aes(x = observed, y = predicted, col = Cultivar, shape = Split)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  
  facet_wrap(~repetition) +
  
  labs(#title = "",#                                                                       
    x = 'Observed Bloom Dates',
    y = "Predicted Bloom Dates") +
  
  theme_bw()


combine_perfo_plot <- combine_perfo_plot +
  geom_text(
    data = summary_stats_df %>% filter(split == "Calibration"),
    aes(x = 42, y = 105, label = paste0("Cal RMSE: ", sprintf("%.2f", mean_rmse))), # sprintf is to round the values, it did not work normally as it removes the second zero
    inherit.aes = FALSE, hjust = 0.5, vjust = 0, size = 3, color = "black") +
  geom_text(
    data = summary_stats_df %>% filter(split == "Calibration"),
    aes(x = 42, y = 101, label = paste0("Cal RPIQ:  ", sprintf("%.2f", mean_rpiq))),
    inherit.aes = FALSE, hjust = 0.5, vjust = 0, size = 3, color = "black") +
  geom_text(
    data = summary_stats_df %>% filter(split == "Validation"),
    aes(x = 42, y = 97, label = paste0("Val RMSE: ", sprintf("%.2f", mean_rmse))),
    inherit.aes = FALSE, hjust = 0.5, vjust = 0, size = 3, color = "black") +
  geom_text(
    data = summary_stats_df %>% filter(split == "Validation"),
    aes(x = 42, y = 93, label = paste0("Val RPIQ:  ", sprintf("%.2f", mean_rpiq))),
    inherit.aes = FALSE, hjust = 0.5, vjust = 0, size = 3, color = "black") + 
  
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


ggsave(plot = combine_perfo_plot, filename = "plot/combine_fit_stat/combine_plot_R3.png", width = 16, height = 10, dpi = 600)


# plot with box plot for RMSE and RPIQ

library(forcats) # for making the alphabetic order from A to Z on the plot for cultivar names

# Reshape the data
long_df <- rmse_ipiq_df %>%
  pivot_longer(cols = c(rmse, rpiq_adj), names_to = "metric", values_to = "value")

# remove outlier
# Filter out big outliers in rpiq_adj
long_df <- long_df %>%
  filter(!(metric == "rpiq_adj" & value > 40))


long_df$cultivar <- factor(long_df$cultivar, levels = rev(sort(unique(long_df$cultivar)))) # alphabetic order

# Custom facet labels (if needed)
custom_labels <- c(
  "rmse" = "Root Mean Squared Error of Prediction (RMSEP)",
  "rpiq_adj" = "Ratio of Performance to Inter-Quartile distance (RPIQ)")

RMSE_box_A <- ggplot(data = long_df, aes(x = value, y = cultivar, fill = split)) +
  geom_boxplot() +
  facet_wrap(metric ~ ., scales = 'free_x', strip.position = "bottom", labeller = labeller(metric = custom_labels)) +  # Uniform spacing
  
  labs(#title = "",#                                                                       
    x = NULL,
    y = "Almond Cultivars") +
  
  scale_x_continuous(breaks = seq(0, max(long_df$value, na.rm = TRUE), by = 5)) +  # Customize x-axis breaks
  
  
  scale_fill_manual(values = c("deepskyblue", "#FF6F80"), 
                    breaks = unique(rmse_ipiq_df$split)) +
  
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 18), 
        axis.title.x = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 16),
        strip.placement = "outside")
ggsave(plot = RMSE_box_A, filename = "plot/box plot/RMSE_box_R3.png", width = 15, height = 10, dpi = 600)
ggsave(plot = RMSE_box_A, filename = "plot/box plot/RMSE_box_R3_Outlier.png", width = 15, height = 10, dpi = 600)

# outliers at 30, 150 of RPIQ for one variety = Qaharbai

# I save long_df for reading lateron for biological meaning
write.csv(long_df, "D:/Phenology/data/long_df_round_3.csv", row.names = FALSE)




