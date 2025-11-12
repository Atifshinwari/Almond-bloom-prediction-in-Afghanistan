library(tidyverse)
# library(LarsChill)
library(chillR)
# library(MEIGOR)

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

# # Try with few foreign varieties (I comment this out b/c I tried this for a test base foregin cultivars)
# bloom_dates <- subset(bloom_dates, variety %in% c("Cardinal", "Carmel", "Ferraduel", "Ferragnes", "Lauranne", "Nonpareil"))
# 
# bloom_dates <- bloom_dates[,c("year", "variety", "budbreak", "firstbloom", "fullbloom")]


#split data into calibration and validation
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
fname <- 'data/parameter_combined_fitting_round_2.csv'

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
  
  # Use of ranges for upper and lower limit: but we ignore it at the moment b/c the there is already very broad
  # par_ranges_cultivar <- par_df %>%
  #   group_by(cultivar) %>% 
  #   summarise(yc_min = min(yc),
  #             yc_max = max(yc),
  #             zc_min = min(zc),
  #             zc_max = max(zc),
  #             s1_min = min(s1),
  #             s1_max = max(s1))
  
  
  # We are using values from our first round, in addition, we using first values for each of shared parameters for each repitition 
  #       yc (CP)                zc (GDH)           s1  (no unit)   Tu (°C)      theta_c (°K)       tau (hours)        pie_c (hours)     Tf (°C)        Tb (°C)         slope (no unit)
  x_0 <- c(par_sub$yc,         par_sub$zc,           par_sub$s1,  par_sub$Tu[1], par_sub$theta_c[1], par_sub$tau[1],   par_sub$pie_c[1], par_sub$Tf[1],  par_sub$Tb[1],  par_sub$slope[1])
  x_U <- c(rep(80,ncult),    rep(700,ncult),    rep(1.2,ncult),    30,       287,         48,           50,            10,        10,          5.00)
  x_L <- c(rep(5,ncult),    rep(100,ncult),    rep(0.1,ncult),    15,         284,        16,           24,             2,        2,          1.2)
  #last value 5           # middle value 700  # middle value 1.2
  
  
  # #take fitted golden delicious as a starting point 
  # #       yc (CP)                zc (GDH)           s1  (no unit)   Tu (°C) theta_c (°K)   tau (hours)    pie_c (hours) Tf (°C)      Tb (°C)      slope (no unit)
  # x_0 <- c(rep(41.12,ncult), rep(271.17, ncult), rep(0.10, ncult),  26.730, 285.14,       44.09,        24.51,         3.12,      2.02,        1.46)
  # x_U <- c(rep(80,ncult),    rep(700,ncult),    rep(1.2,ncult),    30,       287,         48,           50,            10,        10,          5.00)
  # x_L <- c(rep(5,ncult),    rep(100,ncult),    rep(0.1,ncult),    15,         284,        16,           24,             2,        2,          1.2)
  # #last value 5           # middle value 700  # middle value 1.2
  
  #x_0 <- almond_fit[[i]][[cult]]$xbest
  problem<-list(f="combined_eval_fixed", # getting data and performance check 
                x_0 = x_0,
                x_L = x_L,
                x_U = x_U,
                c_L = c_L, 
                c_U = c_U,
                vtr = 50)
  
  
  
  # SeasonList = purrr::map(year_list, function(years){
  #   SeasonList[as.character(years)]
  # })
  
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
fname <- 'data/parameter_combined_fitting_round_2.csv'

par_df2 <- read.csv(fname)

source('util/convenience_functions.R')

par_df2 <- reformat_combined_fit_parameters(par_df2)


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
pred_obs_df <- purrr::map(1:nrow(par_df2), function(i){
  pred <- par_df2[i, par_names] %>% #take model parameters
    unlist() %>%  #make it a vector
    unname() %>%  #get rid of annoying names
    convert_parameters() %>% #convert to 'old' parameter format
    return_predicted_days(SeasonList = SeasonList) #predict bloomdates
  
  data.frame(cultivar = par_df2$cultivar[i],
             repetition = par_df2$repetition[i],
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

#predicted vs observed plot
# in case I want to put combine values of rmse and rpiq for all varieties together for each repetition
summary_stats_df <- rmse_ipiq_df %>%
  group_by(repetition, split) %>%
  summarise(
    mean_rmse = mean(rmse),
    mean_rpiq = mean(rpiq_adj))

combine_perfo_plot <- pred_obs_df %>% 
  ggplot(aes(x = observed, y = predicted, col = cultivar, shape = split)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  
  # scale_x_continuous(breaks=        c(-31,  0, 32, 60,  91, 121, 152), 
  #                    minor_breaks = c(-45, -15, 15, 46, 74, 105, 135, 166), 
  #                    labels = c('Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun'),
  #                    limits = c(91, 130)) +
  # scale_y_continuous(breaks=        c(-31,  0, 32, 60,  91, 121, 152), 
  #                    minor_breaks = c(-45, -15, 15, 46, 74, 105, 135, 166), 
  #                    labels = c('Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun'), 
  #                    limits = c(91, 130)) +
  #facet_grid(~repetition) +
  facet_wrap(~repetition) +
  
  theme_bw()

combine_perfo_plot <- combine_perfo_plot +
  geom_text(
    data = summary_stats_df %>% filter(split == "Calibration"),
    aes(x = 60, y = 105, label = paste0("Cal RMSE: ", round(mean_rmse, 2))),
    inherit.aes = FALSE, hjust = 0.5, vjust = 0, size = 3, color = "black") +
  geom_text(
    data = summary_stats_df %>% filter(split == "Calibration"),
    aes(x = 60, y = 101, label = paste0("Cal RPIQ: ", round(mean_rpiq, 2))),
    inherit.aes = FALSE, hjust = 0.5, vjust = 0, size = 3, color = "black") +
  geom_text(
    data = summary_stats_df %>% filter(split == "Validation"),
    aes(x = 60, y = 97, label = paste0("Val RMSE: ", round(mean_rmse, 2))),
    inherit.aes = FALSE, hjust = 0.5, vjust = 0, size = 3, color = "black") +
  geom_text(
    data = summary_stats_df %>% filter(split == "Validation"),
    aes(x = 60, y = 93, label = paste0("Val RPIQ: ", round(mean_rpiq, 2))),
    inherit.aes = FALSE, hjust = 0.5, vjust = 0, size = 3, color = "black")


ggsave(plot = combine_perfo_plot, filename = "plot/combine_fit_stat/combine_plot_R2.png", width = 16, height = 10, dpi = 600)







