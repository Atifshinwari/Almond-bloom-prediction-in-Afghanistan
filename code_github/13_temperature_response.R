
#------------------------- Chill and heat response plot---------------------#
#---------------------------------------------------------------------------#


#devtools::install_github('https://github.com/larscaspersen/addition_chillR')

library(ggplot2)
library(tidyr)
library(chillR)
library(LarsChill)

source('util/convenience_functions.R')
par_df <- read.csv('parameter_combined_fitting_round_3.csv') %>% 
  reformat_combined_fit_parameters() 

par_converted <- purrr::map(1:nrow(par_df), function(i){
  par_df[i, par_names] %>% 
    unlist() %>% 
    convert_parameters() 
}) %>% 
  bind_rows()

colnames(par_converted)[5:8] <- c('E0', 'E1', 'A0', 'A1')
par_df <- cbind(par_df, par_converted[, 5:8])
rm(par_converted)
par_names_old <- par_names
par_names_old[5:8] <- c('E0', 'E1', 'A0', 'A1')

temp_values = seq(-5, 30, 0.1)

par_sub <- par_df %>% 
  filter(cultivar == 'Belabai') # We are doing this for one cultivar b/c we consider similar agro-climatic conditions for all cultivars

temp_response_list <- purrr::map(1:nrow(par_sub), function(i){
  par_sub[i, par_names_old] %>% 
    unlist() %>% 
    LarsChill::get_temp_response_df(temp_values = temp_values) %>% 
    mutate(cultivar = par_sub$cultivar[i],
           repetition = par_sub$repetition[i]) %>% 
    return()
}, .progress = TRUE)

repetition_test <- repetition

temp_response_list %>% 
  bind_rows() %>% 
pivot_longer(c(Chill_response,
               Heat_response)) %>% 
ggplot(aes(x = Temperature,
           y = value)) +
  geom_line(linewidth = 2,
            aes(col = as.factor(repetition), group = as.factor(repetition))) +
  ylab("Temperature response (arbitrary units)") +
  xlab("Temperature (°C)") +
  facet_wrap(vars(name),
             scales = "free",
             labeller = 
               labeller(name = c(Chill_response = c("Chill response"),
                                 Heat_response = c("Heat response")))) +
  # scale_color_manual(values = c("Chill_response" = "blue",
  #                               "Heat_response" = "red")) +
  theme_bw(base_size = 15)  +
  theme(legend.position = "right")

ggsave("plot/combine_fit_stat/temp_response/temp_response_plot.png", width = 12, height = 8, dpi = 300)

# repetition 9 and 6 are the once below zero
















