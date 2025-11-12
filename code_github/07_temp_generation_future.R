#Loading the stations data 
require(ggplot2)
require(lubridate)
require(tidyverse)
require(chillR)
require(kableExtra)
require(Metrics)
require(hydroGOF)
require(MBC)
library(readxl)
library(dplyr)
library(jalcal)

# Formatting temperature data #


years <- c(2008:2023)
datasets <- paste("PuliAlchin_AT", years, sep = "_")

for (i in 1:length(years)) {
  
  PuliAlchin_AT <- read_excel("temp_data/Chahar_Dara_AT.xlsx",range = "A7:AK38",.name_repair = tolower, sheet = as.character(years[i]))
  
  colnames(PuliAlchin_AT)[1] <- "Day"
  
  jan <-
    data.frame(Month = 1,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[2],
               Tmax = PuliAlchin_AT[3],
               Tmean = PuliAlchin_AT[4])
  feb <-
    data.frame(Month = 2,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[5],
               Tmax = PuliAlchin_AT[6],
               Tmean = PuliAlchin_AT[7])
  mar <-
    data.frame(Month = 3,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[8],
               Tmax = PuliAlchin_AT[9],
               Tmean = PuliAlchin_AT[10])
  apr <-
    data.frame(Month = 4,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[11],
               Tmax = PuliAlchin_AT[12],
               Tmean = PuliAlchin_AT[13])
  mai <-
    data.frame(Month = 5,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[14],
               Tmax = PuliAlchin_AT[15],
               Tmean = PuliAlchin_AT[16])
  jun <-
    data.frame(Month = 6,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[17],
               Tmax = PuliAlchin_AT[18],
               Tmean = PuliAlchin_AT[19])
  jul <-
    data.frame(Month = 7,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[20],
               Tmax = PuliAlchin_AT[21],
               Tmean = PuliAlchin_AT[22])
  aug <-
    data.frame(Month = 8,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[23],
               Tmax = PuliAlchin_AT[24],
               Tmean = PuliAlchin_AT[25])
  sep <-
    data.frame(Month = 9,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[26],
               Tmax = PuliAlchin_AT[27],
               Tmean = PuliAlchin_AT[28])
  okt <-
    data.frame(Month = 10,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[29],
               Tmax = PuliAlchin_AT[30],
               Tmean = PuliAlchin_AT[31])
  nov <-
    data.frame(Month = 11,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[32],
               Tmax = PuliAlchin_AT[33],
               Tmean = PuliAlchin_AT[34])
  dez <-
    data.frame(Month = 12,
               Day = PuliAlchin_AT[1],
               Tmin = PuliAlchin_AT[35],
               Tmax = PuliAlchin_AT[36],
               Tmean = PuliAlchin_AT[37])
  
  PuliAlchin <- rbind(jan,feb,mar,apr,mai,jun,jul,aug,sep,okt,nov,dez)
  
  PuliAlchin <- 
    PuliAlchin %>% 
    mutate(Year = years[i]) %>% 
    rename(Tmin = min,
           Tmax = max,
           Tmean = avg) 
  
  
  PuliAlchin$Date <- as.Date(paste0(PuliAlchin$Year,"-",PuliAlchin$Month,"-",PuliAlchin$Day))
  PuliAlchin <- dplyr::filter(PuliAlchin, !is.na(Date))
  
  PuliAlchin$Prec <- NA 
  
  PuliAlchin <- PuliAlchin[c(7,6,1,2,3,4,5,8)]#Re ordering the columns
  
  assign(datasets[i],
         PuliAlchin)
  
}

PuliAlchin <- rbind(PuliAlchin_AT_2008,
                    PuliAlchin_AT_2009,
                    PuliAlchin_AT_2010,
                    PuliAlchin_AT_2011,
                    PuliAlchin_AT_2012,
                    PuliAlchin_AT_2013,
                    PuliAlchin_AT_2014,
                    PuliAlchin_AT_2015,
                    PuliAlchin_AT_2016,
                    PuliAlchin_AT_2017,
                    PuliAlchin_AT_2018,
                    PuliAlchin_AT_2019,
                    PuliAlchin_AT_2020,
                    PuliAlchin_AT_2021,
                    PuliAlchin_AT_2022,
                    PuliAlchin_AT_2023)
rm(PuliAlchin_AT,
   PuliAlchin_AT_2008,
   PuliAlchin_AT_2009,
   PuliAlchin_AT_2010,
   PuliAlchin_AT_2011,
   PuliAlchin_AT_2012,
   PuliAlchin_AT_2013,
   PuliAlchin_AT_2014,
   PuliAlchin_AT_2015,
   PuliAlchin_AT_2016,
   PuliAlchin_AT_2017,
   PuliAlchin_AT_2018,
   PuliAlchin_AT_2019,
   PuliAlchin_AT_2020,
   PuliAlchin_AT_2021,
   PuliAlchin_AT_2022,
   PuliAlchin_AT_2023,
   jan,feb,mar,apr,mai,jun,jul,aug,sep,okt,nov,dez,
   years,datasets,
   i)

write.csv(PuliAlchin,"temp_data/ChaharDara_AT_Observe.csv", row.names = FALSE)



## Bias correction of simulated data


Adraskan_Obs <- read.csv("temp_data/ChaharDara_AT_Observe.csv")
Adraskan_Sim <- read.csv("temp_data/Chahardara_Simulated.csv")

colnames(Adraskan_Sim) <-  c("Date", "Tmin_Sim", "Tmax_Sim")
Adraskan_Sim$Tmin_Sim <- as.numeric(Adraskan_Sim$Tmin_Sim)
Adraskan_Sim$Tmax_Sim <- as.numeric(Adraskan_Sim$Tmax_Sim)

colnames(Adraskan_Obs)[5] <- "Tmin_Obs"
colnames(Adraskan_Obs)[6] <- "Tmax_Obs"
Adraskan_Obs$Tmin_Obs <- as.numeric(Adraskan_Obs$Tmin_Obs)
Adraskan_Obs$Tmax_Obs <- as.numeric(Adraskan_Obs$Tmax_Obs)

Adraskan_Sim$Date <- as.Date(Adraskan_Sim$Date, format = "%d/%m/%Y")

# Adraskan_Obs$Date <- as.Date(Adraskan_Obs$Date)  # Ensure Date is in Date format
# Adraskan_Sim$Date <- as.Date(Adraskan_Sim$Date)  # Ensure Date is in Date format

start_date <- as.Date(Adraskan_Obs$Date[1])
end_date <- as.Date(Adraskan_Obs$Date[nrow(Adraskan_Obs)])

Adraskan_Sim <- Adraskan_Sim[as.Date(Adraskan_Sim$Date) >=  start_date & as.Date(Adraskan_Sim$Date) <=  end_date,]


# Adraskan_Obs <- Adraskan_Obs[c("Tmin_Obs", "Tmax_Obs")]
# Adraskan_Sim <- Adraskan_Sim[c("Tmin_Sim", "Tmax_Sim")]

Adraskan_Obs$Date <- as.Date(Adraskan_Obs$Date)

Adraskan_Obs_Sim <- merge(Adraskan_Obs, Adraskan_Sim, by = c("Date"))
Adraskan_Obs_Sim <- Adraskan_Obs_Sim[c("Date", "Year", "Month", "Day", "Tmin_Obs", 
                                       "Tmax_Obs", "Tmin_Sim", "Tmax_Sim")]

Adraskan_Sim_His <- read.csv("temp_data/Chahardara_Simulated.csv")
colnames(Adraskan_Sim_His) <-  c("Date", "Tmin_Sim", "Tmax_Sim")
Adraskan_Sim_His$Tmin_Sim <- as.numeric(Adraskan_Sim_His$Tmin_Sim)
Adraskan_Sim_His$Tmax_Sim <- as.numeric(Adraskan_Sim_His$Tmax_Sim)
Adraskan_Sim_His$Date <- as.Date(Adraskan_Sim_His$Date, format = "%d/%m/%Y")

start_date2 <- as.Date("1980-01-01")
Adraskan_Sim_His <- Adraskan_Sim_His[as.Date(Adraskan_Sim_His$Date) >=  as.Date(start_date2) & as.Date(Adraskan_Sim_His$Date) <  start_date,]

# Adraskan_Sim_His <- Adraskan_Sim_His[c("Tmin_Sim", "Tmax_Sim")]

QDM_Adraskan_Tmin <- MBC::QDM(o.c = Adraskan_Obs$Tmin_Obs, m.c = Adraskan_Sim$Tmin_Sim, m.p = Adraskan_Sim_His$Tmin_Sim, ratio=FALSE, trace=0.05, trace.calc=0.5*trace,
                              jitter.factor=0, n.tau=NULL, ratio.max=2,
                              ratio.max.trace=10*trace, ECBC=FALSE, ties='first',
                              subsample=NULL, pp.type=7)
QDM_Adraskan_Tmax <- MBC::QDM(o.c = Adraskan_Obs$Tmax_Obs, m.c = Adraskan_Sim$Tmax_Sim, m.p = Adraskan_Sim_His$Tmax_Sim, ratio=FALSE, trace=0.05, trace.calc=0.5*trace,
                              jitter.factor=0, n.tau=NULL, ratio.max=2,
                              ratio.max.trace=10*trace, ECBC=FALSE, ties='first',
                              subsample=NULL, pp.type=7)


# Combining all calibration period
Adraskan_BC_Cal <- cbind(Adraskan_Obs_Sim, QDM_Adraskan_Tmin$mhat.c)
Adraskan_BC_Cal <- cbind(Adraskan_BC_Cal, QDM_Adraskan_Tmax$mhat.c)
colnames(Adraskan_BC_Cal)[9] <- "QDM_Tmin"
colnames(Adraskan_BC_Cal)[10] <- "QDM_Tmax"

# Combining all historic period
Adraskan_BC_His <- cbind(Adraskan_Sim_His, QDM_Adraskan_Tmin$mhat.p)
Adraskan_BC_His <- cbind(Adraskan_BC_His, QDM_Adraskan_Tmax$mhat.p)
colnames(Adraskan_BC_His)[4] <- "QDM_Tmin_His"
colnames(Adraskan_BC_His)[5] <- "QDM_Tmax_His"

# Plotting
# Plotting Calibration Period
Adraskan_TminPlot_Cal <- ggplot(data = Adraskan_BC_Cal, aes(x = Tmin_Obs, y = Tmin_Sim)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, linetype = "dashed") +
  xlab("Tmin Observed") + 
  ylab("Tmin Simulated") + 
  scale_y_continuous(limits = c(-30,45)) +
  scale_x_continuous(limits = c(-30,45)) +
  #facet_wrap( ~ format(x = as.Date(Date))) +
  #facet_wrap( ~ format(as.Date(Date), "%Y"))
  facet_wrap(~Year)
print(Adraskan_TminPlot_Cal)

ggsave("temp_data/Adraskan_TminPlot_Cal.png",width = 20,height = 10, units = "cm",dpi = 600)

Adraskan_TmaxPlot_Cal <- ggplot(data = Adraskan_BC_Cal, aes(x = Tmax_Obs, y = Tmax_Sim)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, linetype = "dashed") +
  xlab("Tmin Observed") + 
  ylab("Tmin Simulated") + 
  scale_y_continuous(limits = c(-30,45)) +
  scale_x_continuous(limits = c(-30,45)) +
  #facet_wrap( ~ format(x = as.Date(Date))) +
  #facet_wrap( ~ format(as.Date(Date), "%Y"))
  facet_wrap(~Year)
print(Adraskan_TmaxPlot_Cal)

ggsave("temp_data/Adraskan_TmaxPlot_Cal.png",width = 20,height = 10, units = "cm",dpi = 600)

# Plotting Bias Corrected Calibration Period
Adraskan_TminPlot_CalBC <- ggplot(data = Adraskan_BC_Cal, aes(x = Tmin_Obs, y = QDM_Tmin)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, linetype = "dashed") +
  xlab("Tmin Observed") + 
  ylab("Tmin Simulated") + 
  scale_y_continuous(limits = c(-30,45)) +
  scale_x_continuous(limits = c(-30,45)) +
  #facet_wrap( ~ format(x = as.Date(Date))) +
  #facet_wrap( ~ format(as.Date(Date), "%Y"))
  facet_wrap(~Year)
print(Adraskan_TminPlot_CalBC)

ggsave("temp_data/Adraskan_TminPlot_CalBC.png",width = 20,height = 10, units = "cm",dpi = 600)

Adraskan_TmaxPlot_CalBC <- ggplot(data = Adraskan_BC_Cal, aes(x = Tmax_Obs, y = QDM_Tmax)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, linetype = "dashed") +
  xlab("Tmin Observed") + 
  ylab("Tmin Simulated") + 
  scale_y_continuous(limits = c(-30,45)) +
  scale_x_continuous(limits = c(-30,45)) +
  #facet_wrap( ~ format(x = as.Date(Date))) +
  #facet_wrap( ~ format(as.Date(Date), "%Y"))
  facet_wrap(~Year)
print(Adraskan_TmaxPlot_CalBC)

ggsave("temp_data/Adraskan_TmaxPlot_CalBC.png",width = 20,height = 10, units = "cm",dpi = 600)

# Plotting Bias Corrected Historic Period
Adraskan_TminPlot_HisBC <- Adraskan_BC_His %>%
  mutate(Year = lubridate::year(Date)) %>% 
  ggplot(aes(x = Tmin_Sim, y = QDM_Tmin_His)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, linetype = "dashed") +
  xlab("Tmin Simulated") + 
  ylab("Tmin Bias Corrected") + 
  scale_y_continuous(limits = c(-30,45)) +
  scale_x_continuous(limits = c(-30,45)) +
  #facet_wrap( ~ format(x = as.Date(Date))) +
  #facet_wrap( ~ format(as.Date(Date), "%Y"))
  facet_wrap(~Year)
print(Adraskan_TminPlot_HisBC)

ggsave("temp_data/Adraskan_TminPlot_HisBC.png",width = 20,height = 10, units = "cm",dpi = 600)

Adraskan_TmaxPlot_HisBC <- Adraskan_BC_His %>%
  mutate(Year = lubridate::year(Date)) %>% 
  ggplot(aes(x = Tmax_Sim, y = QDM_Tmax_His)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, linetype = "dashed") +
  xlab("Tmin Observed") + 
  ylab("Tmin Simulated") + 
  scale_y_continuous(limits = c(-30,45)) +
  scale_x_continuous(limits = c(-30,45)) +
  #facet_wrap( ~ format(x = as.Date(Date))) +
  #facet_wrap( ~ format(as.Date(Date), "%Y"))
  facet_wrap(~Year)
print(Adraskan_TmaxPlot_HisBC)

ggsave("temp_data/Adraskan_TmaxPlot_HisBC.png",width = 20,height = 10, units = "cm",dpi = 600)

Adraskan_BC_Cal$Date <- as.Date(Adraskan_BC_Cal$Date)

Adraskan_BC_Cal <- Adraskan_BC_Cal[!is.na(Adraskan_BC_Cal$Date), ]

#Tmin
Station_PlotBC_Tmin <- ggplot(Adraskan_BC_Cal, aes(x = Date, y = Tmin_Obs, group = 1)) + 
  geom_line(aes(colour = "Observed"),lwd=0.3) + 
  xlab("Time series of temperature data") + 
  ylab("Daily minimum temperature (°C)") + 
  #scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 4*700)]) +
  geom_line(data = Adraskan_BC_Cal, aes(x = Date, y = Tmin_Sim, colour = "Simulated"),lwd=0.3) +
  geom_line(data = Adraskan_BC_Cal, aes(x = Date, y = QDM_Tmin, colour = "Bias Corrected"),lwd=0.3) + 
  #scale_y_continuous(limits = c(-30,45)) + 
  expand_limits(y = c(min(Adraskan_BC_Cal$Tmin_Obs, na.rm = TRUE), max(Adraskan_BC_Cal$Tmin_Obs, na.rm = TRUE))) +
  #facet_wrap(~ format(Date, "%Y"), scales = "free_x") +
  scale_x_date(date_labels = "%b") +
  scale_color_manual(name = "Temperature Data\nTypes", values = c("Observed" = "darkblue", "Simulated" = "red", "Bias Corrected" = "orange"))
print(Station_PlotBC_Tmin)
ggsave("temp_data/Adraskan_Tmin_BC_Series.png",width = 24,height = 13, units = "cm",dpi = 600)

#Tmax

Station_PlotBC_Tmax <- ggplot(Adraskan_BC_Cal, aes(x = Date, y = Tmax_Obs, group = 1)) + 
  geom_line(aes(colour = "Observed"),lwd=0.3) + 
  xlab("Time series of temperature data") + 
  ylab("Daily maximum temperature (°C)") + 
  #scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 4*700)]) +
  geom_line(data = Adraskan_BC_Cal, aes(x = Date, y = Tmax_Sim, colour = "Simulated"),lwd=0.3) +
  geom_line(data = Adraskan_BC_Cal, aes(x = Date, y = QDM_Tmax, colour = "Bias Corrected"),lwd=0.3) + 
  #scale_y_continuous(limits = c(-30,45)) + 
  expand_limits(y = c(min(Adraskan_BC_Cal$Tmax_Obs), max(Adraskan_BC_Cal$Tmax_Obs))) +
  #facet_wrap(~ format(Date, "%Y"), scales = "free_x")+
  scale_x_date(date_labels = "%b") +
  scale_color_manual(name = "Temperature Data\nTypes", values = c("Observed" = "darkblue", "Simulated" = "red", "Bias Corrected" = "orange"))


ggsave("temp_data/Adraskan_Tmax_BC_Series.png",width = 24,height = 13, units = "cm",dpi = 600)

#Merging the two temperature series (Observed with historic bias corrected)

#1 Cleaning recent (Obs)
#Choosing the wanted columns
Recent_QDM <- Adraskan_Obs_Sim[,c("Date", "Tmin_Obs", "Tmax_Obs")] 

#Renaming the column names
colnames(Recent_QDM)[2] <- "Tmin"
colnames(Recent_QDM)[3] <- "Tmax"

#2 Cleaning historic (BC)
#Choosing the wanted columns
Historic_QDM <- Adraskan_BC_His[,c("Date", "QDM_Tmin_His", "QDM_Tmax_His")]

#Renaming the column names
colnames(Historic_QDM)[2] <- "Tmin"
colnames(Historic_QDM)[3] <- "Tmax"

Recent_QDM$Date <- as.character(Recent_QDM$Date)

station_QDM_1980_2020 <- rbind(Historic_QDM, Recent_QDM)

station_QDM_1980_2020 <- station_QDM_1980_2020 %>% 
  arrange(Date)

#Insert Year, Month and Day column 
station_QDM_1980_2020$Date <- as.Date(station_QDM_1980_2020$Date)

# extract year, month and day information into separate columns
station_QDM_1980_2020$Year <- format(station_QDM_1980_2020$Date, '%Y')
station_QDM_1980_2020$Month <- format(station_QDM_1980_2020$Date, '%m')
station_QDM_1980_2020$Day <- format(station_QDM_1980_2020$Date, '%d')

#Rearrange the columns 
station_QDM_1980_2020 <- station_QDM_1980_2020[,c("Date", "Year", "Month", "Day", "Tmin", "Tmax")]

# Bias corrected data for Chahardara
write.csv(station_QDM_1980_2020, file = paste0("temp_data/BC_1980_2020_QDM.csv"), row.names = FALSE)


### Historic temperature scenarios###
### Historic temperature scenarios###
### Historic temperature scenarios###


Adraskan_cal <- read.csv("D:/Phenology/temp_data/BC_1980_2020_QDM.csv")

# Chose 1980 to 2020 because baseline year causes issue later on if 23 years
Adraskan_cal <- Adraskan_cal %>%
  filter(Year >=1980 & Year <=2020) %>%
  select(-Date)


scenario_1980<-temperature_scenario_from_records(weather=Adraskan_cal,
                                                 year=1980)

temps_1980 <- temperature_generation(weather = Adraskan_cal,
                                     years = c(1980,2020),
                                     sim_years = c(2001,2100),
                                     temperature_scenario = scenario_1980)



scenario_2000<-temperature_scenario_from_records(weather=Adraskan_cal,year=2000) # Because it is also the median year of observed temp data

relative_scenario<-temperature_scenario_baseline_adjustment(
  baseline=scenario_2000,
  temperature_scenario = scenario_1980)

temps_1980<-temperature_generation(weather=Adraskan_cal, years=c(1980,2020),
                                   sim_years=c(2001,2100),
                                   temperature_scenario = relative_scenario)


all_past_scenarios<-temperature_scenario_from_records(
  weather=Adraskan_cal,
  year=c(1980,1990,2000,2010,2020))

adjusted_scenarios<-temperature_scenario_baseline_adjustment(
  baseline=scenario_2000,
  temperature_scenario = all_past_scenarios)

all_past_scenario_temps<-temperature_generation(
  weather=Adraskan_cal,
  years=c(1980,2020),
  sim_years=c(2001,2100),
  temperature_scenario = adjusted_scenarios)

# simulated temperature data 
save_temperature_scenarios(all_past_scenario_temps, "temp_data/historic_scen", "Chahardara_hist_scenarios")


# Chill quantification

frost_model <- function(x)
  step_model(x, data.frame(
    lower = c(-1000, 0),
    upper = c(0, 1000),
    weight = c(1, 0)))

models <- list(Chill_Portions = Dynamic_Model,
               Heat_GDH = GDH,
               Frost_H = frost_model)


# computed chill for all simulated historic temp data
chill_hist_scenario_list<-tempResponse_daily_list(all_past_scenario_temps,
                                                  latitude=36.7,
                                                  Start_JDay = 305,
                                                  End_JDay = 59,
                                                  models = models)

# before saving remove all incomplete winters from the record
chill_hist_scenario_list <- lapply(chill_hist_scenario_list,
                                   function(x) x %>%
                                     filter(Perc_complete == 100))

save_temperature_scenarios(chill_hist_scenario_list, "temp_data/historic_scen/chill_historic", "Chahardara_hist_chill")


# Compute actual observed chill for comparison and plotting
scenarios<-names(chill_hist_scenario_list)[1:5]

all_scenarios<-chill_hist_scenario_list[[scenarios[1]]]
all_scenarios[,"scenario"]<-as.numeric(scenarios[1])

for (sc in scenarios[2:5])
  all_scenarios<-rbind(all_scenarios,
                       cbind(chill_hist_scenario_list[[sc]],scenario=as.numeric(sc)))

all_scenarios<-all_scenarios[which(all_scenarios$Perc_complete==100),]

# Let's compute the actual 'observed' chill for comparison
actual_chill<-tempResponse_daily_list(Adraskan_cal,latitude=36.7,
                                      Start_JDay = 305,
                                      End_JDay = 59,
                                      models = models)[[1]]
actual_chill<-actual_chill[which(actual_chill$Perc_complete==100),]

Adraskan_all_His_scenarios <- ggplot(data=all_scenarios,aes(scenario,Chill_Portions,
                                                            fill=factor(scenario))) +
  geom_violin() +
  ylab("Chill accumulation (Chill Portions)") +
  xlab("Scenario year") +
  theme_bw(base_size=15) +
  ylim(c(0,90)) +
  geom_point(data=actual_chill,
             aes(End_year,Chill_Portions,fill="blue"),
             col="blue",show.legend = FALSE) +
  scale_fill_discrete(name="Scenario",
                      breaks = unique(all_scenarios$scenario)) 
ggsave("temp_data/historic_scen/chill_historic/Chahardara_all_His_scenarios.png", width = 18, height = 12, units = "cm", dpi = 600)

write.csv(actual_chill,"temp_data/historic_scen/chill_historic/Chahardara_observed_chill_305_59.csv", row.names = FALSE)

# Increase in temperature plots
temperature_means<-data.frame(Year=min(Adraskan_cal$Year):max(Adraskan_cal$Year),
                              Tmin=aggregate(Adraskan_cal$Tmin,FUN="mean",
                                             by=list(Adraskan_cal$Year))[,2],
                              Tmax=aggregate(Adraskan_cal$Tmax,FUN="mean",
                                             by=list(Adraskan_cal$Year))[,2])
temperature_means[,"runn_mean_Tmin"]<-runn_mean(temperature_means$Tmin,15)
temperature_means[,"runn_mean_Tmax"]<-runn_mean(temperature_means$Tmax,15)

Tmin_regression<-lm(Tmin~Year, temperature_means)
temperature_means[,"regression_Tmin"]<-Tmin_regression$coefficients[1]+
  Tmin_regression$coefficients[2]*temperature_means$Year

Tmax_regression<-lm(Tmax~Year, temperature_means)
temperature_means[,"regression_Tmax"]<-Tmax_regression$coefficients[1]+
  Tmax_regression$coefficients[2]*temperature_means$Year


Adraskan_Mean_Temp_Tmin <- ggplot(temperature_means,aes(Year, Tmin)) + 
  geom_point() + 
  geom_line(data=temperature_means,aes(Year, runn_mean_Tmin),lwd=2,col="blue") + 
  geom_line(data=temperature_means,aes(Year, regression_Tmin),lwd=2,col="red") +
  theme_bw(base_size=15) +
  ylab("Mean monthly minimum temperature (°C)")
ggsave("temp_data/historic_scen/chill_historic/Chahardara_Mean_Temp_Tmin.png", width = 18, height = 12, units = "cm", dpi = 600)


Adraskan_Mean_Temp_Tmax <- ggplot(temperature_means,aes(Year, Tmax)) + 
  geom_point() + 
  geom_line(data=temperature_means,aes(Year, runn_mean_Tmax),lwd=2,col="blue") + 
  geom_line(data=temperature_means,aes(Year, regression_Tmax),lwd=2,col="red") +
  theme_bw(base_size=15) +
  ylab("Mean monthly maximum temperature (°C)")
ggsave("temp_data/historic_scen/chill_historic/Chahardara_Mean_Temp_Tmax.png", width = 18, height = 12, units = "cm", dpi = 600)



# Future scenarios download and temperature generation


### Future temperature scenarios using SSP###
### Future temperature scenarios using SSP###
### Future temperature scenarios using SSP###

require(chillR)
require(ggplot2)
require(LarsChill)
require(tidyverse)
require(lubridate)
require(magrittr)
require(dplyr)
require(kableExtra)
require(Metrics)
require(zoo)
require(latticeExtra)
require(stringr)
library(reshape2)
library(ggpmisc)
library(patchwork)
require(epwshiftr)
library(purrr)
#devtools::install_github("larscaspersen/addition_chillR")

# Download future climate data
location = c(68.82, 36.70)  # Kunduz location (longitude, latitude)

# I define a bounding box around Kunduz (+/- 1 degree around the coordinates)
area = c(37.70, 67.82, 35.70, 69.82)  # max_latitude, min_longitude, min_latitude, max_longitude

download_cmip6_ecmwfr(
  scenarios = c("ssp126", "ssp245", "ssp370", "ssp585"), 
  area = c(37.70, 67.82, 35.70, 69.82),
  user = '183634',
  key = 'dab625dc-59fe-495a-831b-6889ab9bf7a5',
  model = 'default',
  frequency = 'monthly',
  variable = c('Tmin', 'Tmax'),
  year_start = 2015,
  year_end = 2100)


# Generating change scenario
download_baseline_cmip6_ecmwfr(
  area = c(37.70, 67.82, 35.70, 69.82),
  user = '183634',
  key = 'dab625dc-59fe-495a-831b-6889ab9bf7a5',
  model = 'match_downloaded', # here, the function will automatically look into the download folder, determine what models were used and download all respective baseline data.
  frequency = 'monthly',
  variable = c('Tmin', 'Tmax'),
  year_start = 1986, 
  year_end = 2014, 
  month = 1:12)

# the above data is still grided, we will extract the data of point for Chahardara-Kunduz
station <- data.frame(
  station_name = c("Chahardara"),
  longitude = c(68.82),
  latitude = c(36.70))

extracted <- extract_cmip6_data(stations = station)

#save_temperature_scenarios(extracted, 'temp_cmip6', prefix = "cmip6")

head(extracted$`ssp126_ACCESS-CM2`)

# There are uncommon models so i will try to make all available models for all scenarios
models_remove <-
  c("ssp126_BCC-CSM2-MR",
    "ssp245_BCC-CSM2-MR",
    "ssp585_BCC-CSM2-MR",
    "ssp126_CESM2",
    "ssp245_CESM2",
    "ssp585_CESM2",
    "historical_CIESM",
    "ssp585_CIESM",
    "historical_EC-Earth3-CC",
    "ssp245_EC-Earth3-CC",
    "ssp585_EC-Earth3-CC",
    "ssp126_UKESM1-0-LL",
    "ssp245_UKESM1-0-LL",
    "ssp585_UKESM1-0-LL")

extracted <- names(extracted) %>%
  setdiff(models_remove) %>%    # Remove models in models_remove from extracted
  {extracted[.]}                # Use the filtered names to subset the original extracted list


# Generate change scenario
# Since we have stored the baseline data in the same folder, we can now use a very compact call to generate change scenarios for all the climate projections in the folder.
change_scenarios <- chillR::gen_rel_change_scenario(extracted)

# There error message I was getting is:
# Error in purrr::map2(downloaded_list, weather_list, function(x, y) { : 
#     argument "weather_list" is missing, with no default
#   
#   
#   Called from: vctrs_vec_compat(.y, .purrr_user_env)
#   Browse[1]> 

# what I did was:
# I used the official function:
#   Becausw we are using a modified version of chillR, I revert to the original one provided by chillR. 
# With this, I can either reinstall the package or use the function directly from it. 
# And I used directly using chillR::gen_rel_change_scenario()

head(change_scenarios)

# Save the long dataframe
write.csv(change_scenarios, "temp_data/all_change_scenarios.csv", row.names = FALSE)

# convert data to relevant form that is used by weather generator (convert back into list)
scen_list <- convert_scen_information(change_scenarios)

# we can use same function to convert list back to dataframe 
scen_frame <- convert_scen_information(scen_list)

# Baseline adjustment: use observed kunduz data to inform weather generator of local weather to be simulated for future
Chahardara_temps<-read_tab("temp_data/BC_1980_2020_QDM.csv")

# test for one GCM
# temperature_generation(Chahardara_temps, years = c(1980, 2020), sim_years = c(2001, 2100), scen_list$Chahardara$ssp126$`ACCESS-CM2`)

# Here luckily the median year of our observed data is 2000 which matches the reference year of future projections.
# Therefore, we do not need further steps to adjusting the baseline of the observed data. So we go to temperature generation directly:

# Temperature generation
# Since i am direclty using scen_list I need to adjust the code

# Loop over all SSP scenarios, GCMs, and time points
for (ssp in names(scen_list$Chahardara)) {
  for (gcm in names(scen_list$Chahardara[[ssp]])) {
    for (time_point in names(scen_list$Chahardara[[ssp]][[gcm]])) {
      
      # Extract the specific scenario
      specific_scenario <- scen_list$Chahardara[[ssp]][[gcm]][[time_point]]
      
      # Print structure of specific_scenario to check if it contains 'data'
      print(paste("Checking scenario:", ssp, gcm, time_point))
      str(specific_scenario)
      
      # Check if 'data' is present
      if ("data" %in% names(specific_scenario)) {
        
        # Convert the 'data' element from tibble to data.frame (if necessary)
        specific_scenario$data <- as.data.frame(specific_scenario$data)
        
        # Wrap the specific_scenario object in a list before passing it
        scenario_list <- list(specific_scenario)
        
        # Generate future temperatures by passing the full scenario object wrapped in a list
        temps <- temperature_generation(
          Chahardara_temps, 
          years = c(1980, 2020), 
          sim_years = c(2001, 2100), 
          temperature_scenario = scenario_list,  # Pass the full scenario object wrapped in a list
          temperature_check_args = list(scenario_check_thresholds = c(-5, 15))
        )
        
        # Save the output in a single folder called 'future_climate' with unique filenames
        save_temperature_scenarios(
          temps,
          "temp_data/future_climate",  # Single folder for all outputs
          paste0("Chahardara_futuretemps_", ssp, "_", time_point, "_", gcm)  # Unique file prefix
        )
        
      } else {
        warning(paste("Scenario for", ssp, gcm, time_point, "is missing 'data' element. Skipping."))
      }
    }
  }
}


# Remove _1_ at the end of each file

# Set the directory where your files are saved
file_dir <- "temp_data/future_climate"

# List all files in the directory
files <- list.files(file_dir, pattern = "_1_", full.names = TRUE)

# Loop over each file and rename it to remove '_1_'
for (file in files) {
  # Create the new filename by removing '_1_'
  new_file <- gsub("_1_", "", file)
  
  # Rename the file
  file.rename(file, new_file)
}

# Confirm the files have been renamed
list.files(file_dir)


# Chill quantification

frost_model <- function(x)
  step_model(x, data.frame(
    lower = c(-1000, 0),
    upper = c(0, 1000),
    weight = c(1, 0)))

models <- list(Chill_Portions = Dynamic_Model,
               Heat_GDH = GDH,
               Frost_H = frost_model)





#read future temperature for different scenarios
SSPs<-c("ssp126","ssp245", "ssp585")
Times <- c(2050, 2085)
Latitude <- 36.70

for (SSP in SSPs) {
  for (Time in Times) {
    
    Temps <- load_temperature_scenarios("temp_data/future_climate", 
                                               paste0("Chahardara_futuretemps_", SSP, "_", Time)) 

        # Assigning names to the data frames
    names(Temps) <-
      c("ACCESS-CM2","AWI-CM-1-1-MR","CMCC-ESM2","CNRM-CM6-1-HR","CNRM-ESM2-1", "EC-Earth3-Veg-LR", "FGOALS-g3",
        "FIO-ESM-2-0", "GFDL-ESM4", "INM-CM4-8", "INM-CM5-0", "MIROC6", "MPI-ESM1-2-LR", "MRI-ESM2-0", "NESM3")
    
    chill <- tempResponse_daily_list(
      Temps,
      latitude = Latitude,
      Start_JDay = 305,
      End_JDay = 59,
      models = models)
    
    save_temperature_scenarios(
      chill,
      "temp_data/future_climate/chill_future",
      paste0("Chahardara_futuretemps_", SSP, "_", Time))
  }
}
    

### Load historic scenarios back if needed for plotting both the scenarios and historic chill,
###however we will be plotting lateron

chill_past_scenarios <- load_temperature_scenarios(
  "temp_data/historic_scen/chill_historic",
  "Chahardara_hist_chill")
chill_observed <- load_temperature_scenarios(
  "temp_data/historic_scen/chill_historic",
  "Chahardara_observed_chill_305_59")

chills <- make_climate_scenario(
  chill_past_scenarios,
  caption = "Historic",
  historic_data = chill_observed,
  time_series = TRUE)


###We now load this again, make climate scenario the same way we did for
###the historic data, and add them to our chills list so that we can easily plot them

GCM <- c("ACCESS-CM2","AWI-CM-1-1-MR","CMCC-ESM2","CNRM-CM6-1-HR","CNRM-ESM2-1", "EC-Earth3-Veg-LR", "FGOALS-g3",
         "FIO-ESM-2-0", "GFDL-ESM4", "INM-CM4-8", "INM-CM5-0", "MIROC6", "MPI-ESM1-2-LR", "MRI-ESM2-0", "NESM3")

for (SSP in SSPs) {
  for (Time in Times)
  {
    chill <- load_temperature_scenarios(
    "temp_data/future_climate/chill_future",
    paste0("Chahardara_futuretemps_", SSP, "_", Time))
    
    if (SSP == "ssp126") SSPcaption <- "SSP126"
    if (SSP == "ssp245") SSPcaption <- "SSP245"
    if (SSP == "ssp585") SSPcaption <- "SSP585"
    if (Time == "2050") Time_caption <- "2050"
    if (Time == "2085") Time_caption <- "2085"
    chills <- make_climate_scenario(chill,
                                    caption = c(SSPcaption, Time_caption),
                                    add_to = chills)
  }
}


# We'll first process the past scenarios (element 1 of the chills list).
# Within the data element, we have a list of multiple data.frames for
# the various past scenarios.
# Using a 'for' loop, we cycle through all these data.frames.

for (nam in names(chills[[1]]$data))
{
  # Extract the data frame.
  ch <- chills[[1]]$data[[nam]]
  # Add columns for the new information we have to add and fill them.
  ch[, "GCM"] <- "none"
  ch[, "SSP"] <- "none"
  ch[, "Year"] <- as.numeric(nam)
  
  # Now check if this is the first time we've gone through this loop.
  # If this is the first time, the ch data.frame becomes the output
  # object (past_simulated).
  # If it is not the first time ('else'), we add the current data.frame
  # to the 'past_simulated' object
  if (nam == names(chills[[1]]$data)[1])
    past_simulated <- ch
  else
    past_simulated <- rbind(past_simulated, ch)
}

# We add another column called 'Scenario' and label all rows as 'Historic'
past_simulated["Scenario"] <- "Historic"

# kable(past_simulated[1:5,])  %>%
#   kable_styling("striped", position = "left",font_size = 8)


# We'll want to add the historic observation too, so let's simplify the
# pointer to this information for easier use later

past_observed <- chills[[1]][["historic_data"]]

# kable(past_observed[1:5,])  %>%
#   kable_styling("striped", position = "left",font_size = 8)


# Extract future data
for (i in 2:length(chills))
  for (nam in names(chills[[i]]$data))
  {
    ch <- chills[[i]]$data[[nam]]
    ch[, "GCM"] <- nam
    ch[, "SSP"] <- chills[[i]]$caption[1]
    ch[, "Year"] <- chills[[i]]$caption[2]
    if (i == 2 & nam == names(chills[[i]]$data)[1])
      future_data <- ch
    else
      future_data <- rbind(future_data, ch)
  }


# kable(future_data[1:5,])  %>%
#   kable_styling("striped", position = "left",font_size = 8)




### Plotting ####
### Plotting ####
### Plotting ####

# First read the plot_function_scenario_gg function:

require(ggplot2)
require(ggpmisc)
require(patchwork)

# I would like to annotate the chill and heat values on these plots for all varieties
#read the save parameters
fname <- 'data/parameter_combined_fitting_round_3.csv'

par_df3 <- read.csv(fname)

source('util/convenience_functions.R')

par_df3 <- reformat_combined_fit_parameters(par_df3)
yc_data <- par_df3 %>% 
  select(cultivar, repetition, yc)

# Edited function for chill portions:
plot_all_scenarios_gg <- function(past_observed,
                                  past_simulated,
                                  future_data,
                                  metric,
                                  axis_label,
                                  yc_data) {
  # Calculate summary statistics for yc
  yc_summary <- data.frame(
    ymin = quantile(yc_data$yc, 0.25, na.rm = TRUE),  # Q1
    ymax = quantile(yc_data$yc, 0.75, na.rm = TRUE),  # Q3
    ymid = median(yc_data$yc, na.rm = TRUE)           # Median
  )
  
  rng <- range(past_observed[[metric]],
               past_simulated[[metric]],
               future_data[[metric]],
               yc_summary$ymin, yc_summary$ymax)
  
  # Calculate x-axis range for better positioning 
  x_min <- min(as.numeric(past_simulated$Year))
  x_max <- max(as.numeric(past_simulated$Year))
  
  past_plot <- ggplot() +

    # Add the horizontal box plot using geom_crossbar (all the chill distribution of all cultivars for all repetitions)
    geom_crossbar(data = yc_summary,
                  aes(x = mean(c(x_min, x_max)), ymin = ymin, ymax = ymax, y = ymid),
                  fill = "darkgreen", alpha = 0.3, 
                  width = (x_max - x_min) *1.5, # to slightly extend the green strip I multiplied the width with 1.5
                  color = NA) +
  
    # Annotate geom_crossbar with text
    annotate("text", 
             x = mean(c(x_min, x_max)),  # Centered on the crossbar's x-position
             y = yc_summary$ymax - 6,    # Position slightly above ymax
             label = "Almond Cultivars'\nChilling Requirement", 
             color = "black", size = 2.5, fontface = 'bold') +
    
    geom_boxplot(data = past_simulated,
                 aes_string("as.numeric(Year)", metric, group = "Year"),
                 fill = "skyblue") +
    
    # ggplot was showing only 3 year so I show other too
    scale_x_continuous(
      breaks = c(1980, 1990, 2000, 2010, 2020),  # Ensure all years are shown
      labels = c("1980", "1990", "2000", "2010", "2020")  # Customize labels if needed
    ) +
    scale_y_continuous(limits = c(0, round(rng[2] + rng[2] / 10))) +
    labs(x = "Year", y = axis_label) +
    facet_grid(~Scenario) +
    theme_bw(base_size = 14) +
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    # Add observed data points
    geom_point(data = past_observed,
               aes_string("End_year", metric),
               col = "blue") 
    

  # Create future plots for 2050 and 2085
  future_plot_list <- list()
  for (y in c(2050, 2085)) {
    future_plot_list[[which(y == c(2050, 2085))]] <-
      ggplot(data = future_data[which(future_data$Year == y), ]) +
      
      # Add the horizontal box plot for the yc range
      geom_crossbar(data = yc_summary,
                    aes(x = 0, ymin = ymin, ymax = ymax, y = ymid),
                    fill = "darkgreen", alpha = 0.3, width = Inf, color = NA) +
      
      geom_boxplot(aes_string("GCM", metric, fill = "GCM")) +
      facet_grid(cols = vars(SSP)) +
      scale_x_discrete(labels = NULL, expand = expansion(add = 1)) +
      scale_y_continuous(limits = c(0, round(round(1.1 * rng[2])))) +
      geom_text_npc(aes(npcx = "center", npcy = "top", label = Year), size = 5) +
      theme_bw(base_size = 15) +
      theme(axis.ticks.y = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom",
            legend.margin = margin(0, 0, 0, 0, "cm"),
            legend.background = element_rect(),
            strip.background = element_blank(),
            strip.text = element_text(face = "bold"),
            legend.box.spacing = unit(0, "cm"),
            plot.subtitle = element_text(hjust = 0.5, vjust = -1,
                                         size = 15 * 1.05, face = "bold"))
  }
  
  plot <- (past_plot + future_plot_list +
             plot_layout(guides = "collect",
                         widths = c(1, rep(1.8, length(future_plot_list))))) &
    theme(legend.position = "bottom",
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10),
          axis.title.x = element_blank())
  
  plot
}





# Chill plot
plot_all_scenarios_gg(
  past_observed = past_observed,
  past_simulated = past_simulated,
  future_data = future_data,
  metric = "Chill_Portions",
  axis_label = "Chill (in Chill Portions)",
  yc_data = yc_data)

ggsave(
  "temp_data/future_climate/chill_future/chill_plot/Chahardara_future_CP_cultivars.png",
  width = 18,
  height = 12,
  units = "cm",
  dpi = 600)


# Rename in past_observed
#past_observed <- past_observed %>%
  rename(Heat_GDH = GDH)

# Rename in past_simulated
#past_simulated <- past_simulated %>%
  rename(Heat_GDH = GDH)

  zc_data <- par_df3 %>% 
    select(cultivar, repetition, zc)
  
# Edited function for chill portions:
plot_all_scenarios_gg <- function(past_observed,
                                  past_simulated,
                                  future_data,
                                  metric,
                                  axis_label,
                                  zc_data) {
  # Calculate summary statistics for zc
  zc_summary <- data.frame(
    ymin = quantile(zc_data$zc, 0.25, na.rm = TRUE),  # Q1
    ymax = quantile(zc_data$zc, 0.75, na.rm = TRUE),  # Q3
    ymid = median(zc_data$zc, na.rm = TRUE)           # Median
  )
  
  rng <- range(past_observed[[metric]],
               past_simulated[[metric]],
               future_data[[metric]],
               zc_summary$ymin, zc_summary$ymax)
  
  # Calculate x-axis range for better positioning 
  x_min <- min(as.numeric(past_simulated$Year))
  x_max <- max(as.numeric(past_simulated$Year))
  
  past_plot <- ggplot() +
    
    # Add the horizontal box plot using geom_crossbar (all the chill distribution of all cultivars for all repetitions)
    geom_crossbar(data = zc_summary,
                  aes(x = mean(c(x_min, x_max)), ymin = ymin, ymax = ymax, y = ymid),
                  fill = "darkorange2", alpha = 0.3, 
                  width = (x_max - x_min) *1.5, # to slightly extend the green strip I multiplied the width with 1.5
                  color = NA) +
    
    # Annotate geom_crossbar with text
    annotate("text", 
             x = mean(c(x_min, x_max)),  # Centered on the crossbar's x-position
             y = zc_summary$ymax - 6,    # Position slightly above ymax
             label = "Almond Cultivars'\nChilling Requirement", 
             color = "black", size = 2.5, fontface = 'bold') +
    
    geom_boxplot(data = past_simulated,
                 aes_string("as.numeric(Year)", metric, group = "Year"),
                 fill = "skyblue") +
    
    # ggplot was showing only 3 year so I show other too
    scale_x_continuous(
      breaks = c(1980, 1990, 2000, 2010, 2020),  # Ensure all years are shown
      labels = c("1980", "1990", "2000", "2010", "2020")  # Customize labels if needed
    ) +
    scale_y_continuous(limits = c(0, round(rng[2] + rng[2] / 10))) +
    labs(x = "Year", y = axis_label) +
    facet_grid(~Scenario) +
    theme_bw(base_size = 14) +
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    # Add observed data points
    geom_point(data = past_observed,
               aes_string("End_year", metric),
               col = "blue") 
  
  
  # Create future plots for 2050 and 2085
  future_plot_list <- list()
  for (y in c(2050, 2085)) {
    future_plot_list[[which(y == c(2050, 2085))]] <-
      ggplot(data = future_data[which(future_data$Year == y), ]) +
      
      # Add the horizontal box plot for the yc range
      geom_crossbar(data = zc_summary,
                    aes(x = 0, ymin = ymin, ymax = ymax, y = ymid),
                    fill = "darkorange2", alpha = 0.3, width = Inf, color = NA) +
      
      geom_boxplot(aes_string("GCM", metric, fill = "GCM")) +
      facet_grid(cols = vars(SSP)) +
      scale_x_discrete(labels = NULL, expand = expansion(add = 1)) +
      scale_y_continuous(limits = c(0, round(round(1.1 * rng[2])))) +
      geom_text_npc(aes(npcx = "center", npcy = "top", label = Year), size = 5) +
      theme_bw(base_size = 15) +
      theme(axis.ticks.y = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom",
            legend.margin = margin(0, 0, 0, 0, "cm"),
            legend.background = element_rect(),
            strip.background = element_blank(),
            strip.text = element_text(face = "bold"),
            legend.box.spacing = unit(0, "cm"),
            plot.subtitle = element_text(hjust = 0.5, vjust = -1,
                                         size = 15 * 1.05, face = "bold"))
  }
  
  plot <- (past_plot + future_plot_list +
             plot_layout(guides = "collect",
                         widths = c(1, rep(1.8, length(future_plot_list))))) &
    theme(legend.position = "bottom",
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10),
          axis.title.x = element_blank())
  
  plot
}


# GDH plot
plot_all_scenarios_gg(
  past_observed = past_observed,
  past_simulated = past_simulated,
  future_data = future_data,
  metric = "Heat_GDH",
  axis_label = "Heat (in Growing Degree Hours)",
  zc_data = zc_data)

ggsave(
  "temp_data/future_climate/chill_future/chill_plot/Chahardara_future_GDH_cultivar.png",
  width = 18,
  height = 12,
  units = "cm",
  dpi = 600)


# Frost plot
plot_all_scenarios_gg(
  past_observed = past_observed,
  past_simulated = past_simulated,
  future_data = future_data,
  metric = "Frost_H",
  axis_label = "Frost duration (in hours)")

ggsave(
  "temp_data/future_climate/chill_future/chill_plot/Chahardara_future_Frost.png",
  width = 18,
  height = 12,
  units = "cm",
  dpi = 600)






