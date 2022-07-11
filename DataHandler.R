# Data Retrieval 
# Camilo Bastidas
# 10/6/21
# - modified based on reproducible comments:07/07/22

# Load packages 
library(tidyverse)
library(lubridate)
library(ggpattern)
library(cowplot)
library(gridExtra)
library(viridis)
library(scales)
library(RColorBrewer)
library(remotes)
library(ggpattern)

rm(list = ls()) # remove everything

# Load urls.csv 
# This file contains the urls of all the files we need.
# It was created using an R client to interact with HydroSHare API
# The HydroSHare client is available on https://github.com/program--/HSClientR
# The file is included here to make sure this resource runs independtly.

# Code to create the file -  no need to re-run this part

#install.packages("remotes")
#remotes::install_github("program--/HSClientR")
#library(remotes)
#library(HSClientR)
#remotes::install_github("coolbutuseless/ggpattern")
#install.packages('ggpattern')

# Monthly data
# HS Resource: http://www.hydroshare.org/resource/16c2d60eb6c34d6b95e5d4dbbb4653ef
#MonthlyResID = "16c2d60eb6c34d6b95e5d4dbbb4653ef"
#MonthlyDataFiles <- hs_files(MonthlyResID)$results

# High temporal resolution data and additional information. 
# HS resource: http://www.hydroshare.org/resource/0b72cddfc51c45b188e0e6cd8927227e
#HighResID <- '0b72cddfc51c45b188e0e6cd8927227e'
#HighResFiles <- hs_files(HighResID, count = 200)$results

# WeatherData 
# HS Resource: http://www.hydroshare.org/resource/379d9e7037f04478a99d5aec22e841e6
#SupData_ID <- '379d9e7037f04478a99d5aec22e841e6'
#SupData_Files <- hs_files(SupData_ID)$results

#rbind(MonthlyDataFiles, HighResFiles, SupData_Files) %>%
#  write.csv("urls.csv")


# start erunning here
files <- read_csv("urls.csv") # this file has all the information about the data files we need

# Conversion factors and constants
gl = 3.78541 # gallons to liters
sqft_sqm = 0.092903 # ft2 to m2
cf_l = 28.3168 # cubic foot to liters
kc = 0.8 # crop coefficient for the water balance

# Monthly water use data for the last 2 years
files %>% # list of files
  filter(file_name == 'MonthlyWaterUseData.csv') %>% # filter the file with monthly water use data
  pull(url) %>% # select the url
  read_csv() %>%
  filter(City == 'Logan' & year(Date) %in% c(2017,2018) | City == 'Providence' & year(Date) %in% c(2018,2019)) -> MonthlyWUD # Monthly Water Use Data # last two years for each city
  

# Sites information
files %>% #list of files
  filter(file_name == 'sites.csv') %>%
  pull(url) %>%
  read_csv() -> sites

# Quality Controlled data
files %>%
  filter(str_detect(file_name, "qc_data.csv")) %>%
  arrange(file_name) %>%
  pull(url) -> qc_files

qc <- map2_df(map(qc_files, read_csv), 2:32,  ~ mutate(.x, Site = .y))

# Event Files
files %>% 
  filter(str_detect(file_name, "Events_site_FV_")) %>%
  arrange(file_name) %>%
  pull(url) -> ev_files

ev <- map2_df(map(ev_files, read_csv), 2:32,  ~ mutate(.x, Site = .y))


# WeatherData 
# ID of the weatehr stations used
# Evans Farm: 1138090
# USU Obs.: 1279257
files %>% 
  filter(str_detect(file_name, "1138090")) %>% # Evans Farm ID
  pull(url) %>%
  read_csv() %>%
  select(date_time, eto, precip_tb) %>% # eto and precipitationfrom tipping bucket (in) - see https://climate.usu.edu/index.php for details.
  rename(prc = precip_tb) %>%
  add_column(station = "EvansFarm") -> ef # Evans Farm data

# Evans Farm data
files %>% 
  filter(str_detect(file_name, "1279257")) %>%
  pull(url) %>%
  read_csv() %>%
  select(date_time, eto, precip) %>% # eto and precipitation (in)
  rename(prc = precip) %>% 
  add_column(station = "USUObs") -> uo #  USU Obs. data

full_join(ef, uo) %>% # weather data from both stations
  add_column(wk = isoweek(.$date_time)) %>% # ISO 8601 week of the year
  add_column(y = year(.$date_time)) %>% # year
  group_by(station, y, wk) %>% 
  mutate(wprec = sum(prc), weto = sum(eto), date_time = date_time) %>% # weekly raifall, etp and start day of the week
  filter(row_number()==1 ) %>% # select 1 value per week
  mutate(lndwneed = kc * weto - wprec) %>% # landscape water needs - in inches
  mutate(city = case_when(station == "EvansFarm" ~ "Providence", station == "USUObs" ~ "Logan")) %>% # City - to use each station data
  mutate(ftid = paste0(city,"_", wk,"_", y)) -> wd # Weather Data

# Complete days - this is a list of days that are complete (needed for daily estimations)
qc %>%
  add_column(dt = as.Date(.$Time)) %>%
  group_by(Site, dt) %>%
  mutate (n = n()) %>%
  filter(n > (86400/4) - (3600/4)) %>% # remove days that are not complete +- 1 hours
  summarise(tpu = sum(Pulses), n = mean(n)) %>%
  mutate(fd = paste0(Site, dt)) %>%
  group_by(Site) %>%
  mutate(nd = n()) -> dayp # total pulses per day and number of values


# Complete weeks - list of full weeks, as defined in the article. Needed for the weekly assessment
dayp %>%
  add_column(wk = isoweek(.$dt)) %>% # ISO 8601 week of the year
  add_column(y = year(.$dt)) %>%
  group_by(Site, y, wk) %>%
  mutate(ndays = n()) %>%
  filter(ndays > 6) %>%
  filter(row_number() == 1) %>%
  mutate(fws = paste0(Site,"_", wk,"_", y)) -> fws # full weeks per site


print("The data preparation is complete, now you can run DataAnalysis.R!")
