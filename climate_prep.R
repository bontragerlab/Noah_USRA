library(tidyverse)
library(googlesheets4)

data = read_sheet("https://docs.google.com/spreadsheets/d/1s9jBTTmM33xvjss3SlO7oLSl2mRYFlQZtCB3u5-OKxQ/edit?usp=sharing")


# Reformat ClimateNA data ----------------------------------------------

# Data is formatted with one row per site/year, one column per variable/month
reformatted_data = data %>%
  # Make data tall (one row per variable/site/month/year)
  gather(variable, value, -Year, -ID1, -Latitude, -Longitude, -Elevation) %>% 
  mutate(timescale = if_else(str_detect(variable, "_at|_sp|_wt|_sm"), "seasonal", 
                             if_else(str_detect(variable, "([1-9]|1[0-2])$" ), "monthly", "annual")),
         timescale = if_else(variable %in% c("DD_18", "DD18", "DD5"), "annual", timescale))

table(reformatted_data$variable, reformatted_data$timescale)

annual_data = reformatted_data %>% 
  filter(timescale == "annual")

# AHM  	annual heat-moisture index (MAT+10)/(MAP/1000))
# bFFP 	the day of the year on which FFP begins
# CMD 	Hargreaves climatic moisture deficit (mm)
# CMI 	Hogg’s climate moisture index (mm)
# DD<0 (or DD_0)  	degree-days below 0°C, chilling degree-days
# DD1040 	degree-days above 10°C and below 40°C 
# DD>5 (or DD5) 	degree-days above 5°C, growing degree-days
# DD<18 (or DD_18) 	degree-days below 18°C, heating degree-days
# DD>18 (or DD18) 	degree-days above 18°C, cooling degree-days
# eFFP 	the day of the year on which FFP ends
# EMT 	extreme minimum temperature over 30 years (°C)
# Eref 	Hargreaves reference evaporation (mm)
# EXT 	extreme maximum temperature over 30 years (°C)
# FFP 	frost-free period
# MAP 	annual heat-moisture index (MAT+10)/(MAP/1000))
# MAR 	mean annual solar radiation (MJ m‐2 d‐1)
# MAT  	mean annual temperature (°C)
# MCMT 	mean coldest month temperature (°C)
# MSP mean summer precip?
# MWMT 	mean warmest month temperature (°C)
# NFFD 	the number of frost-free days
# PAS 	precipitation as snow (mm). For individual years, it covers the period between August in the previous year and July in the current year
# RH	mean annual relative humidity (%)
# SHM summer heat-moisture index ((MWMT)/(MSP/1000))   
# TD  	temperature difference between MWMT and MCMT, or continentality (°C)

table(annual_data$variable)

annual_averages = annual_data %>% 
  filter(Year > 1980 & Year <= 2010) %>% 
  group_by(ID1, Latitude, Longitude, Elevation, variable) %>% 
  summarise(mean_1980_2010 = mean(value)) %>% 
  pivot_wider(names_from = variable, values_from = mean_1980_2010, id_cols = c(ID1, Latitude, Longitude, Elevation))

CMD = reformatted_data %>% 
  filter(variable %in% c("CMD_sp", "CMD_sm", "CMD_wt")) %>%  
  filter(Year > 1980 & Year <= 2010) %>% 
  group_by(ID1, Latitude, Longitude, Elevation, variable) %>% 
  summarise(mean_1980_2010 = mean(value)) %>% 
  pivot_wider(names_from = variable, values_from = mean_1980_2010, id_cols = c(ID1, Latitude, Longitude, Elevation))

variables = left_join(annual_averages, CMD)

write_csv(variables, "climate_data.csv")
