
################################################################
# Merging mortality and weather data 
################################################################
#libraries
library(tidyverse)
library(lubridate)
#load data and adjusting length
data_m <- read_rds("00.data//data_m.rds") %>% 
  filter(date >= ymd("2000-01-01") & date <= ymd("2019-12-31"))
data_HNE <- read_rds("00.data//HNE_data.rds")
data_rh <- read_rds("00.data/data_rh_capitals.rds") %>%
  filter(date >= ymd("2000-01-01") & date <= ymd("2019-12-31")) %>% 
  mutate(code_muni= str_sub(code_muni,end=6))
data_tmean_min <- read_rds("00.data//data_tmean_min.rds") %>% 
  filter(date_day >= ymd("2000-01-01") & date_day <= ymd("2019-12-31"))
data_climates <- read_rds("00.data//climates.rds")[,c(1,4,5)]
# data for meta-prediction
data_census <- read_rds("00.data//census_processed.rds") %>% select(!municipio) %>% 
  mutate(codmun6 = codmun6  %>% as.character())
data_coord <- read.csv("00.data//coords.csv") %>% select("CD_MUN", "lon", "lat") %>% 
  mutate(CD_MUN = CD_MUN %>% as.character() %>% str_sub(end = 6))
# checkpoint
length(data_m$date)
length(data_HNE$code)
length(data_tmean_min$code)
length(data_rh$date)
# Merging -----------------------------------------------------------------

data <- left_join(data_m,data_HNE, by = c("mun_name"="code", "date" = "date_night"))
data <- left_join(data, data_rh, by = c("mun_code"="code_muni", "date" = "date") )
data <- left_join(data, data_tmean_min, by = c("mun_name"="code", "date" = "date_day") )
data <- left_join(data, data_climates, by = c("mun_name"="Municipality") )
data <- left_join(data, data_census, by = c("mun_code"="codmun6"))
data <- left_join(data, data_coord, by = c("mun_code"="CD_MUN"))
data %>% summary
# saving 
data %>% write_rds("00.data//data_m_w.rds")
