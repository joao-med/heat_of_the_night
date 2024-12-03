# Data  -------------------------------------------------------

# weather data and creating quantitative variable for Hot Nights #

#libraries
library(tidyverse)
library(raster)
library(terra)

# list of cities
capitals <- c('1100205','1200401','1302603','1400100','1501402','1600303','1721000',
              '2111300','2211001','2304400','2408102','2507507','2611606','2704302',
              '2800308','2927408','3106200','3205309','3304557','3550308','4106902',
              '4205407','4314902','5002704','5103403','5208707','5300108')

# Loop --------------------------------------------------------------------
# Creating a loop that process the weather data
# span 
list_file <- c(seq(1999,2009),"2010_2019")
b = 1999
for (b in list_file){
  print(b)
  grib <- rast(paste0("E://jpmgo//Desktop//data_CDS//datacds_",b,".grib"))
  # brasil shapes
  inshpfname <- "Shape_files/BR_Municipios_2022.shp"
  inshp <- vect(inshpfname)
  inshp$code <- as.character(inshp$CD_MUN)
  time_span <- b
  ## OBS
  # As it is a slow process it was made manually selecting specific capital by each turn
  filtered_mun <- inshp[inshp$code %in% str_sub(capitals), ] 
  filtered_mun$AREA_KM2
  filtered_mun <- filtered_mun 
  filtered_mun %>% plot
  capitals_filtered <- filtered_mun$NM_MUN
  capitals_filtered<- capitals_filtered
  
  for (i in capitals_filtered){
    print(paste("Processing",i))
    inshp_loop <- filtered_mun[filtered_mun$NM_MUN %in% str_sub(i), ] 
    subset_grib_final <- tibble()
    for (a in time_span){
      print(paste("year", a))
      # Define the start and end dates for the year you want to filter (e.g., 2010)
      # all data is in GMT
      start_date <- as.POSIXct(paste0(a,"-01-01 00:00:00"), tz ="GMT")
      end_date <- as.POSIXct(paste0(a,"-12-31 23:00:00"),tz ="GMT")
      # Extract the time dimension from the SpatRaster
      intimes = time(grib)
  
      # Identify the indices corresponding to the desired year
      indices <- which(intimes >= start_date & intimes <= end_date)
      
      # Subset the SpatRaster for the specified time range
      subset_grib <- subset(grib, subset = indices)
      # Extracting data
      ## names to columns and time
      cnames = paste0('temp',".",format(intimes, format = "%Y.%m.%d.%H"))
      ## Extracting
      print("extracting")
      subset_grib <- terra::extract(grib, inshp_loop, fun = mean,na.rm = T)
      names(subset_grib)[-1] <- cnames
      subset_grib$ID <- NULL
      subset_grib$code <- inshp_loop$NM_MUN
      ## pivoting
      subset_grib <- subset_grib %>%
        pivot_longer(
          -code,
          names_to = c("varname","year","month","day","hour"),
          names_sep = "\\."
        )
      subset_grib <- subset_grib %>% mutate(value = value - 273.15) #changing K to celsius
      #save city/year subset
      write_rds(subset_grib, paste0("00.data//cds_data//",i,"_",a,".rds"))
    }
  }
  
}
# Aggregating data --------------------------------------------------------
filtered_mun <- inshp[inshp$code %in% str_sub(capitals), ]
capitals_filtered <- filtered_mun$NM_MUN
list_file <- c(seq(1999,2009),"2010_2019")

file <- tibble()
for (b in capitals_filtered){
  print(b)
  temp_file2 <- tibble()
  for (c in list_file){
    temp_file1 <- read_rds(paste0("00.data//cds_data//",b,"_",c,".rds"))
    temp_file1 <- temp_file1 %>%
      mutate(date_hour = ymd_h(paste(year, month, day,hour, sep = "-")),
             date_day = ymd(paste(year, month, day, sep = "-")))
    temp_file2 <- bind_rows(temp_file1,temp_file2) %>% 
      dplyr::select(code, date_day, date_hour, value)
  }
  file <- file %>% bind_rows(temp_file2)
}
# checkpoint
# Define the start and end dates
start_date <- as.Date("1999-12-29")
end_date <- as.Date("2020-01-02")
# Calculate the difference in days
days_between_dates <- difftime(end_date, start_date, units = "days")
days_between_dates*24*27
file <- file %>% filter(date_day <= ymd('2020-01-02'))
# Saving 
# write_rds(file, "00.data//cds_data//weather_data_all.rds")
file <- read_rds("00.data//cds_data//weather_data_all.rds")
# Sunrise and Sunset ------------------------------------------------------
library(sf)
library(lubridate)
# getting cities centroids
centr <- centroids(inshp[inshp$code %in% str_sub(capitals), ])
# Extract geometries (points) and their coordinates as a matrix
coords <- geom(centr)
# Convert to dataframe
coords <- as.data.frame(centr, geom = "XY")
coords <- coords %>% 
  rename(lon = x,
         lat = y)
write.csv(coords,'00.data//coords.csv')

# Sunset and sunrise calculations
### Script in Python 
sr_ss <- read.csv('00.data//sunrise_sunset_results.csv')
sr_ss <- sr_ss %>% mutate(
  Sunset = Sunset %>% ymd_hms(),
  Sunrise = Sunrise %>% ymd_hms(),
  Date = Date %>% ymd()
)
sr_ss %>% summary
# Meging dataframes to calculation ----------------------------------------
temperature <- file
temperature <- left_join(temperature, sr_ss, by = c("code" = "NM_MUN",
                                                    "date_day"= "Date"))
temperature <- temperature %>% dplyr::select(code,value,date_day,date_hour, Sunrise, Sunset)
# saving
write_rds(temperature,"00.data//temperature_temp.rds")
temperature <- read_rds("00.data//temperature_temp.rds")
# id
temperature <- temperature %>% arrange(code,date_day)
temperature$id <- 1:length(temperature$code)
temperature %>% summary
# Convert temperature_ to data.table for faster subsetting
get_next_day_sunrise <- function(date,city, tibble) {
  next_date <- date + days(1)
  sunrise_time <- tibble %>% filter(date_day == next_date & code == city) 
  sunrise_time <- sunrise_time$Sunrise[1]
  return(sunrise_time)}
# Mark nightly hours
# getting next day sunrise data
library(data.table)
# Create an empty data table
tib <- data.table()
# Loop over each row of the data frame
# Create an empty list to store results
tib_list <- list()
temperature$code %>% length()/2400
for (a in 1:1974) {
  print(paste("cycle", a))
  seq <- seq(1, temperature$date_day %>% length(), 2400)
  if (is.na(seq[a + 1])) {
    end <- temperature$date_day %>% length()
  } else {
    end <- seq[a + 1]
  }
  temperature_ <- temperature[seq[a]:end, ]
  
  # Create an empty tib for this iteration
  tib <- data.table()
  
  for (i in seq_len(nrow(temperature_))) {
    if (i %% 240 == 0) {
      print(i)
    }
    # Get city and date from temperature data frame
    city <- temperature_$code[i]
    date <- ymd(temperature_$date_day[i])
    
    # Get next day sunrise
    next_day_sunrise <- get_next_day_sunrise(date, city, temperature_)
    liltib <- data.table(next_day_sunrise = next_day_sunrise,
                         hour = hour(temperature_$date_hour[i]), 
                         id = temperature_$id[i])
    # Bind the current result to the existing data table
    tib <- rbind(tib, liltib)
  }
  
  # Save the tib for this iteration in the list
  tib_list[[a]] <- tib
}
final_tib <- rbindlist(tib_list)
# the cycle ends in a number such as 2401, and start over in 2401, so theres 
# a repeated value.
# final_tib %>% write_rds("00.data//tib_list_ns.rds")
tib <- read_rds("00.data//tib_list_ns.rds")
## Removing double ids col with NA value
tib <- tib %>% filter(!(id %in% seq(1, temperature$date_day %>% 
                                      length(), 2400) & is.na(next_day_sunrise)))
# Checking
temperature$code %>% length -
  tib$next_day_sunrise %>% length 

temperature <- left_join(temperature,tib)
# establishing when its night time
temperature <- temperature  %>% 
  mutate(
    nightly_hours = if_else(
      Sunset < next_day_sunrise - days(1),  # Handle sunset before sunrise
      date_hour >= Sunset & date_hour < next_day_sunrise,
      date_hour > (Sunset - days(2)) & date_hour < Sunrise))  %>% 
  mutate(nightly_hours = if_else(
    date_hour >= Sunset & date_hour < next_day_sunrise,
    TRUE,
    nightly_hours))
# establishing the nights
temperature <- temperature %>%
  mutate(
    date_night = if_else(
      date_hour < paste0(date_day,"-15") %>% ymd_h,  # Nighttime
      date_day - days(1),  # Previous day's date
      date_day  # Same day's date
    ))
# saving 
# temperature %>% write_rds("00.data//temperature_final.rds")
temperature <- read_rds("00.data//temperature_final.rds")
# Calculating heat excess -------------------------------------------------
# getting thresholds for min temp (percentile 95)
Tthr <- temperature  %>% 
  group_by(date_day, code) %>% 
  summarise(min = min(value)) %>% 
  group_by(code) %>% 
  summarise(min95 = quantile(min,0.95))
# Due to the process lets remove the 2 last nights of the year 
# and the last night of the previous year
# filtering only night hours
temperature_n <- temperature %>% 
  filter(date_night > ymd("1999-12-31") & date_night < ymd("2020-01-01")) %>% 
  filter (nightly_hours == T) %>% select(id,code,date_day,date_night,value)
temperature_n %>% summary
# Define a function to calculate ITthr(tij)
ITthr <- function(tij, Tthr) {
  ifelse(tij > Tthr, 1, 0)
}
# Calculate HNE for each night for each city
HNE_data <- temperature_n %>%
  left_join(Tthr, by = "code") %>% 
  mutate(HNE = (value - min95)* ITthr(value, min95)) %>%
  group_by(code, date_night) %>% 
  summarise(HNE = sum(HNE))
HNE_data %>% summary
# saving 
write_rds(HNE_data, "00.data//HNE_data.rds")
# Getting mean and min temperature ------------------------------------------------

data_mean_min <- temperature %>% 
  group_by(date_day,code) %>% summarise(mean = mean(value),
                                        min = min(value)) %>% 
  filter(date_day > ymd("1999-12-31") & date_day < ymd("2020-01-01"))
data_mean_min %>% summary
write_rds(data_mean_min, "00.data//data_tmean_min.rds")

# Fetching Weather Data from climr ---------------------------------------------
# rh
dados_rh <- tibble ()
for (i in capitals %>% as.integer()){
  lil <- 
    fetch_data(
      code_muni = i,
      product = "brdwgd",
      indicator = "rh",
      statistics = "mean",
      date_start = as.Date("2000-01-01"),
      date_end = as.Date("2019-12-31")
    )
  lil <- bind_cols(lil,code_muni = i)
  dados_rh <- bind_rows(dados_rh, lil)
}
colnames(dados_rh)[2]="rh"
write_rds(dados_rh,'00.data/data_rh_capitals.rds')

# Koppen Classification brazil
# Following Alvares, C.A., Stape, J.L., Sentelhas, P.C., Gonçalves, J.L.M.; 
# Sparovek, G. Köppen’s climate classification map for Brazil. 
# Meteorologische Zeitschrift, v. 22, n. 6, p. 711-728, 2013.

# https://www.ipef.br/ipefexpress/nr071.htm#:~:text=O%20novo%20mapa%20clim%C3%A1tico%20obtido,%2C%20Cwc%2C%20Csa%2C%20Csb.
# https://koppenbrasil.github.io/

cities <- data_HNE$code %>% unique
climates <- xlsx::read.xlsx("00.data//KoppenBrazilian.xls", sheetIndex = 2) %>% 
  select(1:5)
climates <- filter (climates, Municipality %in% cities) [-c(12,13,16,17,23,27,32),] %>% 
  arrange(Municipality)
# saving 
write_rds(climates, "00.data//climates.rds")

