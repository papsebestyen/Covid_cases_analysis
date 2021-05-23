setwd("C:/Users/papse/Desktop/Besz�mol�")

# IMPORT ------------------------------------------------------------------

# Basic adatmanipul�ci�
library(dplyr)

# Map manipul�ci�
library(geojsonio)
library(broom)


# DATA FILTERS ------------------------------------------------------------

country.to.drop <- c('VAT', 'MCO', 'SMR', 'LIE', 'MLT', 'GGY', 'JEY', 'IMN', 'FRO', 'GIB', 'SJM', 'ALA', 'RUS')

# EUROPE COUNTRY DATA -----------------------------------------------------
# Forr�s: https://github.com/datasets/country-codes/blob/master/data/country-codes.csv

data.eu.country <- read.csv('https://raw.githubusercontent.com/datasets/country-codes/master/data/country-codes.csv',
                             encoding = 'UTF8')

data.eu.country <- data.eu.country %>% 
  filter(Region.Name == 'Europe') %>% 
  filter(!(ISO3166.1.Alpha.3 %in% country.to.drop))

# GOOGLE MOBILITY DATA ----------------------------------------------------
# Forr�s: https://www.google.com/covid19/mobility/
#         https://support.google.com/covid19-mobility/answer/9825414?hl=en&ref_topic=9822927

data.mobility <- read.csv(file.path('Data', 'Global_Mobility_Report.csv'))

# Alpha-3 orsz�g k�dok csatol�sa
data.mobility <- data.eu.country %>% 
  select(c(ISO3166.1.Alpha.2, ISO3166.1.Alpha.3)) %>% 
  right_join(., data.mobility, by = c('ISO3166.1.Alpha.2' = 'country_region_code'))

# Ezek nincsenek benne a mobility adatban
data.eu.country %>% 
  filter(!(ISO3166.1.Alpha.2 %in% data.mobility$ISO3166.1.Alpha.2)) %>% 
  select(official_name_en) %>% 
  as.list()

# Sz�r�s eur�p�ra
data.mobility <- data.mobility %>% 
  filter(ISO3166.1.Alpha.2 %in% data.eu.country$ISO3166.1.Alpha.2) %>% 
  filter(sub_region_1 == '') %>% 
  filter(metro_area == '')

# Adat manipul�ci�
data.mobility <- data.mobility %>% 
  mutate(date = as.Date(date))

# COVID DATA --------------------------------------------------------------

data.covid <- readxl::read_xlsx(file.path('Data', 'owid-covid-data.xlsx'))

# Sz�r�s eur�p�ra
data.covid <- data.covid %>% 
  filter(continent == 'Europe') %>% 
  filter(!(iso_code %in% country.to.drop))

# Adat manipul�ci�
data.covid <- data.covid %>% 
  mutate(date = as.Date(date))

# POLICY DATA -------------------------------------------------------------

data.policy <- read.csv(file.path('Data', 'OxCGRT_latest.csv'))

# Sz�r�s eur�p�ra
data.policy <- data.policy %>% 
  filter(CountryCode %in% data.eu.country$ISO3166.1.Alpha.3)

# Adat manipul�ci�
data.policy <- data.policy %>% 
  filter(Jurisdiction == 'NAT_TOTAL') %>% 
  mutate(
    Date = as.Date(as.character(Date), '%Y%m%d')
  )

# WEATHER DATA -----------------------------------------------------------
# Forr�s: https://www.kaggle.com/vishalvjoseph/weather-dataset-for-covid19-predictions

data.weather <- read.csv(file.path('Data', 'daily_weather_2020.csv'))

# Sz�r�s eur�p�ra
data.weather <- data.weather %>% 
  filter(Country.Region %in% data.eu.country$official_name_en)

# Adat manipul�ci�
data.weather <- data.weather %>% 
  mutate(time = as.Date(time))

# �tlag h�m�rs�klet �s csapad�k mennyis�g
data.weather <- data.weather %>% 
  mutate(
    TEMP = (temperatureMax - temperatureMin)/2,
    PRECIPITATION = precipIntensity
    )

# Alpha 3 k�dok csatol�sa
data.weather <- data.eu.country %>% 
  select(official_name_en, ISO3166.1.Alpha.3) %>% 
  right_join(., data.weather, by = c('official_name_en' =  'Country.Region'))

# Adat korrekci�
data.weather <- data.weather %>% 
  filter(official_name_en == Province.State | Province.State == '') %>% 
  group_by(ISO3166.1.Alpha.3, time) %>% 
  filter(row_number() == 1)
  
# MAP DATA ----------------------------------------------------------------

map.url <- 'https://raw.githubusercontent.com/leakyMirror/map-of-europe/master/GeoJSON/europe.geojson'
map.raw <- geojson_read(map.url,  what = "sp")

map.data <- tidy(map.raw, region = "ISO3")

# EXPORT DATA -------------------------------------------------------------

save(data.covid, data.eu.country, data.mobility, data.policy, data.weather, map.data, file = file.path('Data', 'data.RData'))

