rm(list = ls())
setwd("C:/Users/papse/Desktop/Beszámoló")

# IMPORT ------------------------------------------------------------------

library(dplyr)

# LOAD --------------------------------------------------------------------

load(file.path('Data', 'data.RData'))

# DATE SUBSET -------------------------------------------------------------

date.filter <- function(date) date > as.Date('2020-01-01') & date < as.Date('2020-4-30')

# POLICY CHANGE -----------------------------------------------------------

add.lag <- function(data, name){
  data[[paste0(name, '_l1')]] <- dplyr::lag(data[[name]])
  return(data)
}

policy.change <- function(data, name){
  data %>% 
    add.lag(name) %>% 
    select(all_of(c('CountryName', "Date", 
                    name, paste0(name, '_l1'), "CountryCode"))) %>%
    filter(.[[name]] != .[[paste0(name, '_l1')]])
}

# ELSÕ BEVEZETÉS
policy.first <- function(data, variable){
  data %>% 
    filter(.[[variable]] >= 2) %>% 
    group_by(CountryName) %>% 
    arrange(CountryName, .[[variable]]) %>% 
    filter(row_number() == 1)
}

# A VARIÁLÓK
policy.variation <- function(data, name){
  data %>% 
    filter(.[[name]] >= 2) %>% 
    arrange(CountryName, .[[name]]) %>% 
    group_by(CountryName) %>% 
    filter(n() >= 2)
}

# A BE SEM VEZETÕK
policy.never <- function(data, name){
  stay.at.home.change.first <- policy.first(stay.at.home.change, 'C6_Stay.at.home.requirements')
  
  data.policy %>% 
    group_by(CountryCode) %>% 
    filter(row_number() == 1) %>% 
    filter(CountryCode %in% setdiff(data.eu.country$ISO3166.1.Alpha.3, stay.at.home.change.first$CountryCode))
}

stay.at.home.change <- policy.change(data.policy, 'C6_Stay.at.home.requirements')
stay.at.home.change.first <- stay.at.home.change %>% 
  filter(C6_Stay.at.home.requirements >= 2) %>% 
  group_by(CountryName) %>% 
  arrange(CountryName, Date) %>% 
  filter(row_number() == 1)
  
school.closing.change <- policy.change(data.policy, 'C1_School.closing')
school.closing.change.first <- school.closing.change %>% 
  filter(C1_School.closing >= 2) %>% 
  group_by(CountryName) %>% 
  arrange(CountryName, Date) %>% 
  filter(row_number() == 1)
  
workplace.closing.change <- policy.change(data.policy, 'C2_Workplace.closing')
workplace.closing.change.first <- workplace.closing.change %>% 
  filter(C2_Workplace.closing >= 2) %>% 
  group_by(CountryName) %>% 
  arrange(CountryName, Date) %>% 
  filter(row_number() == 1)

travel.controls.change <- policy.change(data.policy, 'C8_International.travel.controls')
travel.controls.change.first <- travel.controls.change %>% 
  filter(C8_International.travel.controls >= 2) %>% 
  group_by(CountryName) %>% 
  arrange(CountryName, Date) %>% 
  filter(row_number() == 1)

facial.coverings.change <- policy.change(data.policy, 'H6_Facial.Coverings')
facial.coverings.change.first <- facial.coverings.change %>% 
  filter(H6_Facial.Coverings >= 2) %>% 
  group_by(CountryName) %>% 
  arrange(CountryName, Date) %>% 
  filter(row_number() == 1)

testing_pol.change <- policy.change(data.policy, 'H2_Testing.policy')
testing_pol.change.first <- testing_pol.change %>% 
  filter(H2_Testing.policy >= 2) %>% 
  group_by(CountryName) %>% 
  arrange(CountryName, Date) %>% 
  filter(row_number() == 1)

contact_trace.change <- policy.change(data.policy, 'H3_Contact.tracing')
contact_trace.change.first <- contact_trace.change %>% 
  filter(H3_Contact.tracing >= 2) %>% 
  group_by(CountryName) %>% 
  arrange(CountryName, Date) %>% 
  filter(row_number() == 1)

stay.at.home.change.first %>% arrange(Date) %>% View()
# RELATIVE POLICY CHANGE --------------------------------------------------

# policy.shift <- policy.first(stay.at.home.change, 'C6_Stay.at.home.requirements')

rel.stay.at.home <- function(policy.data, iso, date){
  if (iso %in% policy.data$CountryCode) {
    rel.date <- date - as.Date(policy.data[policy.data$CountryCode == iso,]$Date)
  } else {
    rel.date <- NA
  }
}

data.policy.timing <- data.policy %>% 
  select(c(Date, CountryCode)) %>% 
  rowwise() %>% 
  mutate(sipo.rel.policy = rel.stay.at.home(stay.at.home.change.first, CountryCode, Date)) %>% 
  mutate(school.rel.policy = rel.stay.at.home(school.closing.change.first, CountryCode, Date)) %>% 
  mutate(workplace.rel.policy = rel.stay.at.home(workplace.closing.change.first, CountryCode, Date)) %>% 
  mutate(travel.rel.policy = rel.stay.at.home(travel.controls.change.first, CountryCode, Date)) %>% 
  mutate(facial.rel.policy = rel.stay.at.home(facial.coverings.change.first, CountryCode, Date)) %>% 
  mutate(testing_pol.rel.policy = rel.stay.at.home(testing_pol.change.first, CountryCode, Date)) %>% 
  mutate(contact_trace.rel.policy = rel.stay.at.home(contact_trace.change.first, CountryCode, Date))

data.policy.timing <- data.policy.timing %>% 
  mutate(
    school = ifelse(is.na(school.rel.policy), F, school.rel.policy >= 0),
    workplace = ifelse(is.na(workplace.rel.policy), F, workplace.rel.policy >= 0),
    travel = ifelse(is.na(travel.rel.policy), F, travel.rel.policy >= 0),
    facial = ifelse(is.na(facial.rel.policy), F, facial.rel.policy >= 0),
    testing_pol = ifelse(is.na(testing_pol.rel.policy), F, testing_pol.rel.policy >= 0),
    contact_trace = ifelse(is.na(contact_trace.rel.policy), F, contact_trace.rel.policy >= 0)
  ) %>% 
  mutate(
    sipo_0_5 = ifelse(is.na(sipo.rel.policy), F, 0 <= sipo.rel.policy & sipo.rel.policy <= 5),
    sipo_6_9 = ifelse(is.na(sipo.rel.policy), F, 6 <= sipo.rel.policy & sipo.rel.policy <= 9),
    sipo_10_14 = ifelse(is.na(sipo.rel.policy), F, 10 <= sipo.rel.policy & sipo.rel.policy <= 14),
    sipo_15_19 = ifelse(is.na(sipo.rel.policy), F, 15 <= sipo.rel.policy & sipo.rel.policy <= 19),
    sipo_20 = ifelse(is.na(sipo.rel.policy), F, 20 <= sipo.rel.policy),
    sipo_6_14 = ifelse(is.na(sipo.rel.policy), F, 6 <= sipo.rel.policy & sipo.rel.policy <= 14),
    sipo_15 = ifelse(is.na(sipo.rel.policy), F, 15 <= sipo.rel.policy)
  )

# EXPORT DATA -------------------------------------------------------------

save(data.covid, data.eu.country, data.mobility, data.policy, data.weather, map.data, data.policy.timing, file = file.path('Data', 'data_final.RData'))
save(stay.at.home.change.first,
     school.closing.change.first,
     workplace.closing.change.first,
     travel.controls.change.first,
     facial.coverings.change.first,
     testing_pol.change.first,
     contact_trace.change.first,
     file = file.path('Data', 'data_policy_change.RData'))
