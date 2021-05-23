rm(list = ls())

# IMPORT ------------------------------------------------------------------

library(dplyr)
library(ggplot2)
# library(scales)
library(ggnewscale)
library(gridExtra)

# LOAD --------------------------------------------------------------------

load(file.path('Data', 'data_final.RData'))
load(file.path('Data', 'data_policy_change.RData'))

# DATE SUBSET -------------------------------------------------------------

date.filter.start <- function(date) date > as.Date('2019-03-01') & date < as.Date('2020-4-15')
date.filter <- function(date) date > as.Date('2020-02-16') & date < as.Date('2020-4-15')

# COVID DESCRIPTIVE STATISTICS --------------------------------------------

# Kijárási korlátozás bevezetés
stay.at.home.change.first %>% 
  arrange(Date) %>% 
  # View() %>% 
  nrow()

# Nem bevezetõk
data.eu.country %>% 
  filter(ISO3166.1.Alpha.3 %in% setdiff(data.eu.country$ISO3166.1.Alpha.3, stay.at.home.change.first$CountryCode)) %>% 
  select(ISO3166.1.Alpha.3, official_name_en)

# Iskola és munkahely korlátozás bevezetés
school.closing.change.first %>% 
  arrange(Date) %>% 
  # View()
  nrow()

workplace.closing.change.first %>% 
  arrange(Date) %>% 
  # View()
  nrow()

# Utazás korlátozás bevezetés
travel.controls.change.first %>% 
  arrange(Date) %>% 
  # View()
  nrow()

# Maszk korlátozás bevezetés
facial.coverings.change.first %>% 
  arrange(Date) %>% 
  View()
  nrow()

# Népesség
data.covid %>% 
  group_by(location) %>% 
  filter(row_number() == 1) %>%
  select(c(location, population, population_density, iso_code)) %>% 
  # filter(!(iso_code %in% c('RUS', 'AND', 'LUX', 'MNE', 'CYP', 'OWID_KOS', 'MKD', 'SVN','BEL', 'NLD'))) %>% 
  arrange(population) %>% View()

quantile.test <- data.covid %>% 
  group_by(location) %>% 
  filter(row_number() == 1) %>%
  select(c(location, population, population_density, iso_code)) %>% 
  ungroup()

quantile(quantile.test$population_density)
# 65.180
# 136.520

quantile(quantile.test$population)
# 2722291
# 11589616

data.covid %>% 
  group_by(location) %>% 
  filter(row_number() == 1) %>%
  select(c(iso_code, location, population, population_density)) %>% 
  filter(!(iso_code %in% c('RUS', 'AND', 'LUX', 'MNE', 'CYP', 'OWID_KOS', 'MKD', 'SVN'))) %>% 
  ggplot(aes(x = population)) +
  geom_histogram(bins = 10) +
  theme_bw()

# Népsûrûség
data.covid %>% 
  group_by(location) %>% 
  filter(row_number() == 1) %>%
  select(c(location, population, population_density)) %>% 
  arrange(desc(population_density))

data.covid %>% 
  group_by(location) %>% 
  filter(row_number() == 1) %>%
  select(c(location, population, population_density)) %>% 
  ggplot(aes(x = population_density)) +
  geom_histogram(bins = 10) +
  theme_bw()

# COVID kumulált esetek
data.covid %>% 
  group_by(date) %>% 
  summarise(
    mean = mean(total_cases_per_million, na.rm = T)
  ) %>% 
  right_join(., data.covid, by = c('date' = 'date')) %>% 
  filter(date.filter(date)) %>%
  ggplot(aes(x = date, y = total_cases_per_million, fill = location)) +
  geom_line(aes(y = total_cases_per_million), col = 'grey50', na.rm = T) +
  geom_line(aes(y = mean), col = 'black', na.rm = T, size = 1.5) +
  # stat_summary(fun.y='mean', geom="line", colour="black") +
  theme_bw() +
  ylab('Egymillió lakosra vetített összes eset') +
  xlab('Dátum')

ggsave(
  file.path('Figures', 'cases.png'),
  units = 'cm',
  width = 15,
  height = 10
)

data.covid %>% 
  filter(date.filter(date)) %>%
  ggplot(aes(x = date, y = total_cases_per_million, fill = location)) +
  geom_line(aes(y = total_cases_per_million), col = 'grey50', na.rm = T) +
  # stat_summary(fun.y='mean', geom="line", colour="black") +
  theme_bw() 

# nagykutyák
data.covid %>% 
  filter(date.filter.start(date)) %>%
  group_by(iso_code) %>% 
  arrange(iso_code, desc(date)) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(total_cases_per_million))

c('AND', 'LUX', 'ISL', 'ESP', 'CHE', 'ITA', 'BEL', 'IRL')

# COVID elsõ eset
data.covid %>% 
  group_by(location) %>% 
  arrange(location, total_cases) %>%
  filter(row_number() == 1) %>% 
  ggplot(aes(x = date)) +
  geom_histogram() +
  theme_bw()

data.covid %>% 
  group_by(location) %>% 
  arrange(location, total_cases) %>%
  filter(row_number() == 1) %>% 
  arrange(date)

# 2020-02-16 elõtt már jelen
data.covid %>% 
  group_by(location) %>% 
  arrange(location, total_cases) %>%
  filter(row_number() == 1) %>% 
  filter(date <= as.Date('2020-02-16')) %>% 
  nrow()

# Egy esetre jutó tesztek
data.covid %>% 
  group_by(date) %>% 
  summarise(
    mean = mean(total_tests_per_thousand, na.rm = T)
  ) %>% 
  right_join(., data.covid, by = c('date' = 'date')) %>% 
  filter(date.filter(date)) %>%
  ggplot(aes(x = date, y = total_tests_per_thousand, fill = location)) +
  geom_line(aes(y = total_tests_per_thousand), col = 'grey50', na.rm = T) +
  geom_line(aes(y = mean), col = 'black', na.rm = T, size = 1.5) +
  # stat_summary(fun.y='mean', geom="line", colour="black") +
  theme_bw() +
  ylab('Ezer lakosra vetített összes teszt') +
  xlab('Dátum')

ggsave(
  file.path('Figures', 'tests.png'),
  units = 'cm',
  width = 15,
  height = 10
)

# nagykutyák
data.covid %>% 
  filter(date.filter.start(date)) %>%
  group_by(iso_code) %>% 
  arrange(iso_code, desc(date)) %>% 
  filter(row_number() == 1) %>% 
  arrange(desc(total_tests_per_thousand))
  
# data.covid$tests_per_case
# MOBILITY DESCRIPTIVE STATISTICS -----------------------------------------

data.mobility %>% 
  filter(date.filter(date)) %>%
  ggplot(aes(x = date, y = residential_percent_change_from_baseline, col = country_region)) +
  geom_line(na.rm = T) +
  theme_bw()

data.mobility %>% 
  group_by(date) %>% 
  summarise(
    mean = mean(residential_percent_change_from_baseline, na.rm = T)
  ) %>% 
  right_join(., data.mobility, by = c('date' = 'date')) %>% 
  filter(date.filter(date)) %>%
  ggplot(aes(x = date, y = residential_percent_change_from_baseline, fill = ISO3166.1.Alpha.3)) +
  geom_line(aes(y = residential_percent_change_from_baseline), col = 'grey50', na.rm = T) +
  geom_line(aes(y = mean), col = 'black', na.rm = T, size = 1.5) +
  # stat_summary(fun.y='mean', geom="line", colour="black") +
  theme_bw() +
  ylab('Otthonmaradás a bázisidõszakhoz képest') +
  xlab('Dátum')

ggsave(
  file.path('Figures', 'mobility.png'),
  units = 'cm',
  width = 15,
  height = 10
)

# POLICY MAP --------------------------------------------------------------


plot.policy.map <- function(policy.data, variable){
  data.to.plot <- policy.data %>% 
    filter(date.filter.start(Date)) %>% 
    select(c(CountryName, CountryCode, Date))
  
  data.to.plot <- left_join(map.data, data.to.plot, by = c('id' = 'CountryCode')) %>% 
    mutate(Nincs = ifelse(is.na(Date), 'Nem vezetett be', NA)) %>% 
    mutate(rus = ifelse(id == 'RUS', 'rus', NA)) %>% 
    mutate(Dátum = Date)
  
  ggplot() +
    geom_polygon(data = data.to.plot, aes(fill = Dátum, x = long, y = lat, group = group)) +
    new_scale_fill() +
    geom_polygon(data = data.to.plot, aes(fill = Nincs, x = long, y = lat, group = group), na.rm = T) +
    scale_fill_manual(values = c('Nem vezetett be' = 'black'), na.value = 'transparent', na.translate = F) +
    new_scale_fill() +
    geom_polygon(data = data.to.plot, aes(fill = rus, x = long, y = lat, group = group), na.rm = T) +
    scale_fill_manual(values = c('rus' = 'grey50'), na.value = 'transparent', na.translate = F, guide = F) + 
    theme(legend.position = "none") +
    theme_void() +
    coord_map() +
    ggtitle(variable)
}

plot.policy.map(stay.at.home.change.first, NULL)

ggsave(
  file.path('Figures', 'sipo.png'),
  units = 'cm',
  width = 15,
  height = 10
)

appendix.1 <- grid.arrange(
  plot.policy.map(stay.at.home.change.first, 'Kijárási korlátozás'),
  plot.policy.map(school.closing.change.first, 'Iskolák bezárása'),
  plot.policy.map(workplace.closing.change.first, 'Munkahelyek bezárása'),
  plot.policy.map(travel.controls.change.first, 'Nemzetközi utazás korlátozása'),
  plot.policy.map(facial.coverings.change.first, 'Maszkokhordás elõírása'),
 nrow = 3)

ggsave(
  plot = appendix.1,
  file.path('Figures', 'policy_appendix.png'),
  units = 'cm',
  width = 20,
  height = 25
)

# First case map ----------------------------------------------------------

# NUMERIC DATA
data.to.plot <- data.covid %>% 
  group_by(iso_code) %>% 
  filter(new_cases != 0) %>% 
  arrange(iso_code, date) %>% 
  filter(row_number() == 1) %>% 
  select(c(iso_code, location, date)) %>% 
  mutate(Dátum = date)

data.to.plot <- left_join(map.data, data.to.plot, by = c('id' = 'iso_code'))

ggplot() +
  geom_polygon(data = data.to.plot, aes(fill = Dátum, x = long, y = lat, group = group)) +
  theme_void() +
  coord_map()

ggsave(
  file.path('Figures', 'first_case_map.png'),
  units = 'cm',
  width = 15,
  height = 10
)

# Descriptive table -------------------------------------------------------

describe.data <- left_join(data.covid, data.policy.timing, by = c('date' = 'Date', 'iso_code' = 'CountryCode'))
describe.data <- left_join(describe.data, data.mobility, by = c('date' = 'date', 'iso_code' = 'ISO3166.1.Alpha.3'))
describe.data <- left_join(describe.data, data.weather, by = c('date' = 'time', 'iso_code' = 'ISO3166.1.Alpha.3'))
describe.data <- describe.data %>% 
  filter(date.filter(date))

describe.data <- describe.data %>% 
  select(c(date, 
           iso_code,
           residential_percent_change_from_baseline,
           total_cases_per_million,
           new_cases_per_million,
           total_tests_per_thousand,
           sipo.rel.policy,
           school.rel.policy,
           workplace.rel.policy,
           travel.rel.policy,
           facial.rel.policy,
           TEMP,
           PRECIPITATION)) %>% 
  mutate(
    sipo = sipo.rel.policy >= 0
  )

library(modelsummary)

describe.data %>% 
  summary()

describe.data %>%
  group_by(iso_code) %>% 
  filter(row_number()==1) %>% 
  View()
  mutate(
    sipo = ifelse(is.na(sipo), F, T),
    school = ifelse(is.na(school), F, T),
    workplace = ifelse(is.na(workplace), F, T),
    travel = ifelse(is.na(travel), F, T),
    facial = ifelse(is.na(facial), F, T)
  ) %>% 
  ungroup() %>% 
  select(!c(iso_code)) %>% 
  datasummary_skim(., type = 'categorical')

describe.data %>% 
  filter(sipo == F) %>% 
  summary()

describe.data %>%
  filter(sipo == F) %>% 
  group_by(iso_code) %>% 
  filter(row_number()==1) %>% 
  mutate(
    sipo = ifelse(is.na(sipo), F, T),
    school = ifelse(is.na(school), F, T),
    workplace = ifelse(is.na(workplace), F, T),
    travel = ifelse(is.na(travel), F, T),
    facial = ifelse(is.na(facial), F, T)
  ) %>% 
  ungroup() %>% 
  select(!c(iso_code)) %>% 
  View()
  datasummary_skim(., type = 'categorical')

describe.data %>% 
  filter(sipo == T) %>% 
  summary()

describe.data %>%
  filter(sipo == T) %>% 
  group_by(iso_code) %>% 
  filter(row_number()==1) %>% 
  mutate(
    sipo = ifelse(is.na(sipo), F, T),
    school = ifelse(is.na(school), F, T),
    workplace = ifelse(is.na(workplace), F, T),
    travel = ifelse(is.na(travel), F, T),
    facial = ifelse(is.na(facial), F, T)
  ) %>% 
  ungroup() %>% 
  select(!c(iso_code)) %>% 
  datasummary_skim(., type = 'categorical')



  