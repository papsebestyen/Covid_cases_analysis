data.diff <- data.frame()

for (iso in data$iso_code %>% unique()) {
  
  iso_data <- data %>% 
    filter(iso_code == iso) %>% 
    arrange(date) %>% 
    select(c(iso_code, date, new_cases_smoothed_per_million)) %>% 
    mutate(cases = log(new_cases_smoothed_per_million + 1))
  
  iso_data$diff_cases <- c(NA, iso_data$cases %>% diff())
  iso_data
  
  data.diff <- rbind(data.diff, iso_data)
  
}

data.diff <- data.diff %>% select(c(date, iso_code, diff_cases))

data <- left_join(data, data.diff, by = c('date' = 'date', "iso_code" = "iso_code"))


# -------------------------------------------------------------------------

map.url <- 'https://raw.githubusercontent.com/leakyMirror/map-of-europe/master/GeoJSON/europe.geojson'
spdf <- geojson_read(map.url,  what = "sp")


spdf_fortified <- tidy(spdf, region = "ISO3")

spdf@data$SUBREGION %>% length()

# NUMERIC DATA
data.to.plot <- data %>% 
  group_by(iso_code) %>% 
  filter(new_cases != 0) %>% 
  arrange(iso_code, date) %>% 
  filter(row_number() == 1) %>% 
  select(c(iso_code, location, date))

data.to.plot <- left_join(spdf_fortified, data.to.plot, by = c('id' = 'iso_code'))

data.to.plot.names <- data.to.plot %>% 
  group_by(id) %>% 
  summarise(
    long = mean(long),
    lat = mean(lat)
  )

# spdf_fortified$nb_equip[ is.na(spdf_fortified$nb_equip)] = 0.001

ggplot() +
  geom_polygon(data = data.to.plot, aes(fill = date, x = long, y = lat, group = group)) +
  theme_void() +
  coord_map() +
  geom_text(data = data.to.plot.names, aes(x = long, y = lat, label = id), size = 3, col = "black")


# -------------------------------------------------------------------------

# NUMERIC DATA
data.to.plot <- data %>% 
  select(c(iso_code, location)) %>% 
  mutate(date = if (iso_code %in% names(policy.change)) policy.change[[iso_code]] else NA)

# data.to.plot

data.to.plot <- left_join(spdf_fortified, data.to.plot, by = c('id' = 'iso_code'))

data.to.plot.names <- data.to.plot %>% 
  group_by(id) %>% 
  summarise(
    long = mean(long),
    lat = mean(lat)
  )

# spdf_fortified$nb_equip[ is.na(spdf_fortified$nb_equip)] = 0.001

ggplot() +
  geom_polygon(data = data.to.plot, aes(fill = date, x = long, y = lat, group = group)) +
  theme_void() +
  coord_map() +
  geom_text(data = data.to.plot.names, aes(x = long, y = lat, label = id), size = 3, col = "black")


# -------------------------------------------------------------------------

test %>% 
  subset(date.filter(Date)) %>%
  filter(CountryCode %in% cee.iso.codes) %>% 
  select(c(CountryName, CountryCode, Jurisdiction, Date, C6_Stay.at.home.requirements, C6_Flag, lagged)) %>% 
  ggplot(aes(x = Date, y = C6_Stay.at.home.requirements, col = CountryName)) +
  geom_line()

# NINCS SZABÁLYOZÁS
cee.iso.codes.df %>% 
  filter(iso_code %in% setdiff(cee.iso.codes, names(policy.change)))

data$iso_code %>% unique()


# -------------------------------------------------------------------------

# NUMERIC DATA
data.to.plot <- data %>% 
  group_by(iso_code) %>% 
  filter(new_cases != 0) %>% 
  arrange(iso_code, date) %>% 
  filter(row_number() == 1) %>% 
  select(c(iso_code, location, date)) %>% 
  mutate(date = ifelse(iso_code %in% cee.iso.codes, date, NA))

data.to.plot <- left_join(spdf_fortified, data.to.plot, by = c('id' = 'iso_code'))

data.to.plot.names <- data.to.plot %>% 
  group_by(id) %>% 
  summarise(
    long = mean(long),
    lat = mean(lat)
  )

# spdf_fortified$nb_equip[ is.na(spdf_fortified$nb_equip)] = 0.001

ggplot() +
  geom_polygon(data = data.to.plot, aes(fill = date, x = long, y = lat, group = group)) +
  theme_void() +
  coord_map() +
  geom_text(data = data.to.plot.names, aes(x = long, y = lat, label = id), size = 3, col = "black")


# -------------------------------------------------------------------------


data %>% 
  # subset(iso_code == 'HUN') %>% 
  subset(date > as.Date('2020-09-01') & date < as.Date('2020-11-301')) %>%
  arrange(desc(new_cases_smoothed_per_million)) %>% 
  select(c(iso_code, new_cases_smoothed_per_million))

data %>% 
  group_by(location) %>% 
  summarise(m = mean(population)) %>% 
  arrange(m)

data %>% 
  group_by(location) %>% 
  summarise(m = mean(population_density)) %>% 
  arrange(m)


# -------------------------------------------------------------------------

data <- left_join(data.covid, data.policy, by = c('date' = 'Date', 'iso_code' = 'CountryCode'))

# -------------------------------------------------------------------------

model.2 <- model.data %>% 
  lm(diff_cases ~ 
       sipo_0_5 +
       sipo_6_9 +
       sipo_10_14 +
       sipo_15_19 +
       sipo_20 +
       # never_pol +
       iso_code +
       date +
       iso_code * date
     , data = .,
     weights = model.data$population_density)

model.data <- data %>% 
  # subset(date > as.Date('2020-10-1') & date < as.Date('2020-11-30'))
  subset(date > as.Date('2020-03-05') & date < as.Date('2020-4-30')) %>% 
  filter(iso_code != 'DEU') %>% 
  filter(iso_code %in% cee.iso.codes) 

model.2.wo.deu <- model.data %>% 
  lm(diff_cases ~ 
       sipo_0_5 +
       sipo_6_9 +
       sipo_10_14 +
       sipo_15_19 +
       sipo_20 +
       # never_pol +
       iso_code +
       date +
       iso_code * date
     , data = .,
     weights = model.data$population_density)

model.data <- data %>% 
  # subset(date > as.Date('2020-10-1') & date < as.Date('2020-11-30'))
  subset(date > as.Date('2020-03-05') & date < as.Date('2020-4-30')) %>% 
  filter(iso_code != 'RUS') %>% 
  filter(iso_code %in% cee.iso.codes) 

model.2.wo.rus <- model.data %>% 
  lm(diff_cases ~ 
       sipo_0_5 +
       sipo_6_9 +
       sipo_10_14 +
       sipo_15_19 +
       sipo_20 +
       # never_pol +
       iso_code +
       date +
       iso_code * date
     , data = .,
     weights = model.data$population_density)

model.data <- data %>% 
  # subset(date > as.Date('2020-10-1') & date < as.Date('2020-11-30'))
  subset(date > as.Date('2020-03-05') & date < as.Date('2020-4-30')) %>% 
  filter(iso_code != 'RUS' & iso_code != 'DEU') %>% 
  filter(iso_code %in% cee.iso.codes) 

model.2.wo.rusdeu <- model.data %>% 
  lm(diff_cases ~ 
       sipo_0_5 +
       sipo_6_9 +
       sipo_10_14 +
       sipo_15_19 +
       sipo_20 +
       # never_pol +
       iso_code +
       date +
       iso_code * date
     , data = .,
     weights = model.data$population_density)


# -------------------------------------------------------------------------

# ES1 <- summary(mobility.m1)
# ES1$coefficients[,2:4] <- test[,2:4]
# 
# coefs <- data.frame(ES1[["coefficients"]])
# 
# coefs$conf.low <- coefs$Estimate+c(-1)*coefs$Std..Error*qt(0.975,42)
# coefs$conf.high <- coefs$Estimate+c(1)*coefs$Std..Error*qt(0.975,42)
# interest <- c('sipo_0_5TRUE', 'sipo_6_9TRUE', 'sipo_10_14TRUE', 'sipo_15_19TRUE', 'sipo_20TRUE')
# coefs <- coefs[rownames(coefs) %in% interest,]
# coefs$time <- c(0, 6, 10, 15, 20)
# 
# ggplot(coefs, aes(time, Estimate))+
#            geom_line() +
#            geom_point()+
#            geom_pointrange(aes(ymin = conf.low, ymax = conf.high))+
#            geom_hline(yintercept = 0, linetype = 2) +
#   theme_bw()

