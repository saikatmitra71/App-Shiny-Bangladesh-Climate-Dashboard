data <- read.csv("www/master.csv")

library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(rmapshaper)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)

shape <- read_sf("www/doc.kml")

shape_div <- shape %>% 
  as_tibble() %>% 
  dplyr::select(Name, geometry) %>% 
  tidyr::spread(key = "Name", value = "geometry") %>% 
  dplyr::mutate(Dhaka = st_union(.$Dhaka, .$Mymensingh)) %>% 
  dplyr::select(-Mymensingh) %>% 
  tidyr::gather("Name", "geometry") %>% 
  st_as_sf()

simp_div <- ms_simplify(shape_div, keep = 0.01, keep_shapes = TRUE)


library(tmap)

data %>% 
  mutate(Date = as_date(paste(Year, Month, "15", sep = "-"))) %>% 
  gather("key", "value", Avg, Min, Max, Rain, Humidity) %>% 
  group_by(Date, Division, key) %>% 
  summarise(summary_rain =  sum(value[key == "Rain"], na.rm = T), 
            summary_other = mean(value[key != "Rain"], na.rm = T)) %>% 
  ungroup() %>% 
  mutate(value = ifelse(summary_rain == 0, summary_other, summary_rain)) %>% 
  left_join(simp_div, by = c("Division" = "Name")) %>% 
  st_as_sf() %>% 
  filter(Date = ) %>% 
  tm_shape() +
  tm_fill(col = "avg") +
  tm_borders()

data %>% 
  mutate(Date = as_date(paste(Year, Month, "15", sep = "-"))) %>% 
  gather("key", "value", Avg, Min, Max, Rain, Humidity) %>% 
  group_by(Date, Division, key) %>% 
  summarise(summary_rain =  sum(value[key == "Rain"], na.rm = T), 
            summary_other = mean(value[key != "Rain"], na.rm = T)) %>% 
  ungroup() %>% 
  mutate(summary_rain = ifelse(is.na(summary_rain), 0, summary_rain)) %>% 
  mutate(value = ifelse(summary_rain == 0, summary_other, summary_rain)) %>% 
  left_join(simp_div, by = c("Division" = "Name")) %>% 
  st_as_sf() %>% 
  .$value %>% is.na() %>% sum()
  
data %>% 
  mutate(Date = as_date(paste(Year, Month, "15", sep = "-"))) %>% 
  gather("key", "value", Avg, Min, Max, Rain, Humidity) %>% 
  group_by(Date, Division, key) %>% 
  summarise(summary_rain =  sum(value[key == "Rain"], na.rm = T), 
            summary_other = mean(value[key != "Rain"], na.rm = T)) %>% 
  ungroup() %>% 
  mutate(summary_rain = ifelse(is.na(summary_rain), 0, summary_rain)) %>% 
  mutate(value = ifelse(summary_rain == 0, summary_other, summary_rain)) %>% 
  mutate(value = ifelse(is.na(value), 0, value)) %>% 
  left_join(simp_div, by = c("Division" = "Name")) %>% 
  st_as_sf()


data %>% 
  mutate(Date = as_date(paste(Year, Month, "15", sep = "-"))) %>% 
  data.frame(Dates = seq(from = max(.$Date), to = min(.$Date))) %>% View()

seq(min(data$Year), max(data$Year)) %>% length

data.frame(
  Year = seq(min(data$Year), max(data$Year)),
  Month = rep(month.abb, each = 38),
  Day = rep(1:31, each = 38)
) %>% View()

day_list <- crossing(
  Year = seq(min(data$Year), max(data$Year)),
  Month = month.name,
  Day = 1:31)

day_list %>% 
  left_join(data, by = c("Year", "Month")) %>% 
  mutate(Date = as_date(paste(Year, Month, Day, sep = "-"))) %>% 
  gather("key", "value", Avg, Min, Max, Rain, Humidity) %>% 
  group_by(Date, Division, key) %>% 
  summarise(summary_rain =  sum(value[key == "Rain"], na.rm = T), 
            summary_other = mean(value[key != "Rain"], na.rm = T)) %>% 
  ungroup() %>% 
  mutate(summary_rain = ifelse(is.na(summary_rain), 0, summary_rain)) %>% 
  mutate(value = ifelse(summary_rain == 0, summary_other, summary_rain)) %>% 
  mutate(value = ifelse(is.na(value), 0, value)) %>% 
  left_join(simp_div, by = c("Division" = "Name")) %>% 
  filter(key == "Avg" & Date == as_date("1981-01-01")) %>% 
  st_as_sf() %>% 
  tm_shape() +
  tm_fill(col = "value") +
  tm_borders()

day_list %>% 
  left_join(data, by = c("Year", "Month")) %>% 
  mutate(Date = as_date(paste(Year, Month, Day, sep = "-"))) %>% 
  gather("key", "value", Avg, Min, Max, Rain, Humidity) %>% 
  group_by(Date, Division, key) %>% 
  summarise(summary_rain =  sum(value[key == "Rain"], na.rm = T), 
            summary_other = mean(value[key != "Rain"], na.rm = T)) %>% 
  ungroup() %>% 
  mutate(summary_rain = ifelse(is.na(summary_rain), 0, summary_rain)) %>% 
  mutate(value = ifelse(summary_rain == 0, summary_other, summary_rain)) %>% 
  mutate(value = ifelse(is.na(value), 0, value)) %>% 
  left_join(simp_div, by = c("Division" = "Name")) %>% 
  filter(key == "Avg" & Date == as_date("1981-01-01")) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = value), col = "black") +
  scale_fill_brewer(type = "Sequential", palette = "Blues", direction = -1) +
  theme_map()

?guide_legend


data %>% 
  gather("key", "value", Avg, Min, Max, Rain, Humidity) %>% 
  group_by(Year, Month, Division, key) %>% 
  summarise(summary_rain =  sum(value[key == "Rain"], na.rm = T), 
            summary_other = mean(value[key != "Rain"], na.rm = T)) %>% 
  ungroup() %>% 
  mutate(summary_rain = ifelse(is.na(summary_rain), 0, summary_rain)) %>% 
  mutate(value = ifelse(summary_rain == 0, summary_other, summary_rain)) %>% 
  mutate(value = ifelse(is.na(value), 0, value)) %>% 
  left_join(simp_div, by = c("Division" = "Name")) %>% 
  st_as_sf()

data_new <- data %>% 
  gather("key", "value", Avg, Min, Max, Rain, Humidity) %>% 
  group_by(Year, Month, Division, key) %>% 
  summarise(summary_rain =  sum(value[key == "Rain"], na.rm = T), 
            summary_other = mean(value[key != "Rain"], na.rm = T)) %>% 
  ungroup() %>% 
  mutate(summary_rain = ifelse(is.na(summary_rain), 0, summary_rain)) %>% 
  mutate(value = ifelse(summary_rain == 0, summary_other, summary_rain)) %>% 
  mutate(value = ifelse(is.na(value), 0, value)) %>% 
  left_join(simp_div, by = c("Division" = "Name")) %>% 
  st_as_sf()

data %>% 
  mutate(Month = factor(Month, levels = month.name)) %>% 
  gather("key", "value", Air_temp, Avg, Min, Max, Rain, Humidity) %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(Year, Month, Division, key) %>% 
  summarise(summary_rain =  sum(value[key == "Rain"], na.rm = T), 
            summary_other = mean(value[key != "Rain"], na.rm = T)) %>% 
  ungroup() %>% 
  mutate(summary_rain = ifelse(is.na(summary_rain), 0, summary_rain)) %>% 
  mutate(value = ifelse(summary_rain == 0, summary_other, summary_rain)) %>% 
  mutate(value = ifelse(is.na(value), 0, value)) %>% 
  left_join(simp_div, by = c("Division" = "Name")) %>% 
  st_as_sf() %>% View()

data %>% 
  gather("key", "value", Air_temp, Avg, Min, Max, Rain, Humidity) %>% 
  filter(Year == 1981, Month == "January", Division == "Barisal", key == "Air_temp") %>% 
  summarise(mean(value, na.rm = T))

data_new %>% 
  as_tibble() %>% 
  filter(Year == 1981, Month == "January", key == "Air_temp") %>% 
  arrange(desc(value)) %>% 
  select(Division, value)

data_new %>% 
  filter(Year == 1981, Month == "January", key == "Air_temp") %>% 
  tm_shape() +
  tm_fill(col = "value") +
  tm_borders() +
  tm_layout(legend.position = c("LEFT", "BOTTOM"))

data %>% 
  filter(Month == "January", key == "Air_temp", Division == "Dhaka") %>% 
  group_by(Month) %>% 
  summarise(summary_rain =  sum(value[key == "Rain"], na.rm = T), 
            summary_other = mean(value[key != "Rain"], na.rm = T)) %>% 
  ungroup() %>% 
  mutate(summary_rain = ifelse(is.na(summary_rain), 0, summary_rain)) %>% 
  mutate(value = ifelse(summary_rain == 0, summary_other, summary_rain)) %>% 
  mutate(value = ifelse(is.na(value), 0, value))

df.selected <- data_new %>% 
  dplyr::select(key, Year, Month, Division)
colnames(df.selected)[4] <- "new"
df.selected


