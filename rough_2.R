shape_station <- read_sf("www/bmd stations/bmd_stations_shape.shp")

data_new <- data %>% 
  mutate(Month = factor(Month, levels = month.name)) %>% 
  gather("key", "value", Air_temp, Avg, Min, Max, Rain, Humidity) %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(Year, Month, Station, key) %>% 
  summarise(summary_rain =  sum(value[key == "Rain"], na.rm = T), 
            summary_other = mean(value[key != "Rain"], na.rm = T)) %>% 
  ungroup() %>% 
  mutate(summary_rain = ifelse(is.na(summary_rain), 0, summary_rain)) %>% 
  mutate(value = ifelse(summary_rain == 0, summary_other, summary_rain)) %>% 
  mutate(value = ifelse(is.na(value), 0, value)) %>% 
  left_join(shape_station, by = c("Station")) %>% 
  st_as_sf()

data_new %>% 
  filter(key == "Avg" & Year == 1981 & Month == "January") %>% 
  tm_shape() +
  tm_fill(col = "value") +
  tm_borders()

test <- data_new %>% 
  filter(key == "Avg" & Year == 1981 & Month == "January") %>% 
  ggplot(aes(value, reorder(Division, value))) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
  labs(x = "", y = "Divisions") +
  theme_minimal()
ggplotly(test)

ggplotly(data_new %>% 
           filter(key == "Avg" & Year == 1981 & Month == "January") %>% 
           ggplot(aes(value, reorder(Division, value))) +
           geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
           labs(x = "", y = "Divisions") +
           theme_minimal())
