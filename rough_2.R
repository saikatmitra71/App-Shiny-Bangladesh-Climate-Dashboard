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

data_new %>% 
  filter(key == "Avg") %>% 
  group_by(Year) %>% 
  summarise(`Aggregated Value` = mean(`Aggregated Value`)) %>% 
  hchart(
    'line',
    hcaes(Year, `Aggregated Value`),
    name = "Average Temperature"
  ) %>% 
  hc_xAxis(title = list(text = "")) %>% 
  hc_yAxis(title = list(text = "Average Temperature"))

data_new %>% 
  filter(key == input$variable & Year == input$year & Month == input$month) %>% 
  arrange(desc(`Aggregated Value`)) %>% 
  hchart(
    'bar',
    hcaes(Division, `Aggregated Value`)
  ) %>% 
  hc_xAxis(title = list(text = ""))

data %>% 
  mutate(Month = factor(Month, levels = month.name)) %>% 
  gather("key", "Aggregated Value", Air_temp, Avg, Min, Max, Rain, Humidity) %>% 
  mutate(`Aggregated Value` = as.numeric(`Aggregated Value`)) %>% 
  filter(key == input$variable & Year == input$year & Month == input$month) %>% 
  ggplot(aes(Division, `Aggregated Value`)) +
  geom_boxplot(fill = "seagreen4") +
  labs(x = "", y = "", title = "Distribution across Divisions") +
  theme_minimal()

data_new %>% 
  as_tibble() %>% 
  group_by(key) %>% 
  filter(`Aggregated Value` == max(`Aggregated Value`) | `Aggregated Value` == min(`Aggregated Value`)) %>% 
  mutate(Type = ifelse(`Aggregated Value` == max(`Aggregated Value`), "Max", "Min")) %>% 
  group_by(key, Type) %>% 
  slice(1)

data_new %>% 
  as_tibble() %>% 
  filter(key == "Avg") %>% 
  hchart('hcboxplot',
         hcaes(Month, `Aggregated Value`))

data_new %>% 
  as_tibble() %>% 
  filter(key == "Avg") %>% 
  hcboxplot(
  y = .$Month,
  x = .$`Aggregated Value`
  ) %>% 
  hc_chart(type = "column")

hcboxplot(
  y = data_new$Month,
  x = data_new$`Aggregated Value`
)

data_new %>% 
  as_tibble() %>% 
  filter(key == "Avg") %>% 
  data_to_boxplot(
    variable = `Aggregated Value`,
    group_var = Month
  )

data_new %>% 
  as_tibble() %>% 
  filter(key == "Avg") %>% 
  ggplot(aes(Month, `Aggregated Value`)) +
  geom_boxplot(fill = "seagreen4") +
  labs(x = "") +
  theme_minimal()


