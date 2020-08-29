library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(sf)
library(rmapshaper)
library(lubridate)
library(ggplot2)
library(tmap)
library(leaflet)
library(plotly)

data <- read.csv("www/master.csv")
shape <- read_sf("www/doc.kml")

shape_div <- shape %>% 
  as_tibble() %>% 
  dplyr::select(Name, geometry) %>% 
  spread(key = "Name", value = "geometry") %>% 
  mutate(Dhaka = st_union(.$Dhaka, .$Mymensingh)) %>% 
  dplyr::select(-Mymensingh) %>% 
  gather("Name", "geometry") %>% 
  st_as_sf()

simp_div <- ms_simplify(shape_div, keep = 0.01, keep_shapes = TRUE)

data_new <- data %>% 
  mutate(Month = factor(Month, levels = month.name)) %>% 
  gather("key", "Aggregated Value", Air_temp, Avg, Min, Max, Rain, Humidity) %>% 
  mutate(`Aggregated Value` = as.numeric(`Aggregated Value`)) %>% 
  group_by(Year, Month, Division, key) %>% 
  summarise(summary_rain =  sum(`Aggregated Value`[key == "Rain"], na.rm = T), 
            summary_other = mean(`Aggregated Value`[key != "Rain"], na.rm = T)) %>% 
  ungroup() %>% 
  mutate(summary_rain = ifelse(is.na(summary_rain), 0, summary_rain)) %>% 
  mutate(`Aggregated Value` = ifelse(summary_rain == 0, summary_other, summary_rain)) %>% 
  mutate(`Aggregated Value` = ifelse(is.na(`Aggregated Value`), 0, `Aggregated Value`)) %>% 
  left_join(simp_div, by = c("Division" = "Name")) %>% 
  st_as_sf()

