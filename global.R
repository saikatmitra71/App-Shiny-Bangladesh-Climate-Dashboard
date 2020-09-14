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
library(highcharter)

data <- read.csv("www/master.csv", stringsAsFactors = FALSE)
simp_div <- read_sf("www/shape.shp")

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

box_data <- data_new %>% 
  as_tibble() %>% 
  mutate(Month = as.character(Month)) %>% 
  group_by(key) %>% 
  filter(`Aggregated Value` == max(`Aggregated Value`) | `Aggregated Value` == min(`Aggregated Value`)) %>% 
  mutate(Type = ifelse(`Aggregated Value` == max(`Aggregated Value`), "Max", "Min")) %>% 
  group_by(key, Type) %>% 
  slice(1) %>% 
  select(Year, Month, Division, key, Type, `Aggregated Value`)
