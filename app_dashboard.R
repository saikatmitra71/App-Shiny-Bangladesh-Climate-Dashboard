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

ui <- dashboardPage(
  dashboardHeader(title = "Bangladesh Climate"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("The Map", tabName = "map", icon = icon("dashboard")),
      menuItem("Static Plots", tabName = "plots", icon = icon("th"))
    ),
    fluidRow(
      selectInput(
        inputId = "variable",
        "Variable:",
        c("Air Temperature (Dry Bulb)" = "Air_temp",
          "Maximum Temperature" = "Max",
          "Minimum Temperature" = "Min",
          "Average Temperature" = "Avg",
          "Rainfall" = "Rain",
          "Humidity" = "Humidity")
      )
    ),
    fluidRow(
      selectInput(
        inputId = "year",
        "Year:",
        unique(data_new$Year)
      )
    ),
    fluidRow(
      selectInput(
        inputId = "month",
        "Month:",
        unique(data_new$Month)
      )
    ),
    fluidRow(
      selectInput(
        inputId = "palette",
        selected = "GnBu",
        "Palette:",
        c("Yellow-Orage-Red" = "YlOrRd",
          "Yellow-Orange-Brown" = "YlOrBr",
          "Yellow-Green-Blue" = "YlGnBu",
          "Yellow-Green" = "YlGn",
          "Red" = "Reds",
          "Red-Purple" = "RdPu",
          "Purple" = "Purples",
          "Purple-Red" = "PuRd",
          "Purple-Blue-Green" = "PuBuGn",
          "Purple-Blue" = "PuBu",
          "Orange-Red" = "OrRd",
          "Orange" = "Oranges",
          "Grey" = "Greys",
          "Green" = "Greens",
          "Green-Blue" = "GnBu",
          "Blue-Purple" = "BuPu",
          "Blue-Green" = "BuGn",
          "Blue" = "Blues")
      )
    ),
    fluidRow(
      selectInput(
        inputId = "theme",
        selected = "white",
        "Theme:",
        c("White" = "white",
          "Grey" = "grey",
          "Natural" = "natural",
          "Black & White" = "bw",
          "Classic" = "classic",
          "Cobalt" = "cobalt",
          "Albatross" = "albatross",
          "Beaver" = "beaver")
      )
    ),
    fluidRow(
      selectInput(
        inputId = "breaks",
        "Breaks:",
        c("Pretty" = "pretty",
          "SD" = "sd",
          "Equal" = "equal",
          "Quantile" = "quantile",
          "K-Means" = "kmeans",
          "Hierarchical" = "hclust",
          "Fisher Score" = "fisher",
          "Jenks" = "jenks",
          "Head/Tails" = "headtails",
          "Log10 Pretty" = "log10_pretty")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              h2("The Map"),
              leafletOutput("map")
      ),
      
      tabItem(tabName = "plots",
              h2("Static Plots"),
              box(plotOutput("barplot")),
              box(plotlyOutput("boxplot"))
      )
    )
  )
)

server <- function(input, output) {
  
  output$map <- renderLeaflet({
    tmap_mode("view")
    map <- data_new %>% 
      filter(key == input$variable & Year == input$year & Month == input$month) %>% 
      dplyr::select(-key, -Year, -Month) %>% 
      tm_shape() +
      tm_fill(col = "Aggregated Value", style = input$breaks, palette = input$palette) +
      tm_borders() +
      tm_style(input$theme)
    tmap_leaflet(map, mode = "view", show = TRUE, add.titles = TRUE)
  })
  
  output$barplot <- renderPlot(
    data_new %>% 
      filter(key == input$variable & Year == input$year & Month == input$month) %>% 
      ggplot(aes(`Aggregated Value`, reorder(Division, `Aggregated Value`))) +
      geom_bar(stat = "identity", fill = "steelblue", width = 0.3) +
      geom_text(aes(label = round(`Aggregated Value`, 2)), position = position_nudge(x = 1)) +
      labs(x = "Aggregated Value", y = "Divisions") +
      theme_minimal()
  )
  
  output$boxplot <- renderPlotly(
    ggplotly(data %>% 
               mutate(Month = factor(Month, levels = month.name)) %>% 
               gather("key", "Aggregated Value", Air_temp, Avg, Min, Max, Rain, Humidity) %>% 
               mutate(`Aggregated Value` = as.numeric(`Aggregated Value`)) %>% 
               filter(key == input$variable & Year == input$year & Month == input$month) %>% 
               ggplot(aes(Division, `Aggregated Value`)) +
               geom_boxplot(fill = "seagreen4") +
               labs(x = "", y = "", title = "Distribution across Divisions") +
               theme_minimal()) %>% 
      config(displayModeBar = F)
  )
}

server <- function(input, output) {
  
  output$map <- renderLeaflet({
    tmap_mode("view")
    map <- data_new %>% 
      filter(key == input$variable & Year == input$year & Month == input$month) %>% 
      dplyr::select(-key, -Year, -Month) %>% 
      tm_shape() +
      tm_fill(col = "Aggregated Value", style = input$breaks, palette = input$palette) +
      tm_borders() +
      tm_style(input$theme)
    tmap_leaflet(map, mode = "view", show = TRUE, add.titles = TRUE)
  })
  
  output$barplot <- renderPlot(
    data_new %>% 
      filter(key == input$variable & Year == input$year & Month == input$month) %>% 
      ggplot(aes(`Aggregated Value`, reorder(Division, `Aggregated Value`))) +
      geom_bar(stat = "identity", fill = "steelblue", width = 0.3) +
      geom_text(aes(label = round(`Aggregated Value`, 2)), position = position_nudge(x = 1)) +
      labs(x = "Aggregated Value", y = "Divisions") +
      theme_minimal()
  )
  
  output$boxplot <- renderPlotly(
    ggplotly(data %>% 
               mutate(Month = factor(Month, levels = month.name)) %>% 
               gather("key", "Aggregated Value", Air_temp, Avg, Min, Max, Rain, Humidity) %>% 
               mutate(`Aggregated Value` = as.numeric(`Aggregated Value`)) %>% 
               filter(key == input$variable & Year == input$year & Month == input$month) %>% 
               ggplot(aes(Division, `Aggregated Value`)) +
               geom_boxplot(fill = "seagreen4") +
               labs(x = "", y = "", title = "Distribution across Divisions") +
               theme_minimal()) %>% 
      config(displayModeBar = F)
  )
}

shinyApp(ui = ui, server = server)