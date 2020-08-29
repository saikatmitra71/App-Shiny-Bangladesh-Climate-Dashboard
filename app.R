library(shiny)
library(dplyr)
library(tidyr)
library(sf)
library(rmapshaper)
library(lubridate)
library(ggplot2)
library(tmap)
library(leaflet)

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
    st_as_sf()

ui <- fluidPage(
    shinythemes::themeSelector(),
    
    h1("Weather Status of Bangladesh"),
    h4("© Saikat Mitra"),

    sidebarLayout(
        sidebarPanel(
            width = 4,
            fluidRow(
                selectInput(
                    inputId = "variable",
                    "Variable:",
                    c("Air Temperature" = "Air_temp",
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
                    inputId = "theme",
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
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "The Map",
                    leafletOutput("map")
                ),
                tabPanel(
                    "Tables and Static Plots",
                    splitLayout(dataTableOutput("table"),
                                plotOutput("boxplot"))
                )
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
            tm_fill(col = "value", style = input$breaks) +
            tm_borders() +
            tm_style(input$theme)
        tmap_leaflet(map, mode = "view", show = TRUE, add.titles = TRUE)
    })
    
    output$table <- renderDataTable(
        data_new %>% 
            as_tibble() %>% 
            filter(key == input$variable & Year == input$year & Month == input$month) %>% 
            arrange(desc(value)) %>% 
            select(Division, value))
    
    output$boxplot <- renderPlot(
        data %>% 
            mutate(Month = factor(Month, levels = month.name)) %>% 
            gather("key", "value", Air_temp, Avg, Min, Max, Rain, Humidity) %>% 
            mutate(value = as.numeric(value)) %>% 
            filter(key == input$variable & Year == input$year & Month == input$month) %>% 
            ggplot(aes(Division, value)) +
            geom_boxplot(fill = "seagreen4") +
            labs(x = "", y = "", title = "Distribution across Divisions") +
            theme_minimal()
    )
}

shinyApp(ui = ui, server = server)