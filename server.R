

server <- function(input, output) {
  
  observe({
    showNotification(
      ui = "Values of all the variables are Averaged by month for aggregation, except Rainfall: it was summed.",
      duration = NULL
    )
  })
  
  output$maxdry <- renderValueBox({
    valueBox(
      tags$p(HTML(paste0(round(box_data[1, 6], 2), "&#8451")),
             style = "font-size: 70%; line-height: 50%;"),
      tags$p(HTML(paste0("Maximum Dry Bulb", br(),
                  box_data[1,3], " Division",br(),
                  box_data[1,2], ", ", box_data[1,1])),
             style = "font-size: 100%; line-height: 100%;"),
      color = "red",
      icon = icon("cloud-sun-rain", lib = "font-awesome")
    )
  })
  
  output$maxavg <- renderValueBox({
    valueBox(
      tags$p(HTML(paste0(round(box_data[3, 6], 2), "&#8451")),
             style = "font-size: 70%; line-height: 50%;"),
      tags$p(HTML(paste0("Maximum Average Temperature", br(),
                  box_data[3,3], " Division", br(),
                  box_data[3,2], ", ", box_data[3,1])),
             style = "font-size: 100%; line-height: 100%;"),
      color = "red",
      icon = icon("cloud-sun-rain", lib = "font-awesome")
    )
  })
  
  output$maxrain <- renderValueBox({
    valueBox(
      tags$p(paste0(round(box_data[11, 6], 2), "mm", sep = ""),
             style = "font-size: 70%; line-height: 50%;"),
      tags$p(HTML(paste0("Maximum Rainfall", br(),
                  box_data[11,3], " Division", br(),
                  box_data[11,2], ", ", box_data[11,1])),
             style = "font-size: 100%; line-height: 100%;"),
      color = "red",
      icon = icon("cloud-sun-rain", lib = "font-awesome")
    )
  })
  
  output$maxhumidity <- renderValueBox({
    valueBox(
      tags$p(paste0(round(box_data[5, 6], 2), "%", sep = ""),
             style = "font-size: 70%; line-height: 50%;"),
      tags$p(HTML(paste0("Maximum Humidity", br(),
                  box_data[5,3], " Division", br(),
                  box_data[5,2], ", ", box_data[5,1])),
             style = "font-size: 100%; line-height: 100%;"),
      color = "red",
      icon = icon("cloud-sun-rain", lib = "font-awesome")
    )
  })
  
  output$mindry <- renderValueBox({
    valueBox(
      tags$p(HTML(paste0(round(box_data[2, 6], 2), "&#8451")),
             style = "font-size: 70%; line-height: 50%;"),
      tags$p(HTML(paste0("Minimum Dry Bulb", br(),
                  box_data[2,3], " Division", br(),
                  box_data[2,2], ", ", box_data[2,1])),
             style = "font-size: 100%; line-height: 100%;"),
      color = "green",
      icon = icon("cloud-sun-rain", lib = "font-awesome")
    )
  })
  
  output$minavg <- renderValueBox({
    valueBox(
      tags$p(HTML(paste0(round(box_data[4, 6], 2), "&#8451")),
             style = "font-size: 70%; line-height: 50%;"),
      tags$p(HTML(paste0("Minimum Average Temperature", br(),
                  box_data[4,3], " Division", br(),
                  box_data[4,2], ", ", box_data[4,1])),
             style = "font-size: 100%; line-height: 100%;"),
      color = "green",
      icon = icon("cloud-sun-rain", lib = "font-awesome")
    )
  })
  
  output$minrain <- renderValueBox({
    valueBox(
      tags$p(paste0(round(box_data[12, 6], 2), "mm", sep = ""),
             style = "font-size: 70%; line-height: 50%;"),
      tags$p(HTML(paste0("Minimum Rainfall", br(),
                  box_data[12,3], " Division", br(),
                  box_data[12,2], ", ", box_data[12,1])),
             style = "font-size: 100%; line-height: 100%;"),
      color = "green",
      icon = icon("cloud-sun-rain", lib = "font-awesome")
    )
  })
  
  output$minhumidity <- renderValueBox({
    valueBox(
      tags$p(paste0(round(box_data[6, 6], 2), "%", sep = ""),
             style = "font-size: 70%; line-height: 50%;"),
      tags$p(HTML(paste0("Minimum Humidity", br(),
                  box_data[6,3], " Division", br(),
                  box_data[6,2], ", ", box_data[6,1])),
             style = "font-size: 100%; line-height: 100%;"),
      color = "green",
      icon = icon("cloud-sun-rain", lib = "font-awesome")
    )
  })
  
  output$dashline <- renderHighchart({
    data_new %>% 
      as_tibble() %>% 
      filter(key == input$dashlinevariable) %>% 
      group_by(Year) %>% 
      summarise(`Aggregated Value` = ifelse(input$dashlinevariable == "Rain",
                                            sum(`Aggregated Value`),
                                            mean(`Aggregated Value`))) %>% 
      hchart(
        'line',
        hcaes(x = Year, y = `Aggregated Value`),
        name = "Value",
        regression = TRUE
      ) %>% 
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = "")) %>% 
      hc_add_dependency("plugins/highcharts-regression.js")
  })
  
  output$dashboxplot <- renderPlotly({
    ggplotly(
      data_new %>% 
        as_tibble() %>% 
        filter(key == input$dashboxvariable) %>% 
        ggplot(aes(Month, `Aggregated Value`)) +
        geom_boxplot(fill = "dodgerblue1") +
        labs(x = "") +
        theme_minimal() +
        theme(axis.text.x=element_text(angle=30, hjust=1))
    ) %>% 
      config(displayModeBar = F)
  })
  
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
  
}


