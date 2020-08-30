

server <- function(input, output) {
  
  output$maxdry <- renderValueBox({
    valueBox(
      HTML(paste0(round(box_data[1, 6], 2), "&#8451")),
      HTML(paste0("Maximum Dry Bulb", br(),
                  box_data[1,3], br(),
                  box_data[1,2], ", ", box_data[1,1])),
      color = "red"
    )
  })
  
  output$maxavg <- renderValueBox({
    valueBox(
      HTML(paste0(round(box_data[3, 6], 2), "&#8451")),
      HTML(paste0("Maximum Average Temperature", br(),
                  box_data[3,3], br(),
                  box_data[3,2], ", ", box_data[3,1])),
      color = "red"
    )
  })
  
  output$maxrain <- renderValueBox({
    valueBox(
      paste0(round(box_data[11, 6], 2), "mm", sep = ""),
      HTML(paste0("Maximum Rain", br(),
                  box_data[11,3], br(),
                  box_data[11,2], ", ", box_data[11,1])),
      color = "red"
    )
  })
  
  output$maxhumidity <- renderValueBox({
    valueBox(
      paste0(round(box_data[5, 6], 2), "%", sep = ""),
      HTML(paste0("Maximum Humidity", br(),
                  box_data[5,3], br(),
                  box_data[5,2], ", ", box_data[5,1])),
      color = "red"
    )
  })
  
  output$mindry <- renderValueBox({
    valueBox(
      HTML(paste0(round(box_data[2, 6], 2), "&#8451")),
      HTML(paste0("Minimum Dry Bulb", br(),
                  box_data[2,3], br(),
                  box_data[2,2], ", ", box_data[2,1])),
      color = "green"
    )
  })
  
  output$minavg <- renderValueBox({
    valueBox(
      HTML(paste0(round(box_data[4, 6], 2), "&#8451")),
      HTML(paste0("Minimum Average Temperature", br(),
                  box_data[4,3], br(),
                  box_data[4,2], ", ", box_data[4,1])),
      color = "green"
    )
  })
  
  output$minrain <- renderValueBox({
    valueBox(
      paste0(round(box_data[12, 6], 2), "mm", sep = ""),
      HTML(paste0("Minumum Rain", br(),
                  box_data[12,3], br(),
                  box_data[12,2], ", ", box_data[12,1])),
      color = "green"
    )
  })
  
  output$minhumidity <- renderValueBox({
    valueBox(
      paste0(round(box_data[6, 6], 2), "%", sep = ""),
      HTML(paste0("Minimum Humidity", br(),
                  box_data[6,3], br(),
                  box_data[6,2], ", ", box_data[6,1])),
      color = "green"
    )
  })
  
  output$avgtempseries <- renderHighchart({
    data_new %>% 
      as_tibble() %>% 
      filter(key == "Avg") %>% 
      group_by(Year) %>% 
      summarise(`Aggregated Value` = mean(`Aggregated Value`)) %>% 
      hchart(
        'line',
        hcaes(x = Year, y = `Aggregated Value`),
        name = "Average Temperature"
      ) %>% 
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = "Average Temperature"))
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
  
  output$barplot <- renderHighchart(
    data_new %>% 
      filter(key == input$variable & Year == input$year & Month == input$month) %>% 
      arrange(desc(`Aggregated Value`)) %>% 
      hchart(
        'bar',
        hcaes(Division, `Aggregated Value`)
      ) %>% 
      hc_xAxis(title = list(text = ""))
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


