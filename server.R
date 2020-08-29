

server <- function(input, output, session) {
  
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

