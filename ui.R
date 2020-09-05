

ui <- dashboardPage(
  dashboardHeader(title = "Bangladesh Climate"),
  dashboardSidebar(
    sidebarMenu(id = "menu",
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                menuItem("The Map", tabName = "map", icon = icon("th")),
                menuItem("Static Plots", tabName = "plots", icon = icon("bar-chart-o"))
    ),
    
    conditionalPanel(
      "input.menu !== 'dashboard'",
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
    
    conditionalPanel(
      "input.menu !== 'dashboard'",
        selectInput(
          inputId = "year",
          "Year:",
          unique(data_new$Year)
      )
    ),
    
    conditionalPanel(
      "input.menu !== 'dashboard'",
      selectInput(
        inputId = "month",
        "Month:",
        unique(data_new$Month)
      )
    ),
    
    conditionalPanel(
      "input.menu === 'map'",
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
    
    conditionalPanel(
      "input.menu === 'map'",
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
    
    conditionalPanel(
      "input.menu === 'map'",
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
      tabItem(tabName = "dashboard",
              h1("At A Glance"),
              fluidRow(tags$head(tags$style(HTML(".small-box.bg-red {height: 125px; background-color: #be1a1a !important; color: #000000 !important;}"))),
                       valueBoxOutput("maxdry", width = 3),
                       valueBoxOutput("maxavg", width = 3),
                       valueBoxOutput("maxrain", width = 3),
                       valueBoxOutput("maxhumidity", width = 3)),
              fluidRow(tags$head(tags$style(HTML(".small-box.bg-green {height: 125px; background-color: #2e8b57 !important; color: #000000 !important;}"))),
                       valueBoxOutput("mindry", width = 3),
                       valueBoxOutput("minavg", width = 3),
                       valueBoxOutput("minrain", width = 3),
                       valueBoxOutput("minhumidity", width = 3)),
              fluidRow(box(title = "Yearly Trend",
                           selectInput(
                             inputId = "dashlinevariable",
                             "Variable:",
                             c("Air Temperature (Dry Bulb)" = "Air_temp",
                               "Maximum Temperature" = "Max",
                               "Minimum Temperature" = "Min",
                               "Average Temperature" = "Avg",
                               "Rainfall" = "Rain",
                               "Humidity" = "Humidity")
                           ),
                           highchartOutput("dashline")),
                       box(title = "Monthly Distribution BoxPlot",
                           selectInput(
                             inputId = "dashboxvariable",
                             "Variable:",
                             c("Air Temperature (Dry Bulb)" = "Air_temp",
                               "Maximum Temperature" = "Max",
                               "Minimum Temperature" = "Min",
                               "Average Temperature" = "Avg",
                               "Rainfall" = "Rain",
                               "Humidity" = "Humidity")
                           ),
                           plotlyOutput("dashboxplot")))),
      tabItem(tabName = "map",
              h2("The Map"),
              column(box(leafletOutput("map", width = "100%", height = "1000px"), width = 12), width = 12)
      ),
      
      tabItem(tabName = "plots",
              h2("Static Plots"),
              box(highchartOutput("barplot")),
              box(plotlyOutput("boxplot"))
      )
    )
  )
)

