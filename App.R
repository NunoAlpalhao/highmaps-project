# Load packages -----------------------------------------------------
library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)

# Load data ---------------------------------------------------------

test <- read.csv("Data.csv")

data_fake <- test %>% 
  select(code = `hc.key`, Total = `total`, country = `Country`, year = `Year`, colnames(test[4]),colnames(test[13]),colnames(test[14]),
         colnames(test[15]),colnames(test[16]),colnames(test[17]),colnames(test[18]),colnames(test[19]),
         colnames(test[20]),colnames(test[21]),colnames(test[22]))


# Determine years in data -------------------------------------------
years <- unique(test$Year)


# UI ----------------------------------------------------------------
ui <- fluidPage(
  
  # App title -------------------------------------------------------
  titlePanel("Greenhouse Gases by Sector"),
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs --------------------------------------------------------    
    sidebarPanel(
      
      sliderInput("year", 
                  label = "Year",
                  min = min(years), 
                  max = max(years), 
                  step = 1,
                  sep = '',
                  value = range(years)),
      
      selectInput("type", 
                  label = "Sector",
                  choices = c('Total CO2 Emissions' = colnames(data_fake[2]),
                              'Energy' = colnames(test[13]),
                              'Industrial Processes' = colnames(test[14]),
                              'Agriculture' = colnames(test[15]),
                              'Waste' = colnames(test[16]),
                              'Land-Use Change and Forestry' = colnames(test[17]),
                              'Bunker Fuels' = colnames(test[18]),
                              'Electricity/Heat' = colnames(test[19]),
                              'Manufacturing/Construction' = colnames(test[20]),
                              'Transportation' = colnames(test[21]),
                              'Other Fuel Combustion' = colnames(test[22]))),
      
      highchartOutput("j", height = "600px")
      
      
      
    ),
  
    # Output --------------------------------------------------------    
    mainPanel(
      highchartOutput("hcontainer", height = "800px")
    )
    
  )
)

# SERVER ------------------------------------------------------------
server = function(input, output) {
  
  gas <- reactive({
    data_fake %>% 
      filter(year %in% c(input$year[1],input$year[2])) %>%
      group_by(country) %>%
      arrange(year) %>%
      arrange(country) %>%
      mutate(Total = ifelse(!is.na(Total),ifelse(!is.na(lag(Total)),100*round((Total / lag(Total)) -1,2),0),0)) %>%
      mutate(Energy..MtCO2e. = ifelse(!is.na(Energy..MtCO2e.),ifelse(!is.na(lag(Energy..MtCO2e.)),100*round((Energy..MtCO2e. / lag(Energy..MtCO2e.)) -1,2),0),0)) %>%
      mutate(Industrial.Processes..MtCO2e. = ifelse(!is.na(Industrial.Processes..MtCO2e.),ifelse(!is.na(lag(Industrial.Processes..MtCO2e.)),100*round((Industrial.Processes..MtCO2e. / lag(Industrial.Processes..MtCO2e.)) -1,2),0),0)) %>%
      mutate(Agriculture..MtCO2e. = ifelse(!is.na(Agriculture..MtCO2e.),ifelse(!is.na(lag(Agriculture..MtCO2e.)),100*round((Agriculture..MtCO2e. / lag(Agriculture..MtCO2e.)) -1,2),0),0)) %>%
      mutate(Waste..MtCO2e. = ifelse(!is.na(Waste..MtCO2e.),ifelse(!is.na(lag(Waste..MtCO2e.)),100*round((Waste..MtCO2e. / lag(Waste..MtCO2e.)) -1,2),0),0)) %>%
      mutate(Land.Use.Change.and.Forestry..MtCO2. = ifelse(!is.na(Land.Use.Change.and.Forestry..MtCO2.),ifelse(!is.na(lag(Land.Use.Change.and.Forestry..MtCO2.)),100*round((Land.Use.Change.and.Forestry..MtCO2. / lag(Land.Use.Change.and.Forestry..MtCO2.)) -1,2),0),0)) %>%
      mutate(Bunker.Fuels..MtCO2. = ifelse(!is.na(Bunker.Fuels..MtCO2.),ifelse(!is.na(lag(Bunker.Fuels..MtCO2.)),100*round((Bunker.Fuels..MtCO2. / lag(Bunker.Fuels..MtCO2.)) -1,2),0),0)) %>%
      mutate(Electricity.Heat..MtCO2. = ifelse(!is.na(Electricity.Heat..MtCO2.),ifelse(!is.na(lag(Electricity.Heat..MtCO2.)),100*round((Electricity.Heat..MtCO2. / lag(Electricity.Heat..MtCO2.)) -1,2),0),0)) %>%
      mutate(Manufacturing.Construction..MtCO2. = ifelse(!is.na(Manufacturing.Construction..MtCO2.),ifelse(!is.na(lag(Manufacturing.Construction..MtCO2.)),100*round((Manufacturing.Construction..MtCO2. / lag(Manufacturing.Construction..MtCO2.)) -1,2),0),0)) %>%
      mutate(Transportation..MtCO2. = ifelse(!is.na(Transportation..MtCO2.),ifelse(!is.na(lag(Transportation..MtCO2.)),100*round((Transportation..MtCO2. / lag(Transportation..MtCO2.)) -1,2),0),0)) %>%
      mutate(Other.Fuel.Combustion..MtCO2e. = ifelse(!is.na(Other.Fuel.Combustion..MtCO2e.),ifelse(!is.na(lag(Other.Fuel.Combustion..MtCO2e.)),100*round((Other.Fuel.Combustion..MtCO2e. / lag(Other.Fuel.Combustion..MtCO2e.)) -1,2),0),0)) %>%
      filter(year == input$year[2])
  })
  
  outputText = 'World'
  makeReactiveBinding("outputText")
  observeEvent(input$Clicked, {
    outputText <<- paste0(input$Clicked)
  })
  
  side <- reactive({
    data_fake %>% 
      filter(year <= input$year[2]) %>%
      filter(year >= input$year[1]) %>%
      group_by(country) %>%
      arrange(year) %>%
      filter(country == outputText)
  })
  
  ClickFunction <- JS("function(event) {Shiny.onInputChange('Clicked', event.point.name);}")
  
  # Text string of selected years for plot subtitle -----------------
  selected_years_to_print <- reactive({
    if(input$year[1] == input$year[2]) { 
      as.character(input$year[1])
    } else {
      paste(input$year[1], " - ", input$year[2], " , ", input$type)
    }
  })
  
  # Highchart -------------------------------------------------------
  output$hcontainer <- renderHighchart({
    colors <- c("#008000","#32CD32","#00FA9A",'#FFFFFF' = 0.1, "yellow", "orange", "red")

    hc <- hcmap(mapData = JS("Highcharts.maps['custom/world-palestine-highres']"), data=gas(),value = input$type, joinBy = c("hc-key", "code"), name = "Percentage Change", borderColor = "#FAFAFA", borderWidth = 0.5,
                tooltip = list(valueDecimals = 0, valuePrefix = "", valueSuffix = "% MTCO2", backgroundColor = "blue"), events = list(click = ClickFunction), cursor = 'pointer') %>% 
      
      hc_chart(backgroundColor = "#87CEFA") %>% 
      hc_colorAxis(stops = color_stops(n=length(colors), colors = colors), max=100, min=-100) %>%  
      hc_title(text = "A Brave New World",
               style = list(fontWeight = "bold")) %>% 
      hc_subtitle(text = paste("Percentage Change Between,",
                               selected_years_to_print())) %>%
      hc_credits(enabled = TRUE, 
                 text = "Sources: CAIT Country GHG Emissions",
                 style = list(fontSize = "14px")) %>%
      hc_mapNavigation(enabled = TRUE) 

    # Print highchart -----------------------------------------------
    hc
  })
  
  output$j <- renderHighchart({
    hc_side <- highchart() %>% 
      hc_title(text=outputText) %>%
      hc_xAxis(categories = side()$year, min=0) %>% 
      #hc_add_series(name = "Total", data = round(side()$value,2)) %>% 
      hc_add_series(name = "Energy", data = round(side()$Energy..MtCO2e.,2), valueSuffix = "% MTCO2") %>% 
      hc_add_series(name = "Industrial Processes", data = round(side()$Industrial.Processes..MtCO2e.,2)) %>% 
      hc_add_series(name = "Agriculture", data = round(side()$Agriculture..MtCO2e.,2)) %>% 
      hc_add_series(name = "Waste", data = round(side()$Waste..MtCO2e.,2)) %>% 
      hc_add_series(name = "Land-Use/Forestry", data = round(side()$Land.Use.Change.and.Forestry..MtCO2.,2)) %>% 
      hc_add_series(name = "Bunker Fuels", data = round(side()$Bunker.Fuels..MtCO2.,2)) %>% 
      hc_add_series(name = "Electricity/Heat", data = round(side()$Electricity.Heat..MtCO2.,2)) %>% 
      hc_add_series(name = "Manufacturing/Construction", data = round(side()$Manufacturing.Construction..MtCO2.,2)) %>% 
      hc_add_series(name = "Transportation", data = round(side()$Transportation..MtCO2.,2)) %>% 
      hc_add_series(name = "Other", data = round(side()$Other.Fuel.Combustion..MtCO2e.,2))

    
    # Print highchart -----------------------------------------------
    hc_side
  })

}

# Run app -----------------------------------------------------------
shinyApp(ui = ui, server = server)