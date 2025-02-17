library(shiny)
library(shinyjs)
library(tidyverse) # Includes ggplot and dplyr
library(maps) # Map data for states and counties
library(mapproj) # For a range of map projections: mercator, albers, mollweide, gilbert...
library(plotly)
library(skimr)
library(dplyr)
library(tidyr)
library(ggiraph)
library(htmlwidgets)
library(readxl)
library(maps)
library(ggplot2)
library(rworldmap)
library(sf)
library(patchwork)
library(leaflet)
library(shinythemes)
rm(list = ls())


data <- data.frame('GDP per capita' = c(10, 0, 1, 6, 3, 8, 6, 4, 3),
                   'Imports' = c(10, 0, 2, 1, 4, 6, 9, 5, 9),
                   'Exports' = c(10, 0, 1, 4, 5, 4, 2, 9, 8), 
                   'Inflation' = c(10, 0, 6, 2, 1, 2, 4, 3, 2),
                   'Unemployment' = c(10, 0, 7, 3, 1, 9, 2, 3, 5),
                   row.names = c("max", "min", "India", "China", "Japan", "Germany", "France", "US", "UK"))

Colors_line <- c('darkgray', 'gold', 'tomato', 'royalblue', 'darkgreen', 'purple', 'pink')

# Sample dataset
global_indicators <- read_excel("P_Data_Extract_From_World_Development_Indicators.xlsx") %>%
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.integer(str_extract(Year, "\\d{4}")))
data2<- read_excel("currency.xlsx")
usd_index <- read_excel("R dataset.xlsx")
# Renaming columns and duplicating the EURO column
usd_index <- usd_index %>%
  rename(
    Japan = JAPANESE,
    India = Indian,
    UK = Pound,
    China = Chinese
  ) %>%
  mutate(
    Germany = EURO,
    France = EURO
  )

series_names <- unique(global_indicators$Series_Name)
country_names <- unique(global_indicators$Country_Name)



data_given <- read_excel("currency.xlsx")

data_longer <- pivot_longer(data_given, cols = c("Yen", "Rupee", "Yuan", "Pound", "USDX","EURO"), names_to = "Country", values_to = "Currency")
currencu_sel <- unique(data_longer$Country)
india.sf <- st_as_sf(map('world', 'india', plot = FALSE, fill = TRUE))
india.sf <- india.sf %>% mutate(country = "India")

japan.sf <- st_as_sf(map('world', 'japan', plot = FALSE, fill = TRUE))
japan.sf <- japan.sf %>% mutate(country = "Japan")

china.sf <- st_as_sf(map('world', 'china', plot = FALSE, fill = TRUE))
china.sf <- china.sf %>% mutate(country = "China")

uk.sf <- st_as_sf(map('world', 'uk', plot = FALSE, fill = TRUE))
uk.sf <- uk.sf %>% mutate(country = "UK")

USA.sf <- st_as_sf(map('world','USA',plot= FALSE,fill= TRUE))
USA.sf <- USA.sf %>% mutate(country="USA")

european_countries <- c("Spain", "France", "Germany", "Italy", "Portugal", "Netherlands", "Belgium", "Switzerland", "Austria", "Luxembourg")
europe.sf <- st_as_sf(map('world', european_countries, plot = FALSE, fill = TRUE))  # Define europe.sf here
europe.sf <- europe.sf %>% mutate(country = "european_countries")

# Define UI for application that draws a histogram
ui <-
  
  navbarPage(
    
    
    title = "Dynamic Duality",
    theme = shinytheme("cerulean"),
    tabPanel("Currency",
             fluidRow(
               h4("Countries we Selected and their weightage in Calculating USDX:" ),
               leafletOutput("combined_maps"),
               
               h4(" Select the Currency and the Time period in order to compare various Currencies"),
               column(3,checkboxGroupInput("selectedCountries", "Select Currency", choices = unique(data_longer$Country), selected = unique(data_longer$Country))),
               
               column(9,sliderInput("timePeriod", "Select Time Period:",
                                    min = as.Date("1998-01-01"), max = as.Date("2022-01-01"),
                                    value = c(as.Date("1998-01-01"), as.Date("2022-01-01")),
                                    step = 1, timeFormat = "%Y-%m-%d")),
               h5("USDX:The U.S. dollar index (USDX) is a measure of the value of the U.S. dollar relative to a basket of foreign currencies. The six currencies included in the USDX are often referred to as America's most significant trading partners"),
               
               plotOutput("plot_Z")
             )
    ),
    tabPanel("Economic Factors",
             fluidRow(
              # Added width argument
              column(4, h5("Graph1"),
               h6("Select the currency for Various Currency against US Dollar Graph"),
               selectInput("selectedCurrency", "Select Currency", choices = colnames(data2)[-1], selected = colnames(data2)[-1]),
               h6("")),
              column(4, h5("Graph 2"),
               h6("The below inputs provide the economic indicator of the country Selected the the Country plot."),
               selectInput("selected_series", "Eocnomic Indicator", choices = unique(global_indicators$Series_Name)),
               selectInput("selected_country", "Country", choices = unique(global_indicators$Country_Name),
               h5("The Contribution of Each country Based on each Indicator"))),
              column(4, h5("Economic Indicators for 7 Different Countries"),plotlyOutput("radarPlot")),
             ),
               
             
              plotlyOutput("line_plot"),
             
              plotlyOutput("combined_plot"),
             
             
             
             
    )
  )



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$combined_maps <- renderLeaflet({
    world_map <- leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 30, zoom = 2)
    
    # Add the maps to the world map
    com_map <- world_map %>%
      addPolygons(data = india.sf, fillColor = "blue", group = "India", 
                  popup = ~paste("Indian Rupee(INR), 4.2% weight"), weight = 1,color="black") %>%
      addPolygons(data = japan.sf, fillColor = "green", group = "Japan",
                  popup = ~paste("Japanese yen (JPY), 13.6% weight"), weight = 1,color="black") %>%
      addPolygons(data = china.sf, fillColor = "red", group = "China",
                  popup = ~paste("Chinese (Yuan), 9.1% weight"), weight = 1,color="black") %>%
      addPolygons(data = uk.sf, fillColor = "purple", group = "UK",
                  popup = ~paste("Pound sterling (GBP), 11.9% weight"), weight = 1,color="black") %>%
      addPolygons(data = europe.sf, fillColor = "orange", group = "Europe",
                  popup = ~paste(" Euro (EUR), 57.6% weight"), weight = 1,color="black") %>% 
      addPolygons(data = USA.sf, fillColor = "grey", group = "USA",
                  popup = ~paste("USDX"), weight = 1,color="black")  
      
    
    
    
    return(com_map)
  })
  
  
  output$ plot_Z <- renderPlot({
    
    selected_countries <- input$selectedCountries
    
    
    
    filtered_data <- data_longer[data_longer$Country %in% selected_countries &
                                   data_longer$YEAR >= input$timePeriod[1] &
                                   data_longer$YEAR <= input$timePeriod[2],
    ]
    
    ggplot(filtered_data) +
      geom_line(aes(x = YEAR,y =Currency,color=Country,group=Country),alpha=5)+
      scale_y_log10()+
      labs( x = "YEAR", y = "CURRENCY") +
      theme_bw()
    
    
    
  })
  
  
  output$radarPlot <- renderPlotly({
    p <- plot_ly(
      data = data.frame(
        theta = rep(colnames(data), each = nrow(data)),
        r = as.vector(t(data)),
        text = rep(rownames(data), times = ncol(data))
      ),
      type = 'scatterpolar',
      mode = 'lines',
      line = list(width = 4, color = Colors_line),
      hoverinfo = 'text',
      text = ~text,
      theta = ~theta,
      r = ~r
    ) %>% 
      layout(
        polar = list(
          radialaxis = list(visible = TRUE, range = c(0, 10)),
          angularaxis = list(rotation = 90)
        ),
        legend = list(traceorder = 'normal'),
        margin = list(r = 10, l = 0), 
        height = 200,  # Adjust the height as needed
        width = 300
        
      )
    
    p
  })
  
  output$combined_plot <- renderPlotly({
    # Filter the data for the selected series and country
    filtered_data <- global_indicators %>%
      filter(Series_Name == input$selected_series & Country_Name == input$selected_country)
    
    # Convert Value to numeric
    filtered_data$Value <- as.numeric(filtered_data$Value)
    
    # Create the interactive bar graph
    bar_plot <- ggplot(filtered_data, aes(x = as.factor(Year), y = Value, fill = Series_Name, text = paste(Value))) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_bw() +
      theme(legend.position = "none")+
      xlab("Year") +  # Customize X-axis label
      ylab("Economic Indicator")+
      ggtitle(input$selected_series)+theme(plot.title = element_text(hjust = 0.5))
      # Customize Y-axis label
       
    
    # Convert ggplot to plotly
    bar_plot <- ggplotly(bar_plot, tooltip = "text")
    
    return(bar_plot)
    
  })
  output$line_plot <- renderPlotly({
    
    selected_currency <- input$selectedCurrency
    
    
    # Create an interactive plot using plot_ly
    plot1 <- plot_ly(data = data2, x = ~YEAR, y = ~get(selected_currency), name = selected_currency, type = 'scatter', mode = 'lines', line = list(color = selected_currency)) %>%
      layout(title = 'Various Currency against US Dollar', xaxis = list(title = 'Year'), yaxis = list(title = 'CURRENCY'), showlegend = TRUE) %>%
      config(displayModeBar = FALSE)
   
    plot1
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)













































