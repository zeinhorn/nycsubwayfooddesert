library(shiny)
library(tidyverse)
library(rvest)
library(geojsonio)
library(leaflet)
#Reading in the data sets
medianIncomes <- read_csv("MedianIncomes.csv")
nhoods <- geojson_read("CommunityDistricts.geojson",
                       what = "sp")
parks <- read_csv("WalkingDistanceToAPark.csv")

#Getting median incomes ready
nhoods.copy <- nhoods
medianIncomesFiltered <- medianIncomes %>%
  filter(`Household Type` == "All Households" &
           TimeFrame == 2019) %>%
  select(Location, Data, Fips)

nhoods.copy@data$boro_cd <- as.numeric(nhoods.copy@data$boro_cd)

#Joining neighborhoods and income; boro_cd = fips = neighborhood codes
nhoods.copy@data <- left_join(nhoods.copy@data,
                              medianIncomesFiltered,
                              by = c("boro_cd" = "Fips"))
nhoods.copy@data <- nhoods.copy@data %>%
  rename("Income" = "Data")

#leaflet
map <- nhoods.copy %>%
  leaflet()

#bins = income brackets
bins <- c(0,50000, 75000, 100000,150000, 200000, Inf)
colors <- colorBin(palette = "YlOrRd",
                   domain = nhoods.copy@data$Income,
                   bins = bins)

ui <- fluidPage(
  leafletOutput(outputId = "nyc_cd")
  
)

server <- function(input, output, session) {
  output$nyc_cd <- renderLeaflet({
    map %>% 
      addPolygons(fillColor = ~colors(Income),
                  color = "black",
                  opacity = 1,
                  fillOpacity = .7,
                  layerId = ~Location) %>%
      setView(-74, 40.7, 10) %>% 
      addLegend(pal = colors,
                values = nhoods.copy@data$Income)
  })
  
}

shinyApp(ui, server)