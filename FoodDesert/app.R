library(shiny)
library(tidyverse)

medianIncomes <- read_csv("MedianIncomes.csv")
library(rvest)
library(geojsonio)
library(leaflet)

nhoods <- geojson_read("NYCNeighborhoods.geojson",
                       what = "sp")
nhoods.copy <- nhoods
medianIncomesFiltered <- medianIncomes %>%
  filter(`Household Type` == "All Households" &
         TimeFrame == 2019) %>%
  select(Location, Data)

nhoods.copy@data <- left_join(nhoods.copy@data,
                              medianIncomesFiltered,
                              by = c("ntaname" = "Location"))
nhoods.copy@data <- nhoods.copy@data %>%
  rename("Income" = "Data")
  

map <- nhoods.data %>%
  leaflet()

#we might need to make the data as.numeric before graphing it

ui <- fluidPage(
  leafletOutput(outputId = "leaflet1")
  
  
  
)

server <- function(input, output, session) {
  output$leaflet1 <- renderLeaflet
  
}

shinyApp(ui, server)