library(shiny)
library(tidyverse)

medianIncomes <- read_csv("Median Incomes.csv")
library(rvest)
library(geojsonio)
library(leaflet)


#med.income <- read.csv(file.choose())
nhoods <- geojson_read(file.choose(),
                       what = "sp")
nhoods.data <- nhoods
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