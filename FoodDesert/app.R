library(shiny)
<<<<<<< Updated upstream
library(tidyverse)

medianIncomes <- read_csv("MedianIncomes.csv")
=======
library(rvest)
library(geojsonio)


med.income <- read.csv(file.choose())
nhoods <- geojson_read(file.choose(),
                       what = "sp")
nhoods2 <- nhoods
nhoods3 <- left_join(nhoods2,
                              med.income,
                              by = "")
#we might need to make the data as.numeric before graphing it
>>>>>>> Stashed changes

ui <- fluidPage(
  
  
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)