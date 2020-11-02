library(shiny)
library(tidyverse)

medianIncomes <- read_csv("MedianIncomes.csv")

ui <- fluidPage(
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)