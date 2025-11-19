library(shiny)
library(tidyverse)
library(readr)

dig.df <- read_csv("DIG.csv")


ui <- fluidPage(
  titlePanel("DIG Data Explorer")
)

server <- function(input, output) {
  
}

shinyApp(ui, server)

