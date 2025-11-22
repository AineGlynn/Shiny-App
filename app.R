library(shiny)
library(tidyverse)
library(readr)

dig.df <- read_csv("DIG.csv")


ui <- fluidPage(
  titlePanel("DIG Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "Treatment", label = "Treatment Group" , choices = c("Treatment", "Placebo"), selected = NULL)
    ),
    mainPanel(
      
    )
  )
)

server <- function(input, output) {
  
}

shinyApp(ui, server)

ui <- fluidPage(
  titlePanel("DIG Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "Treatment", label = "Treatment Group" , choices = c("Treatment", "Placebo"), selected = NULL)
    ),
    mainPanel(
      
    )
  )
)

server <- function(input, output) {
  
}

shinyApp(ui, server)