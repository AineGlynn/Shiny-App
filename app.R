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
  titlePanel("DIG Trial"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "SEX", label = "Select Gender:", choices = c("Male", "Female"), multiple = TRUE),
      sliderInput("BMI", "BMI Range:", min = 14, max = 65, value = c(20, 50)),
      sliderInput("AGE", "Participant Age:", min = 20, max = 95, value = 30, step = 1, animate = FALSE)
      
    ),
    mainPanel(
      plotOutput("plot1"),
      dataTableOutput("table1")
    )
  )
  
)

server <- function(input, output) {
  
  DIG_sub <- reactive({
    dig.df %>%
      # filter(SEX == input$SEX) %>%
      filter(AGE == input$AGE)
  })
  
  output$plot1 <- renderPlot({ 
    ggplot(data = DIG_sub(),aes(x=DIABP,y=SYSBP)) + 
      geom_point(colour = 'magenta4') +
      theme_minimal() 
  })
  
  output$table1 <- renderDataTable({ 
    DIG_sub()
  })
  
}

shinyApp(ui, server)