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





#Aine's Plot of Basline Characteristics: 
dig2 = dig.df %>%
  mutate(SEX = factor(SEX, 
                      levels = c(1, 2), 
                      labels = c('Male', 'Female')), 
         TRTMT = factor(TRTMT,
                        levels = c(0, 1),
                        labels = c('Placebo', 'Treatment')))

ui <- fluidPage(
  titlePanel("DIG Trial"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "SEX", label = "Select Gender:", choices = c("Male", "Female"), multiple = TRUE),
      sliderInput("BMI", "BMI Range:", min = 14, max = 65, value = c(20, 50)),
      sliderInput("AGE", "Participant Age:", min = 20, max = 95, value = 30, step = 1, animate = FALSE)
      
    ),
    mainPanel(
      plotOutput("plot1", click = "plot1_click"),
      plotOutput("plot2", click = "plot2_click"),
      dataTableOutput("table1"),
      dataTableOutput("table2")
    )
  )
  
)

server <- function(input, output) {
  
  DIG_sub <- reactive({
    dig2 %>%
      filter(SEX %in% input$SEX) %>%
     filter(AGE == input$AGE) %>%
    filter(BMI >= input$BMI[1] & BMI <= input$BMI[2])
  })
  
  output$plot1 <- renderPlot({ 
    ggplot(data = DIG_sub(),aes(x=DIABP,y=SYSBP)) + 
      geom_point(colour = 'magenta4') +
      theme_minimal() 
  })
 
  output$plot2 <- renderPlot({ 
    ggplot(DIG_sub(), aes(x = TRTMT)) +
      geom_bar(colour = 'black', fill = c('darkorchid', 'darkblue'), width = 0.4) +
      labs(title = 'Number of Patients per Treatment Group',
           x = 'Treatment Group',
           y = 'Total') +
      theme_bw()
  })
#clicks:
  output$click_info1 <- renderPrint({
    input$plot1_click
  })
  output$click_info2 <- renderPrint({
    input$plot2_click
  })  
   
  # Tables
  output$table1 <- renderDataTable({req(input$plot1_click)
    nearPoints(DIG_sub(), input$plot1_click)})
  output$table2 <- renderDataTable({req(input$plot2_click)
    nearPoints(DIG_sub(), input$plot2_click)})
}
   
 
shinyApp(ui, server)

