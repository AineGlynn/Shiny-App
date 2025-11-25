library(shiny)
library(tidyverse)
library(readr)
library(ggplot2)
library(plotly)
library(survival)
library(survminer)

dig.df <- read_csv("DIG.csv")

dig.df <- dig.df %>%
  select(ID, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT, DIABP, SYSBP, HYPERTEN, CVD, WHF, DIG, HOSP, HOSPDAYS, DEATH, DEATHDAY) %>%
  mutate(SEX = factor(SEX, 
                      levels = c(1, 2), 
                      labels = c('Male', 'Female')), 
         TRTMT = factor(TRTMT,
                        levels = c(1,0),
                        labels = c('Treatment', 'Placebo')),
         Death_Month = round(DEATHDAY/30)) 



ui <- fluidPage(
  titlePanel("DIG Data Exploration"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("plotType","Plot Type:",
                   c(Barchart = "bar", Boxplot = "box")),
      
      conditionalPanel(
        condition = "input.plotType = 'box'",
        selectInput(inputId = "SEX", 
                    label = "Select Gender:",
                    choices = c("Male", "Female"),
                    multiple = TRUE,
                    selected = c("Male", "Female"))),
      
      conditionalPanel(
        condition = "input.plotType = 'bar'",
        sliderInput("BMI",
                    "BMI Range:",
                    min = 14,
                    max = 65,
                    value = c(20, 50))),

      #checkboxGroupInput(inputId = "TRTMT", label = "Treatment Group" , choices = c("Treatment", "Placebo"), selected = NULL),
      #selectInput(inputId = "SEX", label = "Select Gender:", choices = c("Male", "Female"), multiple = TRUE, selected = c("Male", "Female")),
      #sliderInput("BMI", "BMI Range:", min = 14, max = 65, value = c(20, 50)),
      #changing the age so that a range can be selected
      sliderInput("AGE", "Participant Age:", min = 20, max = 95, value = c(30, 60)),
      sliderInput("Death_Month", "Follow up time in months:", min = 0, max = 60, value = c(0,10), animate = TRUE)
    ),
    mainPanel(
      plotOutput("plot1", click = "plot1_click"),
      plotOutput("plot2", click = "plot2_click"),
      plotOutput("plot3"),
      dataTableOutput("table1"),
      dataTableOutput("table2")
    )
  ),
)
  


server <- function(input, output) {
  
  
  DIG_sub <- reactive({
    dig.df %>%
      filter(SEX %in% input$SEX) %>%
      filter(AGE >= input$AGE[1] & AGE <= input$AGE[2]) %>%
      filter(BMI >= input$BMI[1] & BMI <= input$BMI[2]) %>%
      #filter(TRTMT %in% input$TRTMT)
      filter(Death_Month >= input$Death_Month[1] & Death_Month <= input$Death_Month[2])
  })
  
  output$plot1 <- renderPlot({ 
    ggplot(data = DIG_sub(),aes(x=DIABP,y=SYSBP)) + 
      geom_point(colour = 'magenta4') +
      theme_minimal() 
  })
 
  #static plot is there a way to improve it?
  output$plot2 <- renderPlot({ 
    ggplot(DIG_sub(), aes(x = TRTMT)) +
      geom_bar(colour = 'black', fill = c('darkorchid', 'darkblue'), alpha = 0.6, width = 0.4) +
      labs(title = 'Number of Patients per Treatment Group',
           x = 'Treatment Group',
           y = 'Total') +
      theme_minimal()
  })
  #survival function
  surv_func <- reactive({
    survfit(as.formula(paste("Surv(Death_Month, DEATH)~", paste(1))), data = DIG_sub())})
  
  #still working on it
  output$plot3 <- renderPlot({
    #digfit = survfit(as.formula(paste("Surv(Death_Month, DEATH)~", paste(1))), data = DIG_sub())
    #ggsurvplot(surv_func(), data = DIG_sub())
    plot(surv_func()) # need to improve the plot
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

#Trying to make a conditional graph: 
if (interactive()) {
  ui <- fluidPage(
  sidebarPanel(
    selectInput("plotType", "Plot Type",
                c(Bar = "Bar", Boxplot = "Boxplot")
    ),
    conditionalPanel(
      condition = 'input.plotType == 'Boxplot'', 
      selectInput(
        'breaks', 'Breaks', 
        c('AGE', 'BMI', '[Custom]' = 'custom')
      ), 
    )