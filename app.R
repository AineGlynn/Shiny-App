library(shiny)
library(tidyverse)
library(readr)
library(ggplot2)
library(plotly)
library(survival)
library(survminer)

dig.df <- read_csv("DIG.csv")
dig.df <- dig.df %>%
  select(ID, WHF, STRK, MI, DIABETES, ANGINA, HYPERTEN, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT, DIABP, SYSBP, DIG, HOSP, HOSPDAYS, DEATH, DEATHDAY) %>%
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
                   c(scatterplot = "scatter", Barchart = "bar")),

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
      selectInput(inputId = "SEX", label = "Select Gender:", choices = c("Male", "Female"), multiple = TRUE, selected = c("Male", "Female")),
      sliderInput("BMI", "BMI Range:", min = 14, max = 65, value = c(20, 50)),#changing the age so that a range can be selected
       checkboxInput("WHF", "Worsening Heart Failure", TRUE),  verbatimTextOutput("value"),
       checkboxInput("STRK", "Stroke", TRUE),  verbatimTextOutput("value"),
       checkboxInput("MI", "Heart Attack", TRUE),  verbatimTextOutput("value"),
       checkboxInput("DIABETES", "Diabetes", TRUE),  verbatimTextOutput("value"),
       checkboxInput("ANGINA", "Angina", TRUE),  verbatimTextOutput("value"),
       checkboxInput("HYPERTEN", "Hypertension", TRUE),  verbatimTextOutput("value"),
    #  checkboxInput("CVD", "CVD", TRUE),  verbatimTextOutput("value"),
      sliderInput("AGE", "Participant Age:", min = 20, max = 95, value = c(30, 60)),
      sliderInput("Death_Month", "Follow up time in months:", min = 0, max = 60, value = c(0,10), animate = TRUE),
      actionButton("plotBtn", "Plot"),
      textOutput("result"),
    ),
    mainPanel(
      plotlyOutput("plot"),
      plotOutput("plot2", click = "plot2_click"),
      plotOutput("plot3"),
      dataTableOutput("table1"),
      dataTableOutput("table2")
    )
  )
)


server <- function(input, output, session) {
  filteredData <- reactive({
    subset(dig.df, AGE >= input$AGE[1] & AGE <= input$AGE[2])
  })
  rv <- reactiveValues(sliderValue = NULL, buttonClicked = NULL)

  # Observe slider input not working yet:
  observeEvent(input$sumbit, {
    output$result <- renderText({"Thank You"
    })
  })
  #React for plot 2
  plot2react <- reactive({
    dig.df %>%
      filter(WHF %in% input$WHF) %>%
      filter(STRK %in% input$STRK) %>%
      filter(DIABETES %in% input$DIABETES) %>%
      filter(ANGINA %in% input$ANGINA) %>%
      filter(MI %in% input$MI)
  })
  
  DIG_sub <- reactive({
    dig.df %>%
      filter(SEX %in% input$SEX) %>%
      filter(AGE >= input$AGE[1] & AGE <= input$AGE[2]) %>%
      filter(BMI >= input$BMI[1] & BMI <= input$BMI[2]) %>%
      filter(Death_Month >= input$Death_Month[1] & Death_Month <= input$Death_Month[2])
  })

  generate_bar <- function() {
    df <- plot2react() %>%
      count(TRTMT)
    plot_ly(data = df,
            x = ~TRTMT,
            y = ~n,
            type = "bar",
            color = ~TRTMT) %>% 
      layout(title = "Treatment Group")
  }

  #survival function
  surv_func <- reactive({
    survfit(as.formula(paste("Surv(Death_Month, DEATH)~", paste(1))), data = DIG_sub())})

  output$plot <- renderPlotly({
    generate_bar()
  })
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

#Adding boxplots/ barcharts:


#1. I want to change the slider options so make a dataset that exactly matches these
#2. it should just compare against tretament group.
#3. ideally it is a b


# ui <- fluidPage(
#   titlePanel("DIG Data Exploration"),
#   sidebarLayout(
#     sidebarPanel(
#       radioButtons("plotType","Plot Type:",
#                    c(scatterplot = "scatter", Barchart = "bar")),
#       
#       conditionalPanel(
#         condition = "input.plotType = 'box'",
#         selectInput(inputId = "SEX",
#                     label = "Select Gender:",
#                     choices = c("Male", "Female"),
#                     multiple = TRUE,
#                     selected = c("Male", "Female"))),
#       
#       conditionalPanel(
#         condition = "input.plotType = 'bar'",
#         sliderInput("BMI",
#                     "BMI Range:",
#                     min = 14,
#                     max = 65,
#                     value = c(20, 50))),
#       
#       #checkboxGroupInput(inputId = "TRTMT", label = "Treatment Group" , choices = c("Treatment", "Placebo"), selected = NULL),
#       selectInput(inputId = "SEX", label = "Select Gender:", choices = c("Male", "Female"), multiple = TRUE, selected = c("Male", "Female")),
#       sliderInput("BMI", "BMI Range:", min = 14, max = 65, value = c(20, 50)),#changing the age so that a range can be selected
#       checkboxInput("WHF", "Worsening Heart Failure", TRUE),  verbatimTextOutput("value"),
#       checkboxInput("STRK", "Stroke", TRUE),  verbatimTextOutput("value"),
#       checkboxInput("MI", "Heart Attack", TRUE),  verbatimTextOutput("value"),
#       checkboxInput("DIABETES", "Diabetes", TRUE),  verbatimTextOutput("value"),
#       checkboxInput("ANGINA", "Angina", TRUE),  verbatimTextOutput("value"),
#       checkboxInput("HYPERTEN", "Hypertension", TRUE),  verbatimTextOutput("value"),
#       #  checkboxInput("CVD", "CVD", TRUE),  verbatimTextOutput("value"),
#       sliderInput("AGE", "Participant Age:", min = 20, max = 95, value = c(30, 60)),
#       sliderInput("Death_Month", "Follow up time in months:", min = 0, max = 60, value = c(0,10), animate = TRUE),
#       actionButton("plotBtn", "Plot"),
#       textOutput("result"),
#     ),
#     mainPanel(
#       plotOutput("plot1", click = "plot1_click"),
#       plotOutput("plot2", click = "plot2_click"),
#       plotOutput("plot3"),
#       dataTableOutput("table1"),
#       dataTableOutput("table2")
#     )
#   )
# )
# 
# 
# server <- function(input, output, session) {
#   filteredData <- reactive({
#     subset(dig.df, AGE >= input$AGE[1] & AGE <= input$AGE[2])
#   })
#   rv <- reactiveValues(sliderValue = NULL, buttonClicked = NULL)
#   
#   # Observe slider input not working yet:
#   observeEvent(input$sumbit, {
#     output$result <- renderText({"Thank You"
#     })
#   })
#   #React for plot 2
#   plot2react <- reactive({
#     dig.df %>%
#       filter(SEX %in% input$SEX) %>%
#       filter(AGE >= input$AGE[1] & AGE <= input$AGE[2]) %>%
#       filter(BMI >= input$BMI[1] & BMI <= input$BMI[2]) %>%
#       filter(WHF %in% input$WHF) %>%
#       filter(STRK %in% input$STRK) %>%
#       filter(DIABETES %in% input$DIABETES) %>%
#       filter(ANGINA %in% input$ANGINA) %>%
#       filter(MI %in% input$MI) %>%
#       filter(HYPERTEN %in% input$HYPERTEN) %>%
#       # filter(CVD %in% input$CVD) %>%
#       filter(Death_Month >= input$Death_Month[1] & Death_Month <= input$Death_Month[2])
#   })
#   
#   DIG_sub <- reactive({
#     dig.df %>%
#       filter(SEX %in% input$SEX) %>%
#       filter(AGE >= input$AGE[1] & AGE <= input$AGE[2]) %>%
#       filter(BMI >= input$BMI[1] & BMI <= input$BMI[2]) %>%
#       filter(Death_Month >= input$Death_Month[1] & Death_Month <= input$Death_Month[2])
#   })
#   
#   
#   
#   
#   output$plot1 <- renderPlot({
#     plot_data <- filteredData()
#     if(input$plotType == 'box'){
#       ggplot(data = DIG_sub(),aes(x =TRTMT)) +
#         geom_boxplot(colour = 'magenta4') +
#         theme_minimal()
#     } else {
#       
#       #static plot is there a way to improve it?
#       generate_bar <- function() {
#         df <- plot2react() %>%
#           count(TRTMT)
#         plot_ly(data = df,
#                 x = ~TRTMT,
#                 y = ~n,
#                 type = "bar",
#                 color = ~TRTMT) %>% 
#           layout(title = "Treatment Group")
#       }
#     }
#   })
#   
#   #survival function
#   surv_func <- reactive({
#     survfit(as.formula(paste("Surv(Death_Month, DEATH)~", paste(1))), data = DIG_sub())})
#   
#   #still working on it
#   output$plot3 <- renderPlot({
#     #digfit = survfit(as.formula(paste("Surv(Death_Month, DEATH)~", paste(1))), data = DIG_sub())
#     #ggsurvplot(surv_func(), data = DIG_sub())
#     plot(surv_func()) # need to improve the plot
#   })
#   
#   #clicks:
#   output$click_info1 <- renderPrint({
#     input$plot1_click
#   })
#   output$click_info2 <- renderPrint({
#     input$plot2_click
#   })
#   
#   # Tables
#   output$table1 <- renderDataTable({req(input$plot1_click)
#     nearPoints(DIG_sub(), input$plot1_click)})
#   output$table2 <- renderDataTable({req(input$plot2_click)
#     nearPoints(DIG_sub(), input$plot2_click)})
# }
# 
# 
# shinyApp(ui, server)