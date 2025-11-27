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
      sliderInput("BMI", "BMI Range:", min = 14, max = 65, value = c(20, 50)),
      #changing the age so that a range can be selected
      sliderInput("AGE", "Participant Age:", min = 20, max = 95, value = c(30, 60)),
      sliderInput("Death_Month", "Follow up time in months:", min = 0, max = 60, value = c(0,10), animate = TRUE),
      actionButton("plotBtn", "Plot"),
      textOutput("result"),
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

  DIG_sub <- reactive({
    dig.df %>%
      filter(SEX %in% input$SEX) %>%
      filter(AGE >= input$AGE[1] & AGE <= input$AGE[2]) %>%
      filter(BMI >= input$BMI[1] & BMI <= input$BMI[2]) %>%
      #filter(TRTMT %in% input$TRTMT)
      filter(Death_Month >= input$Death_Month[1] & Death_Month <= input$Death_Month[2])
  })

  output$plot1 <- renderPlot({
    plot_data <- filteredData()
    if(input$plotType == 'scatter'){
       ggplot(data = DIG_sub(),aes(x =DIABP,y=SYSBP)) +
      geom_point(colour = 'magenta4') +
      theme_minimal()
    } else {

  #static plot is there a way to improve it?
    ggplot(DIG_sub(), aes(x = TRTMT)) +
      geom_bar(colour = 'black', fill = c('darkorchid', 'darkblue'), alpha = 0.6, width = 0.4) +
      labs(title = 'Number of Patients per Treatment Group',
           x = 'Treatment Group',
           y = 'Total') +
      theme_minimal()
  }
  })

  output$plot2 <- renderPlot({
    ggplot(DIG_sub(), aes(y = TRTMT)) +
      geom_boxplot(fill = "deeppink", colour = 'black', na.rm = TRUE) +
      labs(title = "Treatment Group",
           y = "Treatment Group") +
      theme_classic()
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

#Adding boxplots/ barcharts:

#Gender/ Mortaility/ Race:
# output$plotGender <- renderPlot({
#   ggplot(dig.df, aes(x = TRTMT, fill = SEX)) +
#     geom_bar(position = position_dodge(), colour = 'black')
#   labs(title = "Gender of Patients in comparison to Treatment Group",
#        x = "Gender", y = "Treatment Group", fill = "Gender") +
#     theme_classic()
# })

#1. I want to change the slider options so make a dataset that exactly matches these
#2. it should just compare against tretament group. 
#3. ideally it is a boxplot not heatmap

# ui <- fluidPage(
#   titlePanel("Baseline Characteristics:"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("TRTMT", "Treatment Group:",
#                   choices = names(dig.df)),
#       selectInput("Variables", "Variable to Compare:",
#                   choices = names(dig.df)),
#       actionButton("refresh", "Refresh Plot")
#     ),
#     mainPanel(
#       plotlyOutput("plot")
#     )
#   )
# )
# 
# server <- function(input, output, session) {
# 
#   generate_boxplot <- function() {
#     plot <- plot_ly(
#       x = dig.df[, input$TRTMT],
#       y = dig.df[, input$Variables],
#       type = "box",
#     ) %>% layout(title = input$plot_title)
#     return(plot)
#   }
# 
# 
#   output$plot <- renderPlotly({
#     generate_boxplot()
#   })
# 
#   # Refresh plot on button click
#   observeEvent(input$refresh, {
#     output$plot <- renderPlotly({
#       generate_boxplot()
#     })
#   })
# 
#   }
#   shinyApp(ui = ui, server = server)
# 
#   ui <- fluidPage(
#     titlePanel("Baseline Characteristics:"),
#     sidebarLayout(
#       sidebarPanel(
#         selectInput("TRTMT", "Treatment Group:",
#                     choices = names(dig.df)),
#         selectInput("Variables", "Variable to Compare:",
#                     choices = names(dig.df)),
#         actionButton("refresh", "Refresh Plot")
#       ),
#       mainPanel(
#         plotlyOutput("plot")
#       )
#     )
#   )
#   
#   server <- function(input, output, session) {
#     output$boxPlot <- renderPlotly({
#       plot_ly(
#         data = dig.df,
#         x = ~dig.df[[input$groupVar]],
#         y = ~dig.df[[input$numVar]],
#         type = "box"
#       ) %>%
#         layout(
#           title = input$plot_title,
#           xaxis = list(title = input$groupVar),
#           yaxis = list(title = input$numVar)
#         )
#     })
#     
#     output$groupCounts <- renderTable({
#       grp <- dig.df[[input$groupVar]]
#       
#       # Get all unique groups (e.g., Treatment, Placebo)
#       all_groups <- sort(unique(grp))
#       
#       # Count non-NA numeric values per group
#       counts <- tapply(
#         dig.df[[input$numVar]],
#         grp,
#         function(x) sum(!is.na(x))
#       )
#       
#       # Ensure all groups appear even if count is 0
#       counts_full <- data.frame(
#         Group = all_groups,
#         Count = counts[all_groups] %||% 0   # fallback to 0
#       )
#       
#       counts_full
#     })  
#     
#     # Refresh plot on button click
#     observeEvent(input$refresh, {
#       output$plot <- renderPlotly({
#         generate_heatmap()
#       })
#     })
#     
#     
#     counts_full <- data.frame(
#       Group = all_groups,
#       Count = ifelse(is.na(counts[all_groups]), 0, counts[all_groups])
#     )      
#       
#   }
#   shinyApp(ui = ui, server = server)

