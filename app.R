library(shiny)
library(tidyverse)
library(readr)
library(ggplot2)
library(plotly)
library(survival)
library(survminer)
library(bslib)

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
  navset_tab( 
    #create tabs for different aspects of data exploration
    nav_panel("Baseline", p("Plots of Baseline Characteristics",
                            titlePanel("DIG Baseline Characteristics"),
                            sidebarLayout(
                              sidebarPanel(
                                checkboxGroupInput(inputId = "TRTMT", label = "Treatment Group" , choices = c("Treatment", "Placebo"), selected = c("Treatment", "Placebo")),
                                selectInput(inputId = "SEX", label = "Select Gender:", choices = c("Male", "Female"), multiple = TRUE, selected = c("Male", "Female")),
                                sliderInput("BMI", "BMI Range:", min = 14, max = 65, value = c(20, 50)),
                                sliderInput("AGE", "Participant Age:", min = 20, max = 95, value = c(30, 60))
                                
                                ),
                              mainPanel(
                                plotlyOutput("plot")
                              ))
                            )), 
    nav_panel("Outcomes", p("Plots of Outcome Variable",
                            titlePanel("Outcomes of DIG in different categories"),
                            sidebarLayout(
                              sidebarPanel(
                                checkboxGroupInput(inputId = "TRTMT", label = "Treatment Group" , choices = c("Treatment", "Placebo"), selected = c("Treatment", "Placebo")),
                                selectInput(inputId = "SEX", label = "Select Gender:", choices = c("Male", "Female"), multiple = TRUE, selected = c("Male", "Female")),
                                checkboxInput("WHF", "Worsening Heart Failure", TRUE),  verbatimTextOutput("value"),
                                checkboxInput("STRK", "Stroke", TRUE),  verbatimTextOutput("value"),
                                checkboxInput("MI", "Heart Attack", TRUE),  verbatimTextOutput("value"),
                                checkboxInput("DIABETES", "Diabetes", TRUE),  verbatimTextOutput("value"),
                                checkboxInput("ANGINA", "Angina", TRUE),  verbatimTextOutput("value"),
                                checkboxInput("HYPERTEN", "Hypertension", TRUE),  verbatimTextOutput("value")
                              ),
                              mainPanel(
                                plotlyOutput("plot4")
                              )
                            )
                            )), 
    nav_panel("Survival", p("Survival Plots",
                            titlePanel("Survival Plots of Patients"),
                            sidebarLayout(
                              sidebarPanel(
                                checkboxGroupInput(inputId = "TRTMT", label = "Treatment Group" , choices = c("Treatment", "Placebo"), selected = c("Treatment", "Placebo")),
                                selectInput(inputId = "SEX", label = "Select Gender:", choices = c("Male", "Female"), multiple = TRUE, selected = c("Male", "Female")),
                                sliderInput("Death_Month", "Follow up time in months:", min = 0, max = 60, value = c(0,10), animate = TRUE)
                              ),
                              mainPanel(
                                plotOutput("plot3")
                              )
                            )
                            
                            )), 
    nav_menu( 
      "Other links", 
      nav_panel("D", "Panel D content"), 
      "----", 
      "Description:", 
      nav_item( 
        a("Shiny", href = "https://shiny.posit.co", target = "_blank") 
      ), 
    ), 
  ), 
  id = "tab" 
  
  #   mainPanel(
  #     #plotlyOutput("plot"),
  #     plotlyOutput("plot2"),
  #     plotlyOutput("plot4"),
  #     dataTableOutput("table1"),
  #     dataTableOutput("table2")
  #   )
  # )
)

server <- function(input, output, session) {
  filteredData <- reactive({
    subset(dig.df, AGE >= input$AGE[1] & AGE <= input$AGE[2])
  })
  rv <- reactiveValues(sliderValue = NULL, buttonClicked = NULL)


  #Boxplot: BMI
  plotBMIreact <- reactive({
    dig.df %>%
      filter(TRTMT == input$TRTMT) %>%
      filter(BMI >= input$BMI[1] & BMI <= input$BMI[2])
  })
  #Boxplot: AGE
  plotAGEreact <- reactive({
    dig.df %>%
      filter(TRTMT == input$TRTMT) %>%
      filter(AGE >= input$AGE[1] & AGE <= input$AGE[2])
  })

  #React for plot 2
  plot2react <- reactive({
    dig.df %>%
      filter(TRTMT == input$TRTMT) %>%
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

  generate_BMIboxplot <- function () {
    df1 <- plotBMIreact()
    plot_ly(data = df1,
              x = ~TRTMT,
              y = ~BMI,
              type = "box",
              color = ~TRTMT) %>%
      layout(title = "Treatment Group")

  }

  generate_AGEboxplot <- function () {
    df1 <- plotAGEreact()
    plot_ly(data = df1,
            x = ~TRTMT,
            y = ~AGE,
            type = "box",
            color = ~TRTMT) %>%
      layout(title = "Treatment Group")

  }

  generate_bar <- function() {
    df2 <- plot2react() %>%
      count(TRTMT)
    plot_ly(data = df2,
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

  output$plot2 <- renderPlotly({
    generate_BMIboxplot()
  })

  output$plot4 <- renderPlotly({
    generate_AGEboxplot()
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


#1. Have boxplot -unsure if working 
#2. will else work as they are on different scales (counts and range of age/ bmi)
#3. Else is not working/ 



  # generate_bar <- function() {
  #   if(input$plotType == 'bar'){
  #   df <- plot2react() %>%
  #     count(TRTMT)
  #   plot_ly(data = df,
  #           x = ~TRTMT,
  #           y = ~n,
  #           type = "bar",
  #           color = ~TRTMT) %>%
  #     layout(title = "Treatment Group")
  # } else {
  # generate_box <- function(){
  #   df <- DIG_sub() %>%
  #   plot_ly(data = df,
  #           x = ~TRTMT,
  #           y = ~BMI,
  #           type = "box",
  #           color = ~TRTMT) %>%
  #     layout(title = "Treatment Group")
  # }