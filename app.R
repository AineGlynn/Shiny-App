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
  select(ID, WHF, HOSP, STRK,CVD, MI, DIABETES, ANGINA, HYPERTEN, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT, DIABP, SYSBP, DIG, HOSP, HOSPDAYS, DEATH, DEATHDAY, CVD
         ) %>%
  mutate(SEX = factor(SEX, 
                      levels = c(1, 2), 
                      labels = c('Male', 'Female')), 
         HOSP = factor(HOSP, 
                      levels = c(0, 1), 
                      labels = c('Not Hospitalized', 'Hospitalized')),
         WHF = factor(WHF, 
                       levels = c(0, 1), 
                       labels = c('Not WHF', 'WHF')),
         CVD = factor(CVD, 
                      levels = c(0, 1), 
                      labels = c('No CVD', 'CVD')),
         STRK = factor(STRK, 
                      levels = c(0, 1), 
                      labels = c('No Stroke', 'Stroke')),
         MI = factor(MI, 
                      levels = c(0, 1), 
                      labels = c('No MI', 'MI')),
         DIABETES = factor(DIABETES, 
                      levels = c(0, 1), 
                      labels = c('No Diabetes', 'Diabetes')),
         ANGINA = factor(ANGINA, 
                      levels = c(0, 1), 
                      labels = c('No Angina', 'Angina')),
         HYPERTEN = factor(HYPERTEN, 
                      levels = c(0, 1), 
                      labels = c('No Hypertension', 'Hypertension')),
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
                                sliderInput("AGE", "Participant Age:", min = 20, max = 95, value = c(30, 60)),
                                # checkboxGroupInput(
                                #   "Worsening Heart Failure",
                                #   "WHF:",
                                #   choices = c("Not WHF", "WHF"),
                                #   selected = c("Not WHF","WHF")  # select all by default
                                # ),
                                #checkboxInput("WHF", "Worsening Heart Failure"),  verbatimTextOutput("value"),
                                # checkboxInput("STRK", "Stroke"),  verbatimTextOutput("value"),
                                # checkboxInput("MI", "Heart Attack"),  verbatimTextOutput("value"),
                                # checkboxInput("DIABETES", "Diabetes"),  verbatimTextOutput("value"),
                                # checkboxInput("ANGINA", "Angina"),  verbatimTextOutput("value"),
                                # checkboxInput("HYPERTEN", "Hypertension"),  verbatimTextOutput("value"),
                                # checkboxInput("CVD", "Cardiovascular Disease"),  verbatimTextOutput("value")

                                )),
                              mainPanel(
                                plotlyOutput("treatbar"),
                                plotlyOutput("agebox")
                              ))
                            )),
    nav_panel("Outcomes", p("Plots of Outcome Variable",
                            titlePanel("Outcomes of DIG in different categories"),
                            sidebarLayout(
                              sidebarPanel(
                                checkboxGroupInput(inputId = "TRTMT", label = "Treatment Group" , choices = c("Treatment", "Placebo"), selected = c("Treatment", "Placebo")),
                                selectInput(inputId = "SEX", label = "Select Gender:", choices = c("Male", "Female"), multiple = TRUE, selected = c("Male", "Female")),
                                selectInput(inputId = "WHF", label = "Worsening Heart Failure:", choices = c("No WHF", "WHF"), multiple = FALSE, selected = "No WHF"),                            
                                selectInput(inputId = "STRK", label = "Stroke:", choices = c("No Stroke", "Stroke"), multiple = FALSE, selected = "No Stroke"),                            
                                selectInput(inputId = "MI", label = "Heart Attack:", choices = c("No Heart Attack", "Heart Attack"), multiple = FALSE, selected = "No Heart Attack"),                            
                                selectInput(inputId = "DIABETES", label = "Diabetes:", choices = c("No Diabetes", "Diabetes"), multiple = FALSE, selected = "No Diabetes"),                            
                                selectInput(inputId = "ANGINA", label = "Angina:", choices = c("No Angina", "Angina"), multiple = FALSE, selected = "No Angina"),                           
                                selectInput(inputId = "HYPERTEN", label = "Hypertension:", choices = c("No Hypertension", "Hypertension"), multiple = FALSE, selected = "No Hypertension"),  
                                selectInput(inputId = "CVD", label = "Cardiovascular Disease:", choices = c("No CVD", "CVD"), multiple = FALSE, selected = "No CVD"),
                                selectInput(inputId = "HOSP", label = "Hospitilization:", choices = c("Not Hospitalized", "Hospitalized"), multiple = FALSE, selected = "Not Hospitalized")                        
                                mainPanel(
                                plotlyOutput("hospitalization"),
                                plotlyOutput("bmibox")#this needs to be moved above, just a placeholder
                              )
                            )
                            ))),
    nav_panel("Survival", p("Survival Plots",
                            titlePanel("Survival Plots of Patients"),
                            sidebarLayout(
                              sidebarPanel(
                                checkboxGroupInput(inputId = "TRTMT", label = "Treatment Group" , choices = c("Treatment", "Placebo"), selected = c("Treatment", "Placebo")),
                                selectInput(inputId = "SEX", label = "Select Gender:", choices = c("Male", "Female"), multiple = TRUE, selected = c("Male", "Female")),
                                sliderInput("Death_Month", "Follow up time in months:", min = 0, max = 60, value = c(0,10), animate = TRUE),
                                checkboxGroupInput("CVD", "CVD", choices = c("CVD", "No CVD"), selected = c("CVD", "No CVD"))
                              ),
                              mainPanel(
                                plotOutput("surv1"),
                                plotOutput("surv2"),
                                dataTableOutput("table1")
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
    )
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

  #React for plot count bar chart for each group
  plot2react <- reactive({
    dig.df %>%
      filter(TRTMT == input$TRTMT) #%>%
      # filter(WHF %in% input$WHF) %>%
      # filter(STRK %in% input$STRK) %>%
      # filter(DIABETES %in% input$DIABETES) %>%
      # filter(ANGINA %in% input$ANGINA) %>%
      # filter(MI %in% input$MI) %>%
      # filter(CVD %in% input$CVD)
  })

  #Hospitalization React:
  hospitalizationReact <- reactive({
      dig.df %>%
        filter(TRTMT == input$TRTMT) %>%
       filter(HOSP %in% input$HOSP)  %>%
       filter(WHF %in% input$WHF)%>%
         filter(STRK %in% input$STRK) %>%
         filter(DIABETES %in% input$DIABETES) %>%
        filter(ANGINA %in% input$ANGINA) %>%
         filter(MI %in% input$MI) %>%
         filter(CVD %in% input$CVD)
    })

  DIG_sub <- reactive({
    dig.df %>%
      filter(SEX %in% input$SEX) %>%
      filter(AGE >= input$AGE[1] & AGE <= input$AGE[2]) %>%
      filter(BMI >= input$BMI[1] & BMI <= input$BMI[2])
  })

 surv_filt <- reactive({
   dig.df %>%
     filter(Death_Month >= input$Death_Month[1] & Death_Month <= input$Death_Month[2])%>%
     filter(TRTMT == input$TRTMT) %>%
     filter(SEX %in% input$SEX) %>%
     filter(CVD == input$CVD)
 })

  generate_BMIboxplot <- function() {
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

#Hospitalization PLots:
  generate_hospitalization <- function() {
    df3 <- hospitalizationReact() %>%
      count(TRTMT)
    plot_ly(data = df3,
            x = ~TRTMT,
            y = ~n,
            type = "bar",
            color = ~TRTMT)
  }

  #PLOTS
  output$treatbar <- renderPlotly({
    generate_bar()
  })

  output$bmibox <- renderPlotly({
    generate_BMIboxplot()
  })

  output$agebox <- renderPlotly({
    generate_AGEboxplot()
  })
  output$hospitalization <- renderPlotly({
    generate_hospitalization()
  })

  # #survival function
  # surv_func <- reactive({
  #   survfit(as.formula(paste("Surv(Death_Month, DEATH)~", paste(1))), data = DIG_sub())})

# #survival plot
#   output$surv1 <- renderPlot({
#     #digfit = survfit(as.formula(paste("Surv(Death_Month, DEATH)~", paste(1))), data = DIG_sub())
#     #ggsurvplot(surv_func(), data = DIG_sub())
#     plot(surv_func()) # need to improve the plot
#   })

#Survival Plots
  output$surv1 <- renderPlot({
    fit <- survfit(Surv(Death_Month,DEATH)~1,data=surv_filt())

    ggsurvplot(fit,data=surv_filt(),pval=TRUE, palette = c("orchid2"), title = "Survival Times of All Participants")
  })

  output$surv2 <- renderPlot({
    fit2 <- survfit(Surv(Death_Month,DEATH)~TRTMT,data=surv_filt())

    ggsurvplot(fit2,data=surv_filt(),pval=TRUE, palette = c("lightblue", "hotpink"), title = "Survival Times in Treatment Groups")
  })





  # Tables
  output$table1 <- renderDataTable({ 
    surv_filt()
  })
}


shinyApp(ui, server)
