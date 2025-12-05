library(shiny)
library(tidyverse)
library(readr)
library(ggplot2)
library(plotly)
library(survival)
library(survminer)
library(bslib)
library(rsconnect)

dig.df <- read_csv("DIG.csv")
dig.df <- dig.df %>%
  select(ID, WHF, HOSP, STRK,CVD, MI, DIABETES, ANGINA, HYPERTEN, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT, DIABP, SYSBP, DIG, HOSP, HOSPDAYS, DEATH, DEATHDAY, CVD
         ) %>%
  mutate(SEX = factor(SEX, 
                      levels = c(1, 2), 
                      labels = c('Male', 'Female')), 
         HOSP = factor(HOSP, 
                      levels = c(0, 1), 
                      labels = c('Not Hospitalised', 'Hospitalised')),
         WHF = factor(WHF, 
                       levels = c(0, 1), 
                       labels = c('No WHF', 'WHF')),
         CVD = factor(CVD, 
                      levels = c(0, 1), 
                      labels = c('No CVD', 'CVD')),
         STRK = factor(STRK, 
                      levels = c(0, 1), 
                      labels = c('No Stroke', 'Stroke')),
         MI = factor(MI, 
                      levels = c(0, 1), 
                      labels = c('No Heart Attack', 'Heart Attack')),
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


dig2.df <- dig.df %>%
  mutate(DEATH = factor(DEATH,
                        levels = c(0,1),
                        labels = c("Alive", "Dead")))


ui <- fluidPage(
  navset_tab(
    #create tabs for different aspects of data exploration
    nav_panel("Baseline", p("Plots of Baseline Characteristics",
                            titlePanel("DIG Baseline Characteristics"),
                            sidebarLayout(
                              sidebarPanel(
                                #checkboxGroupInput(inputId = "TRTMT", label = "Treatment Group" , choices = c("Treatment", "Placebo"), selected = c("Treatment", "Placebo")),
                                selectInput(inputId = "SEX", label = "Select Gender:", choices = c("Male", "Female"), multiple = TRUE, selected = c("Male", "Female")),
                                sliderInput("BMI", "BMI Range:", min = 14, max = 65, value = c(20, 50)),
                                sliderInput("AGE", "Participant Age:", min = 20, max = 95, value = c(30, 60))
                                ),
                              mainPanel(
                                plotlyOutput("treatbar"),
                                plotlyOutput("agebox"),
                                plotlyOutput("bmibox")
                              ))
                            )),
    nav_panel("Outcomes", p("Plots of Outcome Variable",
                            titlePanel("Outcomes of DIG in different categories"),
                            sidebarLayout(
                              sidebarPanel(
                                #checkboxGroupInput(inputId = "TRTMT", label = "Treatment Group", choices = c("Treatment", "Placebo"), selected = c("Treatment", "Placebo")),
                                selectInput(inputId = "SEX", label = "Select Gender:", choices = c("Male", "Female"), multiple = TRUE, selected = c("Male", "Female")),
                                selectInput(inputId = "WHF", label = "Worsening Heart Failure:", choices = c("No WHF", "WHF"), multiple = FALSE, selected = "No WHF"),                            
                                selectInput(inputId = "STRK", label = "Stroke:", choices = c("No Stroke", "Stroke"), multiple = FALSE, selected = "No Stroke"),                            
                                selectInput(inputId = "MI", label = "Heart Attack:", choices = c("No Heart Attack", "Heart Attack"), multiple = FALSE, selected = "No Heart Attack"),                            
                                selectInput(inputId = "DIABETES", label = "Diabetes:", choices = c("No Diabetes", "Diabetes"), multiple = FALSE, selected = "No Diabetes"),                            
                                selectInput(inputId = "ANGINA", label = "Angina:", choices = c("No Angina", "Angina"), multiple = FALSE, selected = "No Angina"),                           
                                selectInput(inputId = "HYPERTEN", label = "Hypertension:", choices = c("No Hypertension", "Hypertension"), multiple = FALSE, selected = "No Hypertension"),  
                                selectInput(inputId = "CVD", label = "Cardiovascular Disease:", choices = c("No CVD", "CVD"), multiple = FALSE, selected = "No CVD")
                                #selectInput(inputId = "HOSP", label = "Hospitilization:", choices = c("Not Hospitalized", "Hospitalized"), multiple = FALSE, selected = "Not Hospitalized")                       
                              ),
                              
                                mainPanel(
                                plotlyOutput("hosp"),
                                plotlyOutput("mort")
                                
                                )
                            )
                            )),
    nav_panel("Survival", p("Survival Plots",
                            titlePanel("Survival Plots of Patients"),
                            sidebarLayout(
                              sidebarPanel(
                                #checkboxGroupInput(inputId = "TRTMT", label = "Treatment Group" , choices = c("Treatment", "Placebo"), selected = c("Treatment", "Placebo")),
                                selectInput(inputId = "SEX", label = "Select Gender:", choices = c("Male", "Female"), multiple = TRUE, selected = c("Male", "Female")),
                                sliderInput("Death_Month", "Follow up time in months:", min = 0, max = 60, value = c(0,10), animate = TRUE),
                                sliderInput("AGE", "Participant Age:", min = 20, max = 95, value = c(30, 60)),
                                checkboxGroupInput("CVD", "CVD", choices = c("CVD", "No CVD"), selected = c("CVD", "No CVD"))
                                
                              ),
                              mainPanel(
                                plotlyOutput("surv1"),
                                plotlyOutput("surv2"),
                                plotlyOutput("survbox"),
                                dataTableOutput("table1")
                              )
                            )

                            )),
    nav_menu(
      "The DIG Trial",
      nav_item(
        a("DIG Trial", href = "https://biolincc.nhlbi.nih.gov/studies/dig/")
      )
    )
    ),
  id = "tab",
  
  
  theme = bs_theme(bg = "white",
                   fg = "black",
                   primary = "#E69F00",
                   secondary = "#0072B2",
                   success = "#009E73",
                   base_font = font_google("Inter"),
                   code_font = font_google("JetBrains Mono"))
)

server <- function(input, output, session) {
  filteredData <- reactive({
    subset(dig.df, AGE >= input$AGE[1] & AGE <= input$AGE[2])
  })
  rv <- reactiveValues(sliderValue = NULL, buttonClicked = NULL)


  #Boxplot: BMI
  plotBMIreact <- reactive({
    dig.df %>%
      filter(SEX %in% input$SEX ) %>%
      filter(BMI >= input$BMI[1] & BMI <= input$BMI[2])
  })
  #Boxplot: AGE
  plotAGEreact <- reactive({
    dig.df %>%
      filter(SEX %in% input$SEX ) %>%
      filter(AGE >= input$AGE[1] & AGE <= input$AGE[2])
  })

  #React for plot count bar chart for each group
  plot2react <- reactive({
    dig.df %>%
      filter(SEX %in% input$SEX ) %>%
      filter(AGE >= input$AGE[1] & AGE <= input$AGE[2]) %>%
      filter(BMI >= input$BMI[1] & BMI <= input$BMI[2])
  })

  #Hospitalization React:
  hospReact <- reactive({
      dig2.df %>%
        filter(SEX %in% input$SEX) %>%
        filter(WHF == input$WHF)%>%
        filter(STRK == input$STRK) %>%
        filter(DIABETES == input$DIABETES) %>%
        filter(ANGINA == input$ANGINA) %>%
        filter(MI == input$MI) %>%
        filter(CVD == input$CVD)%>%
        filter(HYPERTEN == input$HYPERTEN)
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
     filter(AGE >= input$AGE[1] & AGE <= input$AGE[2]) %>%
     filter(SEX %in% input$SEX) %>%
     filter(CVD == input$CVD) 
 })

  death_filt <- reactive({
    dig2.df %>%
      filter(Death_Month >= input$Death_Month[1] & Death_Month <= input$Death_Month[2])%>%
      filter(AGE >= input$AGE[1] & AGE <= input$AGE[2]) %>%
      filter(SEX %in% input$SEX) %>%
      filter(CVD == input$CVD) 
  })
  
 #Baseline characterisitics plots
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
  
#Outcome Plots
  output$hosp <- renderPlotly({
    p<- hospReact() %>%
      ggplot(aes(x = TRTMT, fill = HOSP)) +
      geom_bar(alpha = 0.7) +
      scale_fill_manual(values = c("Not Hospitalised" = "lightgreen", "Hospitalised" = "orchid2"))+
      labs(title = "Hospitalisations between Treatment Groups",
           x = "Treatment",
           fill = "Hospitalisation")
      
    ggplotly(p)
  })
  
  #mortality
  output$mort <- renderPlotly({
    m <- hospReact() %>%
      ggplot(aes(x = TRTMT, fill = DEATH)) +
      scale_fill_manual(values = c("Dead" = "lightpink", "Alive" = "aquamarine2"))+
      geom_bar(alpha = 0.7)+
      labs(title = "Mortality between Treatment groups",
           x = "Treatment",
           fill = "Death")
    
    ggplotly(m)
  })
  
#Survival Plots
  output$surv1 <- renderPlotly({
    fit <- survfit(Surv(Death_Month,DEATH)~1,data=surv_filt())

    s1 <- ggsurvplot(fit,
                     data=surv_filt(),
                     size = 0.4,
                     palette = c("orchid2"),
                     title = "Survival Times of All Participants",
                     legend.title = "Strata", legend.labs = "All participants")
    
    ggplotly(s1[[1]])
  })

  output$surv2 <- renderPlotly({
    fit2 <- survfit(Surv(Death_Month,DEATH)~TRTMT,data=surv_filt())

    s2 <- ggsurvplot(fit2,
                     data=surv_filt(),
                     size = 0.4,
                     palette = c("coral","lightseagreen"),
                     title = "Survival Times in Treatment Groups",
                     legend.title = "Strata", 
                     legend.labs = c("Treatment","Placebo"))
    
    ggplotly(s2[[1]])
  })

  #followuptimes per group
  output$survbox <- renderPlotly({
    b<- death_filt() %>%
      ggplot(aes(x = DEATH, y = Death_Month, fill = TRTMT))+
      geom_boxplot(alpha = 0.6) + 
      scale_fill_manual(values = c("Treatment" = "chartreuse2", "Placebo" = "chocolate"))+
      labs(title = "Boxplot of Month in Death and Treatment groups", 
           fill = "Treatment",
           x = "Death Status", 
           y= "Follow up time (Months)")
    
    ggplotly(b)
  })



  # Tables
  output$table1 <- renderDataTable({ 
    surv_filt()
  })
}


shinyApp(ui, server)

#rsconnect::showLogs() 
#rsconnect::deployApp()
#rsconnect::deployApp(appName = "DIG Trial Dataset App")