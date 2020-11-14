
library(shiny)
source("utilities.R")

shinyUI(fluidPage(
    
    tags$head(tags$script(HTML(JS.logify))),
    tags$head(tags$script(HTML(JS.onload))),

    # Application title
    titlePanel("Understanding ROC and AUC"),
    br(),
    
    fluidRow(
        column(3, align = "center",
               br(),
               br(),
               sliderInput("mean_pos",
                           "Mean of positive class:",
                           min = 0,
                           max = 100,
                           value = 100), 
               
               sliderInput("sd_pos",
                           "Standard deviation of positive class:",
                           min = 0,
                           max = 100,
                           value = 20),
               
               sliderInput("mean_neg",
                           "Mean of negative class:",
                           min = 0,
                           max = 100,
                           value = 60), 
               
               sliderInput("sd_neg",
                           "Standard deviation of negative class:",
                           min = 0,
                           max = 100,
                           value = 20),
               
               sliderInput("n", "Set total sample size:",
                           min = 2, max = 5, value = 3, step = 1),
               
               sliderInput("PropPositive",
                           "Set proportion of positive class:",
                           min = 0.01,
                           max = 0.99,
                           value = 0.5)
               
               ),
        
        column(9, align = "center",
               fluidRow(plotlyOutput("distribution_plot")),
               fluidRow(
                   column(6,
                          plotlyOutput("ROC_plot")),
                   column(6,
                          sliderInput("Threshold",
                                      "Set threshold:",
                                      min = 0,
                                      max = 100,
                                      value = 80),
                          actionButton("optimal", "Set to optimal threshold"),
                          br(), br(), br(), br(),
                          tableOutput("summary")
                          )
                   )
               )
        
    )
    
))
