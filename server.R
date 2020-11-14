
library(ggplot2)
library(plotly)
library(pROC)
library(tidyr)
library(MLmetrics)
library(dplyr)
library(shiny)
library(DT)

shinyServer(function(input, output, session) {
    
    # Dynamically update the threshold slider min and max
    observe({
        
        range <- func_threshold_range(input$mean_pos, input$mean_neg, input$sd_pos, input$sd_neg)
        if (input$Threshold < range[1] || input$Threshold > range[2]) {
            val <- mean(range[1], range[2])
        } else {
            val <- input$Threshold
        }
        
        updateSliderInput(session, "Threshold", value = val,
                          min = range[1], max = range[2])

    })
    
    empiricalData <- reactive({
        create_empirical_data(input$n, input$PropPositive, input$mean_pos, input$mean_neg, input$sd_pos, input$sd_neg)
    })
    
    theoreticalData <- reactive({
        create_theoretical_dist_data(input$mean_pos, input$mean_neg, input$sd_pos, input$sd_neg)
    })
    
    theoreticalROCData <- reactive({
        create_theoretical_ROC_data(input$mean_pos, input$mean_neg, input$sd_pos, input$sd_neg)
    })
        
    output$distribution_plot <- renderPlotly({
        
        plot_distributions(theoreticalData(), empiricalData(), input$Threshold)
        
    })
    
    observeEvent(input$optimal, {
        opt <- optimal_threshold(theoreticalROCData())
        updateSliderInput(session, "Threshold", value = opt$x)
    })
    
    output$ROC_plot <- renderPlotly({
        
        plot_ROC(theoreticalROCData(), empiricalData(), input$Threshold)
        
    })
    
    accuracy_r <- reactive({
        accuracy(input$mean_pos, input$mean_neg, input$sd_pos, input$sd_neg, input$Threshold)
    })
    
    auc <- reactive({
        calc_auc(theoreticalROCData())
    })
    
    gini <- reactive({
        calc_gini(theoreticalROCData())
    })
    
    output$summary <- renderTable({
        result_table(auc(), gini(), accuracy_r())
                      # options = list(paging = FALSE, 
                      #                searching = FALSE,
                      #                autoWidth = TRUE,
                      #                info = FALSE)
                      # )
    })

})
