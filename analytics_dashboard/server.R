library(shiny)
library(XML)
library(xml2)
source('analyzer.R')
source('xml_helpers.R')
source('data.R')
source('table.R')

# Define server logic
shinyServer(function(input, output, session) {
    

    model <- reactive({
        model <- Model$new(variable = input$graphDependentVar,
                          day = input$graphDay)
        model$fit_model()
        return(model)
    })
    
    observe({
        selected_var <- input$meansDependentVariable
        selected_day <- input$meansDay
        
        updateSelectInput(session,
                          inputId = "graphDependentVar",
                          selected = selected_var)
        updateSelectInput(session,
                          inputId = "graphDay",
                          selected = selected_day)
    })
    

    output$result_plot <- renderPlot({
        means <- model()$get_means()
        plt <- plot_means(df = means,
                         x_var = means$Trt,
                         y_var = means$lsmean,
                         xlabel = "Treatment",
                         ylabel = input$graphDependentVar,
                         error = means$SE)
        return(plt)
    })
    
    output$measurement_table <- renderUI({
        return(create_table(measurements, tbl_width = 0.75))
    })
    
    # Data tables for model, ANOVA, and means
    output$fit <- renderTable({
        return(model()$get_model_summary())
    })
    
    output$anova <- renderTable({
        return(model()$get_anova_table())
    })
    
    output$data <- renderDataTable({
        return(model()$get_means())
    })
    
    output$pairwise <- renderTable({
        anova <- model()$get_anova_table()
        if (anova$`Pr(>F)`[1] <= 0.05) {
            return(model$get_pairwise())
        } else {
            return("Since the ANOVA F test was not significant (the P 
                   value in the anova table), we will not separate the means.")
        }
    })
    
    # Strings from XML resource file
    output$main_title <- renderText({
        text <- get_string('title')
        return(text)
    })
    
    output$overview <- renderText({
        text <- get_string('overview')
        return(text)
    })
    
    output$overview_b <- renderText(({
        text <- get_string('overviewb')
        return(text)
    }))
    
    output$model_description <- renderText({
        return(get_string('modeldesc'))
    })
    
    output$anova_description <- renderText({
        return(get_string('anovadesc'))
    })
    
    output$lsmeans_description <- renderText({
        return(get_string('lsmeansdesc'))
    })
})
