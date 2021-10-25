library(shiny)
library(XML)
library(xml2)
source('analyzer.R')
source('xml_helpers.R')
source('data.R')
source('table.R')
source('plots.R')

# Define server logic
shinyServer(function(input, output, session) {
    
    validate_input <- function() {
        validate(
            need(!((input$graphDependentVar == "adg") & (input$graphDay == "0")), ""),
            need(!((input$meansDependentVariable == "adg") & (input$meansDay == "0")), "")
        )
    }
    
    observe({
        graph_var <- input$graphDependentVar
        graph_day <- input$graphDay
        table_var <- input$meansDependentVariable
        table_day <- input$meansDay
        
        if ((graph_var == "adg") & (graph_day == "0")) {
            updateSelectInput(session, "graphDay", selected = "14")
        }
        
        if ((table_var == "adg") & (table_day == "0")) {
            updateSelectInput(session, "meansDay", selected = "14")
        }
    })
    
    graph_model <- reactive({
        model <- Model$new(variable = input$graphDependentVar,
                          day = input$graphDay)
        model$fit_model()
        return(model)
    })
    
    table_model <- reactive({
        model <- Model$new(variable = input$meansDependentVariable,
                           day = input$meansDay)
        model$fit_model()
        return(model)
    })

    output$result_plot <- renderPlot({
        validate_input()
        means <- graph_model()$get_means()
        plt <- plot_means(df = means,
                         x_var = means$trt,
                         y_var = means$lsmean,
                         xlabel = "GAA Inclusion, g/d",
                         ylabel = input$graphDependentVar,
                         error = means$SE)
        return(plt)
    })
    
    output$measurement_table <- renderUI({
        validate_input()
        return(create_table(measurements, tbl_width = 0.75))
    })
    
    # Data tables for model, ANOVA, and means
    output$fit <- renderTable({
        validate_input()
        return(graph_model()$get_model_summary())
    })
    
    output$anova <- renderTable({
        validate_input()
        return(graph_model()$get_anova_table())
    })
    
    output$data <- renderDataTable({
        validate_input()
        return(table_model()$get_means())
    })
    
    output$pairwise <- renderTable({
        validate_input()
        anova <- graph_model()$get_anova_table()
        if (anova$`Pr(>F)`[1] <= 0.05) {
            return(graph_model()$get_pairwise())
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
