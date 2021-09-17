library(shiny)
library(shinythemes)

# Define UI for application
shinyUI(fluidPage(title = "ASI 560 Analytics Dashboard",
  tags$head(
    includeCSS('www/css/main_page.css'),
    tags$link(rel = "shortcut icon",
              href = "images/cow.png")
  ),
  titlePanel(textOutput("main_title")),
  navbarPage( title = "",
              # Home page
              tabPanel(title = icon("home"),
                       div(
                         h3("Overview"),
                         p(textOutput('overview')),
                         div(class = "center-content",
                             p("Figure 1: Creatine and energy metabolism"),
                           img(src = 'images/creatine.png',
                               width = 250,
                               height = 250)
                           ),
                         p(
                           a("Previous research",
                             href = 'https://www.sciencedirect.com/science/article/pii/S1751731120001603'),
                           textOutput('overview_b', inline = TRUE)
                         )
                       ),
                       div(
                         h3("Measurements"),
                         div(id = "measurementTable",
                           uiOutput('measurement_table'))
                       )),
              
              # Page for graph
              tabPanel(title = "Graph",
                       div(
                         fluidRow(
                           # Place selectInputs in a row
                           column(6,
                                  selectInput(inputId = 'graphDependentVar',
                                              label = h3("Select a response"),
                                              choices = list("ADG" = "ADG035",
                                                             "ADFI" = "ADFId035",
                                                             "G:F" = "GF035"))),
                           column(6,
                                  selectInput(inputId = 'graphDay',
                                              label = h3("Select a day"),
                                              choices = list("0" = "0",
                                                             "0 - 14" = "14",
                                                             "0 - 28" = "28",
                                                             "0 - 42" = "42")))
                         ),
                         plotOutput("result_plot")
                       ),
                       div(class="center-content",
                           h3("Model Summary"),
                           tableOutput("fit"),
                           h4("What does this mean?"),
                           p(textOutput("model_description"))),
                       div(class="center-content",
                         h3("Analysis of Variance"),
                         tableOutput("anova"),
                         h4("What does this mean?"),
                         p(textOutput('anova_description'))
                       ),
                       div(class = "center-content",
                           h3("Pairwise Comparisons"),
                           tableOutput('pairwise'))
              ),
              # Page for data table (means of model)
              tabPanel(title = "Data Table",
                       h3("Least squares means"),
                       fluidRow(
                         column(6,
                                selectInput(inputId = "meansDependentVariable",
                                            label = h3("Select a response"),
                                            choices = list(
                                              "ADG" = "ADG035",
                                              "ADFI" = "ADFId035",
                                              "G:F" = "GF035"
                                            ))),
                         column(6,
                                selectInput(inputId = "meansDay",
                                            label = h3("Select a day"),
                                            choices = list(
                                              "0" = "0",
                                              "0 - 14" = "14",
                                              "0 - 28" = "28",
                                              "0 - 42" = "42"
                                            )))
                       ),
                       div(style = "width: 100%;",
                         dataTableOutput('data')
                       ),
                       h4("What does this mean?"),
                       p(textOutput('lsmeans_description')))
  )
))
