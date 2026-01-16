# histogram
library(shiny)
library(ggplot2)

# Define UI for module that draws a boxplots for various factors
histogramUI <- function(id) {
  fluidRow(
    column(4,
           selectInput(NS(id,"gender"), "Gender", choices = gender_choices, selected = NULL),
           selectInput(NS(id,"so_demo"), "Sociodemographic statistic", choices = unique(confidence$sociodemographics), selected = NULL),
           checkboxGroupInput(NS(id,"institute"), "Institution", choices = unique(confidence$institution), selected = NULL),
           selectInput(NS(id,"stat_item"), "Results by:", choices = unique(confidence$statistics), selected = NULL)
    ),
    column(8, plotOutput(NS(id, "plot")))
  )
}

# Define Server for module to draw boxplots
histogramServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$selected <- renderText({input$gender})

    plt_data <- reactive(get_data(input$gender, input$so_demo, input$institute, input$stat_item))
    output$plot <- renderPlot({
      get_plot(plt_data(), input$gender, input$stat_item)
      })
  })
}
