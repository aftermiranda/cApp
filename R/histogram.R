# histogram
library(shiny)
library(ggplot2)
library(DT)

# Define UI for module that draws a histogram
histogramUI <- function(id) {
  fluidRow(
    column(4,
           selectInput(NS(id,"gender"), "Gender", choices = unique(confidence$gender), selected = NULL),
           selectInput(NS(id,"var2"), "Y Variable", choices = unique(confidence$sociodemographics)),
           numericInput(NS(id,"bins"), "bins", value = 10, min = 1)),
    column(8, dataTableOutput(NS(id, "table1")),
           textOutput(NS(id, "selected")))
  )
}

# Define Server for module to draw histogram
histogramServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$selected <- renderText({input$gender})
    if(observe(input$gender)){
      data <- reactive(confidence[which(confidence$gender == input$gender),])
      output$table1 <- renderDataTable({datatable(data)})
    }
  })
}
