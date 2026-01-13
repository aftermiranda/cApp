# histogram


# Define UI for module that draws a histogram
histogramUI <- function(id) {
  taglist(
    selectInput(NS(id,"var"), "Variable", choices = names(mtcars)),
    numericInput(NS(id,"bins"), "bins", value = 10, min = 1),
    plotOutput(NS(id, "hist"))
  )
}

# Define Server for module to draw histogram
histogramServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(mtcars[[input$var]])
    output$hist <- renderPlot({
      hist(data(), breaks = input$bins, main = input$var)
    }, res = 96)
  })
}
