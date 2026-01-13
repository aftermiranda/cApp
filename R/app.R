library(shiny)

sampleApp <- function() {
  ui <- navbarPage(
    "Sample app",
    tabPanel("Histogram", histogramUI("tab1"))
  )

  server <- function(input, output, session) {
    histogramServer("tab1")
  }

  shinyApp(ui, server)
}
