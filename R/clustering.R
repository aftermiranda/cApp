library(shiny)
library(ggplot2)

clusteringUI <- function(id){
  fluidRow(
    column(
      width = 3,
      sliderInput("cluster_k", "K-value for clustering", value = 1, min = 1, max = 10),
    ),
    column(
      width = 9,
      plotOutput(NS(id, "cluster_plot"))
    )
  )
}

clusteringServer <- function(id){
  moduleServer(id, function(input, output, session){
    output$cluster_plot <- renderPlot({ plot_k(input$cluster_k) })
  })
}
