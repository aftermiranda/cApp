library(shiny)
library(bslib)


sampleApp <- function() {
  ui <- fluidPage(
    theme = bs_theme(bootswatch = "lux"),
    "Institutional Confidence in Canada",
    navset_tab(
      nav_panel("Summary", summaryUI("tab1")),
      nav_panel("Box Plots", histogramUI("tab2")),
      nav_panel("Clustering", clusteringUI("tab3"))
  ))

  server <- function(input, output, session) {
    summaryServer("tab1")
    histogramServer("tab2")
    clusteringServer("tab3")
  }

  shinyApp(ui, server)
}
