library(shiny)
library(bslib)


sampleApp <- function() {
  ui <- fluidPage(
    theme = bs_theme(bootswatch = "cosmo"),
    "Institutional Confidence in Canada",
    navset_tab(
      nav_panel("Summary", summaryUI("tab1")),
      nav_panel("Box Plots", histogramUI("tab2")),
      nav_panel("Categorization", categorizeUI("tab3"))
  ))

  server <- function(input, output, session) {
    summaryServer("tab1")
    histogramServer("tab2")
    categorizeServer("tab3")
  }

  shinyApp(ui, server)
}
