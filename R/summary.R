

summaryUI <- function(id){
  fluidPage(
    titlePanel("Statistics on Confidence in Canadian Institutions"),
    fluidRow(
      column(3),
      column(9, includeHTML("R/summary.html"))
    )
  )
}

summaryServer <- function(id){
  moduleServer(id, function(input, output, session) {

  })
}
