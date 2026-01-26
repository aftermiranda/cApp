

summaryUI <- function(id){
  fluidPage(
    titlePanel("Statistics on Confidence in Canadian Institutions"),
    fluidRow(
      column(4, imageOutput(NS(id, "sum_img"))),
      column(8, includeHTML("summary.html"))
    )
  )
}

summaryServer <- function(id){
  moduleServer(id, function(input, output, session) {
    output$sum_img <- renderImage(
      {
        list(src="education_plot.png", width="100%")
      },
    deleteFile = FALSE,
    )

  })
}
