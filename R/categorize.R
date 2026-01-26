library(shiny)
library(ggplot2)

# UI for categorization functions
categorizeUI <- function(id){
  fluidRow(
    column(
      width = 4,
      tags$div(
        p("This categorization will attempt to determine the ranking based on the data you select."),
        hr()
      ),

      # user input for categorization methods
      radioButtons(NS(id, "user_function"),
                   "Categorization Method:",
                   choices = c("Classification Tree" = "class_tree",
                               "Multinomial Regression" = "mult_reg",
                               "Random Forest" = "rand_for")),

      # user input for statistic
      radioButtons(NS(id, "stat_item"),
                   "Selected Statistic:",
                   choices = c("Percentage of persons" = "percent",
                               "Number of persons" = "populate")),

      # user input for gender
      checkboxGroupInput(NS(id, "gender"),
                         "Genders to Include:",
                         choices = c("Total, all persons",
                                     "Men", "Women"),
                         selected = "Total, all persons"),

      # user input for other variables to include
      checkboxGroupInput(NS(id, "variables"),
                         "Variables to Include:",
                         choices = c("Sociodemographics" = "sociodemographics",
                                     "Institution" = "institution",
                                     "Month" = "month",
                                     "Year" = "year")
                         ),
    ),
    column(
      width = 8,

      # print plot of categorization
      plotOutput(NS(id, "category_plot")),
      hr(),
      includeHTML('categorize.html')
    )
  )
}

categorizeServer <- function(id){
  moduleServer(id, function(input, output, session){

    output$category_plot <- renderPlot({ cat_plot(input$user_function,
                                                  input$stat_item,
                                                  input$gender,
                                                  input$variables) })
  })
}
