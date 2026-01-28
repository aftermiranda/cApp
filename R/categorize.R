library(shiny)
library(ggplot2)

# UI for categorization functions
categorizeUI <- function(id){
  fluidRow(
    column(
      width = 4,

      # user input for categorization methods
      selectInput(NS(id, "user_function"),
                  "Categorization Method:",
                  choices = c("Classification Tree" = "class_tree",
                              "Multinomial Regression" = "mult_reg",
                              "Random Forest" = "rand_for")),

      # user input for statistic
      selectInput(NS(id, "stat_item"),
                  "Selected Statistic:",
                  choices = c("Percentage of persons" = "percent",
                              "Number of persons" = "populate")),

      # user input for gender
      selectInput(NS(id, "gender"),
                  "Genders to Include:",
                  choices = c("Total",
                              "Men", "Women"),
                  selected = "Total"),

      # user input for sociodemographics
      selectizeInput(NS(id, "socioSelect"),
                     "Sociodemographics:",
                     choices = unique(confidence_fact$sociodemographics),
                     multiple = TRUE,
                    ),

      # user input for other variables to include
      checkboxGroupInput(NS(id, "variables"),
                         "Variables to Include:",
                         choices = c("Institution" = "institution",
                                     "Month" = "month",
                                     "Year" = "year")
                         ),
      uiOutput(NS(id, "institutions")),
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
  thematic::thematic_shiny()

  moduleServer(id, function(input, output, session){

    output$institutions <- renderUI({
      if("institution" %in% input$variables){
        checkboxGroupInput(NS(id, "instSelect"),
                           "Specific Institutions:",
                           choices = unique(confidence_fact$institution))
      }
    })

    output$category_plot <- renderPlot({ cat_plot(input$user_function,
                                                  input$stat_item,
                                                  input$gender,
                                                  input$variables,
                                                  input$socioSelect,
                                                  input$instSelect) })
  })
}
