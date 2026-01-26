theme_update(plot.title = element_text(hjust=0.5, size=20),
             plot.subtitle = element_text(hjust=0.5, size=15),
             panel.background = element_rect(fill = "white"),
             axis.line = element_line(color = "darkgrey"),
             panel.grid.major = element_line(color="darkgrey"),
             panel.grid.minor = element_line(color="lightgrey"))


#' Reduce data based on chosen parameters
#'
#' @param gender male, female or total depending on user input
#' @param socio_demo sociodemographic selection from user
#' @param statistic input statistic from user indicating percent or population
#'
#' @returns reduced
#' @export
#'
#' @examples
#' d1 <- get_data(gender = "Women", so_demo = "Visible minority population", stat_item = "Percentage of persons")
#' head(d1)
#' # A tibble: 6 × 5
#'  institution ranks  month year  value
#'  <chr>       <chr>  <chr> <chr> <dbl>
#'1 Police      1 or 2 04    2022   15.1
#'2 Police      1 or 2 10    2022   17.1
#'3 Police      1 or 2 04    2023   15.2
#'4 Police      1 or 2 10    2023   14.4
#'5 Police      1 or 2 07    2024   14.7
#'6 Police      1 or 2 10    2024   16.1

get_data <- function(gender=NULL, so_demo=NULL, institute=NULL, stat_item=NULL){
  # Reduce data based on the selected statistic
  if(!is.null(stat_item)){
    if(stat_item == "Percentage of persons"){
      reduced <- percentages
    } else{
      reduced <- populations
    }
  }else{
    reduced <- confidence
  }

  # if gender is selected by user, reduce data
  if(!is.null(gender)){
    values <- reduced[,which(names(reduced) == gender)]
    reduced <- reduced[,-which(names(reduced) %in% gender_choices)]
    reduced <- cbind(reduced, values[,1])
    colnames(reduced)[which(names(reduced) == gender)] <- "value"
  }

  # if sociodemographic item is selected by user, reduce data
  if(!is.null(so_demo)){
    reduced <- reduced[which(reduced$sociodemographics == so_demo),]
    reduced <- reduced[, -which(names(reduced) %in% c("sociodemographics"))]
  }

  # if institution is selected by user, reduce data
  if(!is.null(institute)){
    if(length(institute) == 1){
      reduced <- reduced[which(reduced$institution == institute),]
    }
    if(length(institute) > 1){
      reduced <- reduced[which(reduced$institution %in% institute),]
    }
  }

  return(reduced)
}



#' Plotting function for confidence data giving yearly box plots
#'
#' @param df a dataframe containing confidence data as defined in get_data()
#'
#' @returns p1 a box plot with user specified values
#' @export
#'
#' @examples
get_plot <- function(df, gender, stat_item){
  # check how many institutions are selected
  inst <- unique(df$institution)

  # create general plot
  p1 <- ggplot(df) +
    geom_boxplot(aes(x=ranks, y=value, color=year)) +
    xlab("Selected Rankings") + ylab(stat_item) #+ labs(title = t1)

  # if multiple institutions are selected, create facet_wrap
  if(length(inst) > 1){
    p1 <- p1 + facet_wrap(~institution)
  }
  return(p1)
}
