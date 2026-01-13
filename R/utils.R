
#' Initial tab plot generator
#'
#' @param x
#' @param y
#'
#' @returns
#' @export
#'
#' @examples
histo_plot <- function(x,y){
  return(ggplot() + geom_bar(aes(x=x,y=y)) )
}
