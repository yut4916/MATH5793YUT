#' Shiny App to Test Multivariate Normality
#'
#' @return interactive html shiny browser
#' @export
#'
#' @examples \dontrun{MATH5793YUT::shinyMVN()}
#' @importFrom ggplot2 ggplot aes geom_point geom_abline geom_text geom_hline geom_vline ggtitle xlab ylab labs coord_fixed geom_boxplot geom_violin
shinyMVN <- function(){
  shiny::runApp(system.file("shiny-examples/shinyMVN", package="MATH5793YUT"), launch.browser = TRUE)
}
