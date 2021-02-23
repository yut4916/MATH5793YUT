#' My First Shiny App
#'
#' @return interactive html shiny browser
#' @export
#'
#' @examples \dontrun{MATH5793YUT::myfirstShiny()}
myfirstShiny <- function(){
  shiny::runApp(system.file("shiny-examples/firstShiny", package="MATH5793YUT"), launch.browser = TRUE)
}
