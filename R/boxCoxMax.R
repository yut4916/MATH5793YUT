#' Box-Cox Expression Solution
#'
#' @param x numeric vector of data
#' @param lambdaMin numeric value; minimum value of lambda
#' @param lambdaMax numeric value; maximum value of lambda
#' @param increment numeric value; delta
#'
#' @return named list; solution (lambda value that maximizes the Box-Cox expression), maximum (max value of expression), and a plot (l(lambda) vs lambda)
#' @export
#'
#' @examples boxCoxMax(rnorm(50))
boxCoxMax <- function(x, lambdaMin=-3, lambdaMax=5, increment=0.001){
  lambdas <- seq(lambdaMin,lambdaMax,increment)

  ly <- c()
  for(y in lambdas){
    l <- MATH5793YUT::boxCox(x, y)
    ly <- c(ly, l)
  }

  values <- data.frame(x=lambdas, y=ly)
  maxL <- max(values$y)
  solution <- values[values$y==maxL,"x"]

  plot <- ggplot2::ggplot(values, ggplot2::aes(x=x, y=y)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Box-Cox Expression") +
    ggplot2::xlab("Lambda") +
    ggplot2::ylab("l(lambda)")

  invisible(list(solution=solution, maximum=maxL, plot=plot))
}
