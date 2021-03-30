#' Q-Q Plot
#'
#' @param Xi numeric vector; continuous univariate data
#'
#' @return plot
#' @export
#'
#' @examples qqplot(runif(50))
#' @importFrom stats qnorm
qqplot <- function(Xi){
  # Order the data from smallest to largest
  xj <- Xi[order(Xi)]

  # Calculate the number of observations
  n <- length(Xi)

  # Calculate probability
  pj <- (1:n - 0.5)/n

  # Get quantiles
  qj <- qnorm(pj)

  # Plot
  ggplot2::ggplot(data=data.frame(qj,xj), ggplot2::aes(x=qj, y=xj)) +
    ggplot2::geom_point()  +
    ggplot2::ggtitle("Univariate Q-Q Plot") +
    ggplot2::xlab("Standard Normal Quantiles") +
    ggplot2::ylab("Value") +
    ggplot2::labs(subtitle="Assessing Normality")
}
