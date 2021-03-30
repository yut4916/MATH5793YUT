#' Q-Q Plot Correlation Coefficient
#'
#' @param Xi numeric vector; continuous univariate data
#'
#' @return numeric value; the correlation coefficient (equation 4-31 from JW)
#' @export
#'
#' @examples rQ(runif(50)); rQ(rnorm(30))
#' @importFrom stats qnorm
rQ <- function(Xi){
  # Calculate mean
  xbar <- mean(Xi)

  # Order the data from smallest to largest
  xj <- Xi[order(Xi)]

  # Calculate the number of observations
  n <- length(Xi)

  # Calculate probability
  pj <- (1:n - 0.5)/n

  # Get quantiles
  qj <- qnorm(pj)

  # Calculate mean
  qbar <- mean(qj)

  # Calculate rQ, the correlation coefficient
  rQ <- sum((xj - xbar)*(qj - qbar))/sqrt(sum((xj - xbar)^2)*sum((qj - qbar)^2))
  rQ
}


# something to add: load Table 4.2 and use n, alpha, and rQ values to test hypothesis of normality
