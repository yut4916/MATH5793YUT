#' Box-Cox Expression
#'
#' @param x numeric vector of data
#' @param lambda numeric value
#'
#' @return numeric value; l(lambda) for the given lambda
#' @export
#'
#' @examples boxCox(rnorm(50), 2)
boxCox <- function(x, lambda){
  n <- length(x)
  xT <- MATH5793YUT::powerTrans(x, lambda)
  (-1)*n/2*log(1/n*sum((xT - mean(xT))^2)) + (lambda - 1)*sum(log(x))
}
