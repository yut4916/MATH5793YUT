#' T-Squared Test and Plot
#'
#' @param data a data.frame with p variables and n observations
#' @param mu0 a numeric vector, length p
#' @param alpha a numeric value between 0 and 1
#'
#' @return named list
#' @export
#'
#' @examples Tsq.test(data=data.frame(rnorm(50), rnorm(50)), mu0=c(-0.5, 1.2))
#' @importFrom stats cov qf
Tsq.test <- function(data, mu0, alpha=0.05){
  # data = data.frame with p variables and n observations
  # mu0 = numeric vector, length p
  # alpha = numeric value between 0 and 1

  n <- dim(data)[1]
  p <- dim(data)[2]

  xbar <- colMeans(data)
  S <- cov(data)
  Si <- solve(S)

  # Compute the generalized squared distance
  gsd <- n*t(xbar - mu0)%*%Si%*%(xbar - mu0)

  # Compute the F statistic
  Fstat <- qf(1 - alpha, p, n-p)
  c2 <- p*(n-1)/(n-p)*Fstat

  # Test the hypothesis (i.e., compare gsd and c2)
  result <- ""
  if(gsd <= c2){
    result <- "Fail to reject"
  } else{
    result <- "Reject the null"
  }

  # Get the eigen values and eigen vectors
  values <- eigen(S)$values
  vectors <- eigen(S)$vectors

  # Calculate the half-lengths of the major and minor axes
  majorHalf <- sqrt(values[1])*sqrt(p*(n-1)/(n*(n-p))*Fstat)
  minorHalf <- sqrt(values[2])*sqrt(p*(n-1)/(n*(n-p))*Fstat)

  # Get the ratio of the major and minor axes
  ratio <- majorHalf/minorHalf

  # Plot the data and their confidence region ellipse
  g <- ggplot2::ggplot(data, ggplot2::aes(x=data[,1], y=data[,2])) +
    ggplot2::geom_point() +
    ggplot2::geom_point(data=data.frame(mu0), ggplot2::aes(x=data.frame(mu0)[1,], y=data.frame(mu0)[2,]), color="red") +
    ggplot2::geom_segment(ggplot2::aes(y=xbar[2], x=min(data[,1])-0.1, xend=xbar[1], yend=xbar[2]), size=0.05, linetype="dashed") + # horizontal dashed
    ggplot2::geom_segment(ggplot2::aes(x=xbar[1], y=min(data[,2])-0.1, xend=xbar[1], yend=xbar[2]), size=0.05, linetype="dashed") + # vertical dashed
    # geom_segment(aes(x=xbar[1], y=xbar[2], xend=majorHalf*vectors[1,1], yend=majorHalf*vectors[2,1]), size=0.5) + # major axis
    # geom_segment(aes(x=xbar[1], y=xbar[2], xend=minorHalf*vectors[1,2], yend=minorHalf*vectors[2,2]), size=0.5) + # minor axis
    ggplot2::stat_ellipse(level=1-alpha)

  # Return a list
  invisible(list(result=result, quadSize=gsd, scaledQuant=c2, eigenVectors=vectors, eigenValues=values,
                 majorHalf=majorHalf, minorHalf=minorHalf, axesRatio=ratio, plot=g))
}
