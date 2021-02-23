#' The Newton-Raphson Algorithm Function
#'
#' @param x0 a numeric value for initializing x, the angle of rotation (in radians)
#' @param f a function that takes one input, x, and plugs it into the equation for s12t
#' @param delta a numeric value for the increment in our derivative approximation (h)
#' @param epsilon a numeric value for how close our root approximation is to zero
#' @param range a vector of two numeric values, the range of x in radians
#'
#' @return a plot of s12t vs x with the Newton-Raphson approximations shown and a list of numeric values, the true roots, where s12t=0.
#' @export
#'
#' @examples my.newt2.0(x0=1, delta=0.000001, f=function(x) x^2, range=c(0, pi/2))
#'
my.newt2.0 <- function(x0, f, delta=0.0001, epsilon=1e-12, range=c(0, 2*pi)){
  # x0 initial value
  # f the function to be zeroed
  # delta is the increment in the derivative
  # epsilon is how close our approximation is to zero

  fdash <- function(x){
    (f(x+delta)-f(x))/delta
  }

  d <- 1000 # initial values
  i <- 0
  x <- c() # empty vector
  y <- c()
  x[1] <- x0 # assign initial guess
  y[1] <- f(x[1]) # initial y value

  while(d > epsilon & i<100){ # ensures that it doesnt loop too much
    i <- i+1
    x[i+1] <-  x[i] - f(x[i])/fdash(x[i]) # NR step
    y[i+1] <- f(x[i+1]) # update y value
    d <- abs(y[i+1]) # update d
  }

  graphics::curve(f(x), xlim=range(c(range(x), -range(x))), xaxt="n", main="Newton-Raphson Algorithm")
  graphics::points(x, y, col="red", pch=19, cex=1.5)
  graphics::axis(1, x, round(x, 2), las=2)
  graphics::abline(h=0, col="red")

  graphics::segments(x[1:(i-1)], y[1:(i-1)], x[2:i], rep(0, i-1), col="blue", lwd=2)
  graphics::segments(x[2:i], rep(0, i-1), x[2:i], y[2:i], lwd=0.5, col="pink")

  root <- rootSolve::uniroot.all(f, range)
  list(root=root)
}
