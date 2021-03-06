% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my.newt2.0.R
\name{my.newt2.0}
\alias{my.newt2.0}
\title{The Newton-Raphson Algorithm Function}
\usage{
my.newt2.0(x0, f, delta = 1e-04, epsilon = 1e-12, range = c(0, 2 * pi))
}
\arguments{
\item{x0}{a numeric value for initializing x, the angle of rotation (in radians)}

\item{f}{a function that takes one input, x, and plugs it into the equation for s12t}

\item{delta}{a numeric value for the increment in our derivative approximation (h)}

\item{epsilon}{a numeric value for how close our root approximation is to zero}

\item{range}{a vector of two numeric values, the range of x in radians}
}
\value{
a plot of s12t vs x with the Newton-Raphson approximations shown and a list of numeric values, the true roots, where s12t=0.
}
\description{
The Newton-Raphson Algorithm Function
}
\examples{
my.newt2.0(x0=1, delta=0.000001, f=function(x) x^2, range=c(0, pi/2))

}
