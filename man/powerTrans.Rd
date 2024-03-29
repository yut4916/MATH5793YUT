% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/powerTrans.R
\name{powerTrans}
\alias{powerTrans}
\title{Power Transformations}
\usage{
powerTrans(x, lambda)
}
\arguments{
\item{x}{numeric vector of data}

\item{lambda}{numeric value}
}
\value{
numeric vector, transformed according to Box-Cox family of power transformations (equation 4-34 from JW)
}
\description{
Power Transformations
}
\examples{
powerTrans(rnorm(50), 2)
}
