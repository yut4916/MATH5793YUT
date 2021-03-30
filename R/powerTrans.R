#' Power Transformations
#'
#' @param x numeric vector of data
#' @param lambda numeric value
#'
#' @return numeric vector, transformed according to Box-Cox family of power transformations (equation 4-34 from JW)
#' @export
#'
#' @examples powerTrans(rnorm(50), 2)
powerTrans <- function(x, lambda){
  if(lambda==0){
    log(x)
  } else{
    (x^lambda - 1)/lambda
  }
}
