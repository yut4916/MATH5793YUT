#' Trace
#'
#' @param mat a matrix
#'
#' @return a numerical value; the sum of the diagonals
#' @export
#'
#' @examples tr(matrix(c(1,2,3,4), nrow=2))
tr <- function(mat){
  sum(diag(mat))
}
