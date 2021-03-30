#' Principal Component Analysis
#'
#' @param Sigma variance-covariance matrix
#'
#' @return named list with PCA coefficients, correlation coeficients, and population variance proportion for non-standardized and standardized data
#' @export
#'
#' @examples myPCA(matrix(c(1,-2,0, -2,5,0, 0,0,2), ncol = 3, byrow =TRUE))
myPCA <- function(Sigma){
  S <- Sigma # preferred naming convention
  n <- dim(S)[1]

  # Get eigen values and vectors
  e <- eigen(S)$vectors
  lamda <- eigen(S)$values

  # Calculate rho
  rho <- matrix(nrow=n, ncol=n)
  for(i in 1:n){
    for(k in 1:n){
      rho_ik <- e[i,k]*sqrt(lamda[i])/sqrt(S[k,k])

      rho[i,k] <- rho_ik
    }
  }

  prop <- lamda/sum(lamda)

  # Repeat for Rho
  Vhalf <- matrix(0, nrow=n, ncol=n)
  diag(Vhalf) <- sqrt(diag(S))
  Rho <- solve(Vhalf)%*%S%*%solve(Vhalf)

  # Get eigen values and vectors
  e_R <- eigen(Rho)$vectors
  lamda <- eigen(Rho)$values

  # Calculate rho
  rho_R <- matrix(nrow=n, ncol=n)
  for(i in 1:n){
    for(k in 1:n){
      rho_ik <- e_R[i,k]*sqrt(lamda[i])/sqrt(Rho[k,k])

      rho_R[i,k] <- rho_ik
    }
  }

  prop_R <- lamda/sum(lamda)

  invisible(list(PC_Sigma=e, PC_rho=e_R, rho_YiXj=rho, rho_YiZk=rho_R, prop_Y=prop, prop_Z=prop_R))
}
