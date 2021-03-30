#' Proportion Test of Normality (JW 4-29)
#'
#' @param data data.frame with numeric variables
#'
#' @return named list containing the proportion of the data within 1 and 2 standard deviations as well as the test result for each variable Xi in data
#' @export
#'
#' @examples ptn4.29(data=data.frame(x=rnorm(50), y=rchisq(50,1)))
ptn4.29 <- function(data){
  n <- dim(data)[1]
  xbar <- colMeans(data)
  S <- cov(data)

  right1 <- 1.396/sqrt(n)
  right2 <- 0.628/sqrt(n)

  testResult <- as.data.frame(data[FALSE,])
  within_1sd <- data
  within_2sd <- data

  for(i in 1:dim(data)[2]){
    lower1 <- xbar[i] - sqrt(S[i,i])
    upper1 <- xbar[i] + sqrt(S[i,i])
    within_1sd[,i] <- data[i] > lower1 & data[i] < upper1
    phat1_i <- sum(within_1sd[[i]])/n

    lower2 <- xbar[i] - 2*sqrt(S[i,i])
    upper2 <- xbar[i] + 2*sqrt(S[i,i])
    within_2sd[,i] <- data[i] > lower2 & data[i] < upper2
    phat2_i <- sum(within_2sd[[i]])/n

    left1 <- abs(phat1_i - 0.683)
    left2 <- abs(phat2_i - 0.954)

    one_sd <- left1 <= right1
    two_sd <- left2 <= right2

    testResult_i <- one_sd & two_sd
    # if(one_sd & two_sd){
    #   testResult_i <- TRUE
    #   #print(paste0("Variable ", colnames(data)[i], " appears normal"))
    # } else {
    #   testResult_i <- FALSE
    #   #print(paste0("Variable ", colnames(data)[i], " appears non-normal"))
    # }

    testResult[1,i] <- testResult_i
    testResult[2,i] <- one_sd
    testResult[3,i] <- two_sd
    testResult[4,i] <- phat1_i
    testResult[5,i] <- phat2_i
  }

  testResult[1,] <- as.logical(testResult[1,])
  row.names(testResult) <- c("Normal", "Test_1SD", "Test_2SD", "p1", "p2")
  invisible(list(testResults=testResult, within_1sd=within_1sd, within_2sd=within_2sd))
}
