#' Summary method for linreg objects
#' 
#' @description Extract coefficients from an object of the class .
#' @param object An object of class .
#' @return A vector of coefficients.
#' @examples 

summary.linreg <- function(object,...){
  m <- matrix(0, nrow = nrow(test$var_beta_hat), ncol = 4, dimnames = list(row.names(test$var_beta_hat), c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))
  m[,1] <- test$beta_hat
  m[,2] <- sqrt(diag(test$var_beta_hat))
  m[,3] <- test$t_beta
  m[,4] <- test$p
  
  res <- paste("Residual standard error:", round(sqrt(test$sigma2_hat), digits = 4), "on", test$df, "degrees of freedom")
  

  }


