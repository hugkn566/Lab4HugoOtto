#' Print out the estimated coeffiencts 
#' 
#' @description Estimated coeffiencts from an object of class
#' @param object An object of Class
#' @return A list of estimated coeffiencts
print.linreg <- function(x) {
  matris <- as.matrix(x$beta_hat)
  testar <- as.data.frame(t(matris))
  rownames(testar) <- ""
  lista <- list(coefficents=testar)
  return(lista)
  }

