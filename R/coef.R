#' Coef method for linreg objects
#' 
#' @description Extract coefficients from an object of the class .
#' @param object An object of class .
#' @return A vector of coefficients.
#' @examples 
#'  

coef.linreg <- function(object){
  coefs <- as.vector(object$beta_hat)
  names(coefs) <- row.names(object$beta_hat)
  return(coefs)
}