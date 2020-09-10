#' Predict method for linreg objects
#' 
#' @description Predicted values from an object of the class .
#' @param object An object of class .
#' @return A vector of predicted values.
#' @examples 
#'  

pred <- function(object,...){
  y_hat <- as.vector(object$y_hat)
  return(y_hat)
}


