#' Predict method for ridgereg objects
#' 
#' @description Predicted values from an object of the class "ridgereg".
#' @param object An object of class "ridgereg", generated by a call to \code{\link{ridgereg}}.
#' @param newdata Option new data to calculate predictions for
#' @param ... additional arguments.
#' @return A vector of predicted values.
#' @examples 
#' ridgereg_obj <- ridgereg(Petal.Length~Species, datasets::iris)
#' predict(ridgereg_obj)
#' @export

predict.ridgereg <- function(object, newdata=NULL, ...){
  if(methods::hasArg(newdata)==FALSE){
    y_hat <- as.vector(object$y_hat)
    return(y_hat) 
  }else{
  
    X <- stats::model.matrix(object$formula, newdata)
    x_norm <- scale(X[,2:ncol(X)])
    x_norm <- cbind(X[,1], x_norm)
  
  preds <- x_norm %*% object$coef
  return(preds)
  }
}
