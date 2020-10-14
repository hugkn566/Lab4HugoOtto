#' Perform Ridge regression with OLS
#' 
#' @param formula An object of the class \code{\link[stats]{formula}}.
#' @param data A data set of the class \code{\link[base]{data.frame}}.
#' @param lambda A numeric value.
#' @return The function returns the results of the Ridge regression as an object of class "ridgereg".
#' @examples 
#' ridredge_obj <- ridgereg(Petal.Length~Species, datasets::iris)
#' @export

ridgereg <- function(formula,data,lambda=0){
  # Checking if the arguments are correct
  stopifnot(class(formula)=="formula" & is.data.frame(data))
  
  # Setting up the matrices
  X <- stats::model.matrix(formula, data)
  y_name <- all.vars(formula, data)
  Y <- as.matrix(data[y_name[1]])
  # x_norm <- scale(X[,2:ncol(X)])
  # x_norm <- cbind(X[,1], x_norm)
  
  #Regressions coef
  Bridge <- solve((t(X)%*%X)+((lambda)*diag(ncol(X))))%*%(t(X)%*%(Y))
  y_hat <- X%*%Bridge
  
  if(is.null(rownames(Bridge)[1])){
    rownames(Bridge) <- c("(Intercept)", as.character(formula[3]))
  } else {
    rownames(Bridge)[1] <- "(Intercept)"
  }
  
  res <- list(coef = Bridge,
              y_hat = y_hat,
              data_name= deparse(substitute(data)),
              y_name = as.character(formula[2]),
              x_names = as.character(formula[3]),
              lambda = lambda,
              formula=formula,
              data = data)
  class(res) <- "ridgereg"

  return(res)
}


