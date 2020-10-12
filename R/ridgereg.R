ridgereg <- function(formula,data,lambda){
  # Checking if the arguments are correct
  stopifnot(class(formula)=="formula" & is.data.frame(data))
  
  # Setting up the matrices
  X <- stats::model.matrix(formula, data)
  y_name <- all.vars(formula, data)
  Y <- as.matrix(data[y_name[1]])
  x_norm <- scale(X[,2:ncol(X)])
  x_norm <- cbind(X[,1], x_norm)
  
  #Regressions coef
  Bridge <- solve((t(x_norm)%*%x_norm)+((lambda)*diag(ncol(X))))%*%(t(x_norm)%*%(Y))
  y_hat <- x_norm%*%Bridge
  
  rownames(Bridge)[1] <- "(Intercept)"
  res <- list(coef = Bridge, 
              y_hat = y_hat,
              data_name= deparse(substitute(data)),
              y_name = as.character(formula[2]),
              x_names = as.character(formula[3]),
              lambda = lambda,
              formula=formula)
  class(res) <- "ridgereg"

  return(res)
}
