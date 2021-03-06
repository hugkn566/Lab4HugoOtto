#' Plot method for linreg xs
#' 
#' @description \code{plot} method for xs of the class "linreg".
#' @param x an object of class "linreg", generated by a call to \code{\link{linreg}}.
#' @param ... additional arguments.
#' @return Two scatter plots. The first one is residuals vs fitted values and the second is the scale-location.
#' @examples 
#' linreg_obj <- linreg(Petal.Length~Species, datasets::iris)
#' plot(linreg_obj)
#' @export


plot.linreg <- function(x,...){
  colnames(x$y_hat) <- "y_hat"
  colnames(x$e_hat) <- "e_hat"
  
  data <- as.data.frame(cbind(x$y_hat, (x$e_hat)))
  outliers <- match(utils::tail(sort(abs(x$e_hat)), n=3), abs(x$e_hat))
  
  p <- ggplot2::ggplot(data=data, ggplot2::aes(x = y_hat, y = e_hat)) + 
    ggplot2::geom_point(shape=1, size=3) + 
    ggplot2::geom_text(data=data[outliers,], ggplot2::aes(label=outliers), nudge_x = 0.15) +
    ggplot2::stat_smooth(data=data[-outliers,], se=FALSE, color="#ff6442") + 
    ggplot2::geom_hline(yintercept = 0, col="grey", linetype="dotted") + 
    theme_linkoping() + 
    ggplot2::labs(x=" Fitted Values", y="Residuals", title="Residuals vs Fitted")

  data2 <- as.data.frame(cbind(x$y_hat, sqrt(abs(x$e_hat/sqrt(x$sigma2_hat)))))
  
  p2 <- ggplot2::ggplot(data=data2, ggplot2::aes(x = y_hat, y = e_hat)) + 
    ggplot2::geom_point(shape=1, size=3) + 
    ggplot2::geom_text(data=data2[outliers,], ggplot2::aes(label=outliers), nudge_x = 0.15) +
    ggplot2::stat_smooth(data=data2[-outliers,], se=FALSE, color="#ff6442") + 
    ggplot2::geom_hline(yintercept = 0, col="grey", linetype="dotted") + 
    theme_linkoping() +
    ggplot2::labs(x=" Fitted Values", y=expression(sqrt("|Standardized residuals|")), title="Scale-Location") + 
    ggplot2::theme(plot.title=ggplot2::element_text(hjust = 0.5))

  suppressMessages(suppressWarnings(graphics::plot(p)))  
  graphics::par(ask=TRUE)
  suppressMessages(suppressWarnings(graphics::plot(p2)))
  graphics::par(ask=FALSE)
}


