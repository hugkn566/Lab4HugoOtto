#' ggplot2 theme with colors from the Linkoping University graphical manual
#' @export

theme_linkoping <- function(){
    ggplot2::theme(
    
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    
    plot.title = ggplot2::element_text(size = 20,
                              hjust = 0.5,
                              color="#00b9e7"),
    
    axis.title = ggplot2::element_text(size = 15,
                              color="#00b9e7"),
    
    axis.text = ggplot2::element_text(size = 15),
    axis.line = ggplot2::element_line(color = "black"),
    
    panel.background = ggplot2::element_rect(fill="white")
  )
} 

