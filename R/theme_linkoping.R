#' ggplot2 theme with colors from the Linkoping University graphical manual
#' @export

theme_linkoping <- function(){
  theme_minimal() %+replace%
  
  theme(
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    plot.title = element_text(size = 20,
                              hjust = 0.5,
                              color="#00b9e7"),
    
    axis.title = element_text(size = 15,
                              color="#00b9e7"),
    
    axis.text = element_text(size = 15),
    axis.line = element_line(color = "black"),
  )
} 

