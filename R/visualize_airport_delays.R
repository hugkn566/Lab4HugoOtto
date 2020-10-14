#' Visualize delays for airports in the USA
#' 
#' @description Uses ggplot2 to create a scatterplot showing delays at different airport in the USA
#' @return Returns a ggplot object containing a scatterplot showing delays at different airport in the USA
#' @examples 
#' airport_delays <- visualize_airport_delays()
#' airport_delays
#' @export
#' @importFrom dplyr %>% 

visualize_airport_delays <- function(){
flights <- nycflights13::flights
airports <- nycflights13::airports
airport <- airports[,1:4]
delay_depart <- flights %>% 
  dplyr::group_by(faa = origin) %>% 
  dplyr::summarise(delay=mean(dep_delay, na.rm = T))
delay_arrv <- flights %>% 
  dplyr::group_by(faa = dest) %>% 
  dplyr::summarise(delay=mean(arr_delay, na.rm = T))
delay <- dplyr::bind_rows(delay_depart, delay_arrv)
data <- dplyr::inner_join(airport, delay, by="faa")

p <- ggplot2::ggplot(data, ggplot2::aes(x = lon, y =lat)) +
  ggplot2::geom_point(ggplot2::aes(color= delay)) + 
  ggplot2::theme_bw() +
  ggplot2::labs(title = "Flight delays"  , x = "Longitude" , y = "Latitude")  +
  ggplot2::theme(axis.title.y=ggplot2::element_text(vjust = 0.5, size = 13 , face = "bold")) +
  ggplot2::theme(axis.title.x=ggplot2::element_text(vjust = 0.5 ,size = 13 , face = "bold")) +
  ggplot2::theme(plot.title=ggplot2::element_text(size = 14, face = "bold" , hjust = 0.4 )) +
  ggplot2::theme(axis.text.y=ggplot2::element_text(size = 11)) + 
  ggplot2::theme(axis.text.x=ggplot2::element_text(size = 11)) +
  ggplot2::theme(panel.grid.major.x=ggplot2::element_blank(), 
                 panel.grid.minor.x=ggplot2::element_blank(),
                 panel.grid.major.y=ggplot2::element_blank(),
                 panel.grid.minor.y=ggplot2::element_blank()) +
  ggplot2::scale_color_gradient(low = "#1A9850", high = "#D73027") +
  ggplot2::labs(colour = "Delay")
return(p)
}
