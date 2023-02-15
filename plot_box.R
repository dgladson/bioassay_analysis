bioassay.plot_box <- function(data, x=treatment.name, y=dead, day=day, weight=FALSE, ylabel=NULL, xlabel=NULL){
  require(ggplot2)
  require(rlang)
  x <- ensym(x)
  y <- ensym(y)
  day <- ensym(day)
  x_string <- deparse(substitute(x))
  y_string <- deparse(substitute(y))
  message <- paste("This is the boxplot for: x =", x_string, "and y =", y_string, "over each day tested.")
  if (weight==TRUE) {
    facet_box <- NULL
    scale=NULL
  } else if (weight==FALSE) {
    facet_box <- ~day
    scale='free_y'}
  
  if (is.null(ylabel)){
    label.y <- y_string 
  } else
  {
    label.y = ylabel
  }
  
  if (is.null(xlabel)){
    label.x <- x_string 
  } else
  {
    label.x = xlabel
  }
  
  
#plots box plot with dacet wrap by day. 
  plot <- ggplot(data, aes(x={{x}}, y={{y}}, fill = NULL)) + 
    geom_boxplot() +
    xlab(label.x) +
    ylab(label.y) +
    guides(x =  guide_axis(angle = 90)) +
    facet_wrap(facets=facet_box, scales=scale) 
  return(list(message, plot))
}
