bioassay.plot_bar <- function(data, x=treatment.name, y=dead, day=day, weight=FALSE, y_limit=1, ylabel=NULL, xlabel=NULL, fill=NULL, day_oi=NULL, trt_oi=NULL){
  require(rlang)
  require(ggplot2)
  require(splus2R)
  x <- ensym(x)
  y <- ensym(y)
  day <- ensym(day)
  if (!missing(fill)){
  fill <- ensym(fill)
  }

  x_string <- deparse(substitute(x))
  y_string <- deparse(substitute(y))
  
  if (!is.missing(day_oi)){
  data <- subset(data, day %in% day_oi) 
  }
  
  if (!is.missing(trt_oi)){
  data <- subset(data, data[[x]] %in% trt_oi)  

  }

  
  if (is.missing(fill)){
  day_means_bar=aggregate(data[[y]] ~ data[[x]] + data[[day]], FUN= mean)
  colnames(day_means_bar) <- c(x_string, 'day', y_string)
  day_sd_bar=aggregate(data[[y]] ~ data[[x]] + data[[day]], FUN= sd)
  colnames(day_sd_bar) <- c(x_string, 'day', y_string)
  day_len_bar=aggregate(data[[y]] ~ data[[x]] + data[[day]], FUN= length)
  colnames(day_len_bar) <- c(x_string, 'day', y_string)
  se_bar = as.numeric(day_sd_bar[[y]]) / sqrt(as.numeric(day_len_bar[[y]])) #for bar graph. 
  day_bar = cbind(day_means_bar, se_bar) #for bar graph. 
  } else {
    
    day_means_bar=aggregate(data[[y]] ~ data[[x]] + data[[day]] + data[[fill]], FUN= mean)
    colnames(day_means_bar) <- c(x_string, 'day', fill, y_string)
    day_sd_bar=aggregate(data[[y]] ~ data[[x]] + data[[day]] + data[[fill]],  FUN= sd)
    colnames(day_sd_bar) <- c(x_string, 'day', fill, y_string)
    day_len_bar=aggregate(data[[y]] ~ data[[x]] + data[[day]] + data[[fill]], FUN= length)
    colnames(day_len_bar) <- c(x_string, 'day', fill, y_string)
    se_bar = as.numeric(day_sd_bar[[y]]) / sqrt(as.numeric(day_len_bar[[y]])) #for bar graph. 
    day_bar = cbind(day_means_bar, se_bar) #for bar graph.  
  }
  #creates bar graph using geom_bar. 
  
  if (weight==TRUE) {
    facet_bar <- NULL
  } else if (weight==FALSE) {
    facet_bar <- ~day
  }
  if (is.null(ylabel)){
    label.y <- y
  } else {
    label.y <- ylabel
  }
  

  if (is.missing(xlabel)){
    label.x <- x
  } else {
    label.x <- xlabel
  }   
  
  if (is.missing(fill)){
    filled <- NULL
  } else {
    filled <- as.factor(day_bar[[fill]])
    
  }
  
    #plots bar graph with facet wrap by day. 
  
  plot <- ggplot(data=day_bar, aes(x=as.factor({{x}}), y={{y}}, fill=filled)) +
    geom_bar(stat="identity", width=0.5) +
    ylim(0, y_limit) +
    geom_errorbar(aes(ymin={{y}}+se_bar, ymax={{y}}-se_bar), width=0.1,
                  position=position_dodge(0.005)) +
    labs(y=label.y, x=label.x, fill=deparse(substitute(fill))) + 
    guides(x =  guide_axis(angle = 90)) +
    facet_wrap(facet=facet_bar, scales='free_y') +
    theme(axis.text=element_text(size=15), 
          axis.title=element_text(size=15,face="bold"))
  print(day_bar)
  return(plot)
}


