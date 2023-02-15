bioassay.plot_line <- function(data, x=treatment.name, y=prop, day=day, ylabel=NULL, xlabel=NULL, start=1, ylim=1){
  require(ggplot2)
  require(rlang)
  require(ggplot2)
  x <- ensym(x)
  y <- ensym(y)
  day <- ensym(day)
  day_string <- as_string(day)
  x_string <- deparse(substitute(x))
  y_string <- deparse(substitute(y))
  message <- paste("This is the line-graph for: x =", x_string, "and y =", y_string, "over each day. Bars = +/- SE")
  #makes a df for day 0 (which isn't already in the inputed df).
  df_data <- data.frame(y_var=data[[y]], x_var=data[[x]], day_var=data[[day]])
  day_min <- min(data[[day]])
  x_len <- nrow(subset(data, day == day_min)) 
  y_day_0 <- rep(start, x_len) 
  x_day_0 <- data[[x]][1:x_len]
  day_0 = rep(0, x_len)
  df_day_0 <- data.frame(y_var= y_day_0, x_var = x_day_0, day_var = day_0) #makes day 0 df
  df <- rbind(df_day_0, df_data) #binds day 0 and rest of data into single dataframe 
  day_means_line=aggregate(df$y_var, by=list(treatment=df$x_var, day = df$day_var), mean) 
  day_sd_line=aggregate(df$y_var, by=list(treatment=df$x_var, day = df$day_var), sd) 
  day_len_line=aggregate(df$y_var, by=list(treatment=df$x_var, day = df$day_var), length)
  se_line = day_sd_line$x / sqrt(day_len_line$x) #for line graph
  
  if (is.null(ylabel)){
    label.y <- y_string 
  } else
  {
    label.y = ylabel
  }
  
  if (is.null(xlabel)){
    label.x <- day_string
  } else
  {
    label.x = xlabel
  }
  
  plot <- ggplot(day_means_line, aes(x = day, y = x, color = treatment)) +
    geom_line(size=2) + 
    geom_errorbar(aes(ymin=x+se_line, ymax=x-se_line), width=1,
                  position=position_dodge(0.005)) +
    xlab("Day") +
    ylab(label.y) +
    xlab(label.x) +
    geom_point() + 
    theme_classic() +
    theme(axis.text=element_text(size=15), 
          axis.title=element_text(size=15,face="bold"), legend.text = element_text(size=15), 
          legend.key.size = unit(1, 'cm'), 
          legend.title = element_text(size=15)) +
    guides(colour = guide_legend(override.aes = list(size=10))) +
    scale_x_continuous(breaks=seq(0,10,by=2)) + 
    ylim(0, ylim)
  print(day_means_line)   

  return(list(message, plot))
}
