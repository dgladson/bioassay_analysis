


bioassay.analyze_consumption <- function(data, x=treatment, area=area, day=day, ref='REF', xlabel=NULL, ylabel=NULL, rna_ng = NULL, treatment.rename=NULL){
require(ggplot2)
x <- ensym(x)
area <- ensym(area)
day <- ensym(day)
x_string <- deparse(substitute(x))
y_string <- deparse(substitute(y))
df <- data.frame(x=data[[x]], area=data[[area]], day=data[[day]])
df$x <- as.factor(df$x)





#subsets by day and strips each day into samples and reference leaves. Calculates percent leaf consumption based on each day's reference leaf. 
samples = c()
for (d in unique(df$day)){
sample <- c()
by_day <- subset(df, day == d) 
refs <- subset(by_day, x==ref)
avg_ref <- mean(refs$area)

sample <- subset(by_day, x!=ref)
sample$percent.consumption <- ((avg_ref - sample$area) / avg_ref) * 100
samples <- rbind(samples, sample)

}





for (i in seq_along(samples$percent.consumption)){      #Turns negative consumption values into 0 accounting for leaf cutting and area measurement errors. 
  if (samples$percent.consumption[i] <0)
    samples$percent.consumption[i] = 0
}

  day_means_bar=aggregate(percent.consumption ~x + day, data=samples, FUN= mean) #aggregates means and SE's for each treatment.
  day_sd_bar=aggregate(percent.consumption ~x + day, data=samples, FUN= sd)
  day_len_bar=aggregate(percent.consumption ~x + day, data=samples, FUN= length)
  se_bar = day_sd_bar$percent.consumption / sqrt(day_len_bar$percent.consumption) #for bar graph. 
  day_bar = cbind(day_means_bar, se_bar) #for bar graph. 
  colnames(day_bar) <- c('treatment', 'day', 'percent.consumption', 'se')
  if (is.null(xlabel)){                        # optional renaming of x and y labels for bar graph. 
    label.x=x_string
  } else  {
    label.x=xlabel
  }
  
  if (is.null(ylabel)){
    label.y=y_string
  } else  {
    label.y=ylabel
  }
  
  day_bar$day <- factor(day_bar$day, levels = c(unique(day_bar$day)))
  
  

  if (is.null(treatment.rename)){                                                   #optional method for renaming the default treatments stripped from json files. This method is controlled by param 'treatment.rename'
    colnames(day_bar) <- c('treatment.name', 'day', 'percent.consumption', 'se')    # and takes a dictionary to map the changes accordingly. To streamline, use 'bioassay::utils.rename'. 
  } else {
    new_list <- c()
    for (i in seq_along(day_bar$treatment)){
      trt <-as.character( day_bar$treatment[i])
      new_list <- append(values=treatment.rename[trt], x=new_list )
    }
    day_bar$treatment.name <- new_list
  }
  


  
  plot <- ggplot(day_bar, aes(x=treatment.name, y=percent.consumption, fill=day)) +                           #creates bar graph
    geom_bar(position='dodge', stat='identity') + 
    geom_errorbar(aes(ymin=percent.consumption-se, ymax=percent.consumption+se), width=0.5, position=position_dodge(0.85)) +
    theme(axis.text=element_text(size=15), 
          axis.title=element_text(size=15,face="bold")) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    xlab(label.x) +
    ylab(label.y) +
    ylim(0,100)

  print(plot)
  
invisible(day_bar) 

#print(samples)#prints mean percent leaf consumption for each treatment with an option to save the df to an object.                                    


}




       