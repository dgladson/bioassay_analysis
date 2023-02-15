bioassay.analyze_LC <- function(data, dose=optional.group_1, alive=alive, n=n, day_col=day, day_oi=5, LC=c(50, 99), type='probit'){
dose <- ensym(dose)
alive <- ensym(alive)
n <- ensym(n)

day_col <- ensym(day_col)



data$dead <- data[[n]] - data[[alive]]  # create two new columns
data$total <- data[[alive]] + data$dead
df <- subset(data, data[[day_col]]==day_oi)


#aggregate data
agg_alive <- aggregate(df[[alive]], by=list(df[[dose]], df[[day_col]]), sum)
agg_dead <- aggregate(df$dead, by=list(df[[dose]], df[[day_col]]), sum)
agg_total <- aggregate(df$total, by=list(df[[dose]], df[[day_col]]), sum)
agg_data <- cbind(agg_alive, agg_dead$x, agg_total$x)
colnames(agg_data) <- c('dose', 'day', 'alive', 'dead', 'total')

print(agg_data)

#builds probit model
if (type=='probit'){
  m <- LC_probit((dead / total) ~ log10(as.numeric(dose)), p = LC, 
                 weights = total, data = agg_data[agg_data$dose != 0, ], long_output = TRUE)
  print(m)  
  
}
 if (type=='logit'){
   m <- LC_logit((dead / total) ~ log10(as.numeric(dose)), p = LC, 
                  weights = total, data = agg_data[agg_data$dose != 0, ], long_output = TRUE)
   print(m)   
  
 }



#graphs probit model
p1_probit <- ggplot(data = agg_data[agg_data$dose != 0, ],
                    aes(x = log10(dose), y = (dead / total))) +
  geom_point() +
  geom_smooth(method = "glm",
              method.args = list(family = binomial(link = "probit")),
              aes(weight = total), colour = "#FF0000", se = TRUE) +
  theme(axis.text=element_text(size=20), 
        axis.title=element_text(size=20,face="bold"))
print(p1_probit)




}
