bioassay.model <- function(data, x=treatment.name, y=prop, n=n, alive=alive, type='binomial', day=day, ref='shrimp.seq.2'){
  library(rcompanion)
  require(RVAideMemoire)
  require(multcomp)
  require(multcompView)
  require(car)
  library(emmeans)
  x <- ensym(x)
  y <- ensym(y)
  day <- ensym(day)
  alive <- ensym(alive)
  n <- ensym(n)
  x_string <- as_string(x)
  y_string <- as_string(y)
  day_string <- as_string(day)
  data$dead <- data[[n]] - data[[alive]]

  
  #build model for each unique day
for (i in unique(data$day)){
  print('____________________________________________________________________________________________')
  print(paste('This is a', ' ', type, ' ', 'glm for x=', x_string, ' ', 'and y=', y_string, ' ', 'for day', ' ', i, '.', sep=""))
  sub <- subset(data, day==i)
  sub$x <- as.factor(sub[[x]])
  sub <- within(sub, x <- relevel(x, ref = ref))
  model <- glm(data=sub, sub[[y]]~x, family=type)
  a <- Anova(model, type=3)
  print(a)
  groups <- cld(emmeans(model, ~ x), 		alpha=0.05,Letters=letters,adjust="tukey")
  plot <- plot(emmeans(model, ~ x), ylab=x_string)
  print(plot)
  print(groups)
  
  
  #make contingency table for FET
  
#  alive.number <- aggregate(sub[[alive]], by=list(sub[[x]]), sum)
#  dead.number <- aggregate(sub$dead, by=list(sub[[x]]), sum)
#  con.t <- cbind(alive.number$x, dead.number$x)
  

#  colnames(con.t) <- c('alive', 'dead')
#  row.names(con.t) <- alive.number$Group.1
#  con.mc <- fisher.multcomp(con.t)
#  con.df<- con.mc$p.value
#  con.df <- data.frame(con.df)
#  mult_Ptable <- fullPTable(con.df)
#  mult_groups<- multcompLetters(mult_Ptable,
#                               compare="<",
#                               threshold=0.05,
#                               Letters=letters,
#                               reversed = FALSE)
 
# print(paste('These are multiple comparisons', ' ',  "using Fisher's Exact Test",' ', 'for x=', x_string, ' ', 'and y=', y_string, ' ', 'for day', ' ', i, '.', sep=""))

 #print(paste('These are the statistically significant groups with alpha = 0.05'))
 #letter_list <- c()
 #for (i in seq_along(mult_groups$Letters)){
#  letter <- mult_groups$Letters[[i]]
#  letter_list <- append(x=letter_list, values=letter)
# }#
#
# con.t <- cbind(con.t, letter_list)
# print(con.t)

 

  
    }
  }
