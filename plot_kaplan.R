bioassay.plot_kaplan <- function(data, treatment.name=treatment.name, day=day, event=event, weight=FALSE, groups=TRUE){
  require(rlang)
  require(ggplot2)
  require(ggfortify)
  require(survival)
  require(survminer)
  require(rcompanion)
  require(multcomp)
  require(multcompView)
  treatment.name <- ensym(treatment.name)
  day <- ensym(day)
  event <- ensym(event)
  df <- data.frame(day=data[[day]], event=data[[event]], treatment.name=data[[treatment.name]])
  
  fit <- surv_fit(Surv(day, event) ~ treatment.name, data=df)
  plot <- ggsurvplot(fit, data=df, pval = TRUE, conf.int = TRUE) 
  plot<- plot$plot + theme_bw()+facet_wrap(~treatment.name) +theme(axis.text=element_text(size=15), 
                                                                   axis.title=element_text(size=15,face="bold"), legend.text = element_text(size=15))
  res <- pairwise_survdiff(Surv(day, event) ~ treatment.name, data=df)
  
  # to convert t-test array to group letters. 
  residuals_df <-data.frame(unclass(res), check.names = FALSE, stringAsFators = FALSE)
  residuals_df <- residuals_df[,3:(ncol(residuals_df)-2)]
  residuals_cols<-colnames(residuals_df)
  col_list<-c()
  for (i in residuals_cols){
    new_cols <- unlist(strsplit(i, split='value.', fixed=TRUE))[2]
    col_list<-append(new_cols, x=col_list)
  }
  colnames(residuals_df)<- col_list
  residuals_Ptable <- fullPTable(residuals_df)
  kap_groups <- multcompLetters(residuals_Ptable,
                                compare="<",
                                threshold=0.05,
                                Letters=letters,
                                reversed = FALSE)
  kap_list <- c()
  name_list <- c()
  for (i in seq_along(kap_groups$Letters)){
  letter <- kap_groups$Letters[[i]]
  name <- names(kap_groups$Letters[i])
  kap_list <- append(x=kap_list, values=letter)
  name_list <- append(x=name_list, values=name)
  }
  kap_list <- data.frame(kap_list)
  row.names(kap_list)<- name_list

  return(list(plot, res, kap_list))
  
}
