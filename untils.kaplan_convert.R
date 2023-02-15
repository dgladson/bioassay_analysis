bioassay.kaplan_convert <- function(data, x=treatment.name, id=cup.ID, alive=alive, removed=removed, end=end, rep=rep, n=n, day=day){
  require(rlang)
  id <- ensym(id)
  alive <- ensym(alive)
  removed <- ensym(removed)
  end <- ensym(end)
  rep <- ensym(rep)
  n <- ensym(n)
  x<-ensym(x)
  day=ensym(day)
  
  sub_data = data.frame(id=data[[id]], treatment.name=data[[x]], alive=data[[alive]], rep=data[[rep]], n=data[[n]], day=data[[day]], removed=data[[removed]], end=data[[end]]) #dumps all inputs into a new df. 
  day_0<- subset(sub_data, day == min(sub_data$day)) #inputs don't include day 0. This adds day 0 data. 'Alive' is stripped from 'n'. Assumed there will be no censored data at day 0. 
  day_0$alive = day_0$n
  day_0$day =0
  day_0$removed = 0
  day_0$end= 0
  new_data <- rbind(day_0, sub_data)
  master_list=c()
  event <-c()
  for (i in unique(new_data$id)){                 #breaks data into each unique 'id'. 
    by_id <-subset(new_data, new_data$id == i)
    vect_len <- nrow(by_id)
    event=c()
    for (d in 2:vect_len){                        #compares each day from the previous (starting at the second collected day because day 0 is included). 'Removed' indv are considered. Appends values to a new list. 
      event<-append(values=(by_id$alive[d-1] - (by_id$alive[d]+by_id$removed[d-1])), x=event)
    }
    by_id$event <- c(0, event)                    # makes a list for each unique 'id' of the number of events. (events = death)
    by_id$censor <- by_id$removed+by_id$end       # removed indv and indv at end of experiment are considered censored data. 
    master_list <- rbind(master_list, by_id)      # compiles data from each 'id' into a single master df. 
  }
                                                  #expands the list. Ex: changing 'event: 6' into 6 lines of 'event: 6'
  expanded_event =c()
  for (i in 1:nrow(master_list)){
    row_event<- (master_list[i,])
    times<- as.numeric(row_event$event)
    if (times > 0){
      expanded<- row_event[rep(1, times),] 
      expanded_event <- rbind(expanded_event, expanded)
    }
  }
  expanded_event$censor<- rep(0, nrow(expanded_event))       #Same with censored data. 
  expanded_censor <- c()
  for (i in 1:nrow(master_list)){
    row_event<- (master_list[i,])
    times<- as.numeric(row_event$censor)
    if (times > 0){
      expanded<- row_event[rep(1, times),] 
      expanded_censor <- rbind(expanded_censor, expanded)
    }
  }
  expanded_censor$event <- rep(0, nrow(expanded_censor))
  
  expanded_list<- rbind(expanded_censor, expanded_event)        #Changes event data to 1 if > 0. So, 'event: 6' is now: 'event: 1' in 6 rows. 
  one_event <- c()
  for (i in 1:nrow(expanded_list)){
    row<- (expanded_list[i,])
    if (row$event > 0){
      one_event <- append(values=1,x=one_event)
    } else if (row$event == 0){
      one_event<- append(values=0,x=one_event)
    }
  }
  expanded_list$event <- one_event
  one_censor<- c()
  for (i in 1:nrow(expanded_list)){
    row<- (expanded_list[i,])
    if (row$censor > 0){
      one_censor <- append(values=1,x=one_censor)
    } else if (row$censor == 0){
      one_censor<- append(values=0,x=one_censor)
    }
  }
  expanded_list$censor <- one_censor
  return(expanded_list)
}

