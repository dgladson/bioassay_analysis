bioassay.validate <- function(data, id=cup.ID, alive=alive, removed=removed){
  require(rlang)
 
  # enquote
  id <- ensym(id)
  alive <- ensym(alive)
  removed <- ensym(removed)
  diffs <-  c() # creates new list
  result <- 'PASS' #sets result to 'pass' as default
  for (i in unique(data[[id]])){ # for each cup, create a list of the differences between the days
    sub_id <- subset(data,  data[[id]] == i)
    id_len <- nrow(sub_id)
    diffs <- c() #clears diffs for each id
    for (d in 2:id_len){ 
      diffs<-append(values=(sub_id$alive[d-1] - (sub_id$alive[d] + sub_id$removed[d-1])), x=diffs)
    }
    for (x in diffs){ #for each item in the list of differences, check to see if there are any negative numbers. 
      if (x < 0){
        result = 'VALIDATION FAILED: SURVIVAL INCREASES OVER TIME: CHECK DATA ENTRY AND RE-IMPORT DATAFRAME'
        print(result)
        print(sub_id)
      } 
    }
  }
  
  if (result == 'PASS') {
  }
  return(result)
}