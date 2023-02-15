
utils.rename <- function(list=NULL, clipboard=TRUE, prefix='t'){       # creates dictionary for bioassay.analyze_consumption parameter: treatment.rename()
  if ( is.null(list)& clipboard==TRUE){                                   # optional method of using clipboard to pass list
  rename.list <- read.delim('clipboard', header = F)
  } else{
  rename.list <- list
  }
  new_dict <- c()
  for (i in 1:nrow(rename.list)){                           # creates dictionary using prefix and the order number of each element. 
    key <- paste(prefix, i, sep="")
    value = rename.list[i,]
    new_dict[key] = value
  }
    invisible(new_dict)                                   # intended to be used with bioassay.analyze_consumption to rename 
}                                                         # values from PlantCV json files from 't1' etc. to actual treatment names.

                                                          # examples use: 
                                                          # results <- bioassay.analyze_consumption(data, 
                                                          # treatment.rename = utils.rename())
  
