bioassay.analyze <- function(data, x=treatment.name, y=dead, day=day, rep=rep, 
                             n=n, alive=alive, id=cup.ID, end=end, removed=removed, 
                             weight=FALSE, kaplan=TRUE, y_limit=1, ylabel='proportion survived', 
                             xlabel='treatment.name', type='binomial', start=0, ref='shrimp'){
  require(rlang)
  x <- ensym(x)
  y <- ensym(y)
  day <-ensym(day)
  rep <-ensym(rep) 
  n <- ensym(n) 
  alive <- ensym(alive) 
  id <-ensym(id) 
  end <-ensym(end) 
  removed <-ensym(removed)
  x_string <- as_string(x)
  y_string <- as_string(y)
  rep_string <- as_string(rep)
  n_string <- as_string(n)
  alive_string <- as_string(alive)
  id_string <- as_string(id)
  end_string <- as_string(end)
  removed_string <- as_string(removed)
  day_string <- as_string(day)
 
  
  model <- bioassay.model(data, x=!!sym(x_string), y=!!sym(y_string), day=!!sym(day_string), ref=ref)
  print(model)
  
  
  bar <- bioassay.plot_bar(data=data, x=!!sym(x_string), y=!!sym(y_string), day=!!sym(day_string), weight=weight, y_limit=y_limit, ylabel=ylabel, xlabel=xlabel)
  print(bar)
  
  box <- bioassay.plot_box(data=data, x=!!sym(x_string), y=!!sym(y_string), day=!!sym(day_string), weight=weight, ylabel=ylabel, xlabel=xlabel)
  print(box)
  
  line <- bioassay.plot_line(data=data, x=!!sym(x_string), y=!!sym(y_string), day=!!sym(day_string), ylabel=ylabel, xlabel='Day', start=start )
  print(line)
  if (kaplan==FALSE){
  print('Kaplan Passed')
  } else {
  kap_converted <- bioassay.kaplan_convert(data=data, x=!!sym(x_string), day=!!sym(day_string), id=!!sym(id_string), rep=!!sym(rep), removed=!!sym(removed_string), n=!!sym(n_string), alive=!!sym(alive_string), end=!!sym(end_string))
  kaplan <- bioassay.plot_kaplan(data=kap_converted, treatment.name=treatment.name, day=day, event=event, weight=weight)
  print(kaplan)
  }
  
}



