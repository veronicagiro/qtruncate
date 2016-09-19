intersect_args <- function(x , y){
  
  x[intersect(names(x), names(y))]  <-  y[intersect(names(x), names(y))]
  x
}
