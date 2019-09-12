#1.1.1 euclidean()
euclidean <- function(x,y){
  r <- x%%y;
  return(ifelse(r, euclidean(y, r), y))
}


