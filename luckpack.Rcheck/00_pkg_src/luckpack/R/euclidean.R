#' Create E Alg
#' 
#' @param x A numberical value
#' @param y A numberical value
#' @return The greatest common divisior
#' @examples
#' euclidean(1, 1)
#' euclidean(10, 1)
#' @export
euclidean <- function(x,y){
  r <- x%%y;
  return(ifelse(r, euclidean(y, r), y))
}


