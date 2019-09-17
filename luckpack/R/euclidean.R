#' The Euclidean Algorithm
#' 
#' @param x A numerical value
#' @param y A numerical value
#' @return The greatest common divisior of two numbers
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
#' @export
euclidean <- function(x,y){
  r <- x%%y;
  return(ifelse(r, euclidean(y, r), y))
}
