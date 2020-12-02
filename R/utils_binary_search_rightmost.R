binary_search_rightmost <- function(x, target) {
  x <- sort(x)
  n <- length(x)
  l <- 0
  r <- n
  while (l < r) {
    m <- floor((l+r)/2)
    if (x[m+1] > target) {
      r <- m
    } else {
      l <- m+1
    }
  }
  return(r+1)
}
