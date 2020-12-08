rsquared <- function(y,fitted) {
  rss <- sum((fitted - y)^2)
  tss <- sum((y-mean(y))^2)
  rsq <- 1 - rss/tss
  return(rsq)
}
