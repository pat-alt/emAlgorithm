sim_categorical <- function(n, prob) {
  k <- length(prob)
  # Normalise prob if necessary:
  if (sum(prob)!=1) {
    prob <- prob/sum(prob)
  }
  emp_cdf <- cumsum(prob)
  U <- runif(n)
  X <- vapply(
    1:n,
    function(i) {
      col <- rep(0,k)
      col[binary_search_rightmost(x=emp_cdf, target=U[i])] <- 1
      return(col)
    },
    rep(0,k)
  )
  return(X)
}
