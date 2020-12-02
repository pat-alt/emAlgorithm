sim_categorical_fl <- function(n, prob) {
  k <- length(prob)
  # Normalise prob if necessary:
  if (sum(prob)!=1) {
    prob <- prob/sum(prob)
  }
  emp_cdf <- cumsum(prob)
  # Matrix for 1-hot-encoding:
  X <- matrix(rep(0, n*k), nrow = k, ncol = n)
  U <- runif(n)
  for (j in 1:n) {
    i <- binary_search_rightmost(x=emp_cdf, target=U[j])
    X[i,j] <- 1
  }
  return(X)
}


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
