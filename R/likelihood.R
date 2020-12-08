likelihood.multilevel_model <- function(
  model,
  theta,
  z,
  j
) {
  list2env(theta, envir = environment())
  y <- matrix(model$y[model$group==j])
  X <- matrix(model$X[model$group==j,], ncol = ncol(model$X))
  n_j <- model$n_j[j]
  ll <- (2*pi*phi)^(-n_j/phi) *
    exp( -(2*phi)^(-1) * crossprod(y-z-X %*% beta) )
  return(ll)
}

likelihood <- function(model, theta, z, j ) {
  UseMethod("likelihood")
}
