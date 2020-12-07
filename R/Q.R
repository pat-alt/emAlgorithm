Q.multilevel_model <- function(model, theta, p) {
  n_j <- model$n_j
  list2env(theta, envir = environment())
  y <- model$y
  X <- model$X
  U <- model$U
  N <- nrow(y)
  M <- length(n_j)
  # Posterior moments:
  mu_M <- matrix(p[,"mu"])
  v_M <- matrix(p[,"v"])
  mu_N <- matrix(rep.int(mu_M, times=n_j))
  v_N <- matrix(rep.int(v_M, times=n_j))
  # Compute:
  A <- N * log(2*pi*phi)
  B <- phi^(-1) * ( crossprod(y - X %*% beta) - 2 * crossprod(y - X %*% beta, mu_N) + sum( mu_N^2 + v_N ) )
  C <- M * log(2*pi*psi)
  D <- psi^(-1) * ( crossprod(U %*% gamma) + sum( mu_M^2 + v_M ) - 2 * crossprod(U %*% gamma, mu_M))
  value <- (-1/2) * ( A + B + C + D )
  return(value)
}

Q <- function(model, theta, p) {
  UseMethod("Q")
}
