Q.multilevel_model <- function(model, theta, p) {
  n_j <- model$n_j
  list2env(theta, envir = environment())
  log_lik <- matrix(
    sapply(
      1:length(n_j),
      function(j) {
        y <- matrix(model$y[model$group==j])
        X <- matrix(model$X[model$group==j,], ncol = ncol(model$X))
        U <- model$U
        # Latent given previous theta:
        z_j <- z[j,] # estimate of z which comes from theta
        Z_M <- z
        N <- nrow(y)
        M <- nrow(Z_M)
        log_lik <- (-1/2) *
          ( N * log(2*pi*phi) + phi^(-1) * crossprod(y - z_j - X %*% beta)
            + M * log(2*pi*psi) + psi^(-1) * crossprod(Z_M - U %*% gamma) )
        return(log_lik)
      }
    )
  )
  return(t(log_lik) %*% p)
}

Q <- function(model, theta, p) {
  UseMethod("Q")
}
