posterior.multilevel_model <- function(
  model,
  theta
) {
  n_j <- model$n_j
  list2env(theta, envir = environment())
  # Posterior density ----
  var_posterior <- function(phi, psi, n_j) {
    v <- (n_j*phi^(-1) + psi^(-1))^(-1)
    return(v)
  }
  # Posterior mean ----
  mean_posterior <- function(phi, psi, n_j, v, mu_a, mu_b) {
    v * n_j * phi^(-1) * (mu_a) +
      v * psi^(-1) * (mu_b)
  }
  # Compute density ----
  p <- t(
    sapply(
      1:length(n_j),
      function(j) {
        y <- matrix(model$y[model$group==j])
        X <- matrix(model$X[model$group==j,], ncol = ncol(model$X))
        U <- matrix(model$U[j,], ncol = ncol(model$U))
        if (nrow(X) > 1) {
          mu_a <- mean(y) - colMeans(X) %*% beta
        } else {
          mu_a <- mean(y) - X %*% beta
        }
        mu_b <- unique(U) %*% gamma
        # Variance:
        v <- var_posterior(phi, psi, n_j[j])
        # Mean:
        mu <- mean_posterior(phi, psi, n_j[j], v, mu_a, mu_b)
        # Density:
        return(c(v=v, mu=mu))
      }
    )
  )
  return(p)
}

posterior <- function(model, theta) {
  UseMethod("posterior")
}
