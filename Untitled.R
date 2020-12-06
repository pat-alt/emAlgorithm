# Posterior density ----
var_posterior <- function(phi, psi, n_j) {
  v <- (n_j*phi^(-1) + psi^(-1))^(-1)
  return(v)
}

mean_posterior <- function(phi, psi, n_j, v, mu_a, mu_b) {
  v * n_j * phi^(-1) * (mu_a) +
    v * psi^(-1) * (mu_b)
}

posterior_density <- function(phi, psi, n_j, mu_a, mu_b) {
  # Variance:
  v <- var_posterior(phi, psi, n_j)
  # Mean:
  mu <- mean_posterior(phi, psi, n_j, v, mu_a, mu_b)
  # Density:
  (2*pi*v)^(-1/2) * exp( (-2*v)^(-1) * crossprod(z-mu) )
}
