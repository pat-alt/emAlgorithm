update_theta.multilevel_model <- function(model, theta, p) {
  # Gather:
  n_j <- model$n_j
  list2env(theta, envir = environment())
  X <- model$X
  y <- model$y
  N <- nrow(y)
  M <- length(n_j)
  U <- model$U
  # Posterior moments:
  mu_M <- matrix(p[,"mu"])
  v_M <- matrix(p[,"v"])
  mu_N <- matrix(rep.int(mu_M, times=n_j))
  v_N <- matrix(rep.int(v_M, times=n_j))
  # beta: ----
  beta_map <- qr.solve(crossprod(X), (crossprod(X,y) - crossprod(X, mu_N)))
  theta$beta <- beta_map # update
  # phi: ----
  phi_map <- (1/N) *
    (crossprod(y-X%*%beta_map) - 2 * crossprod(y - X %*% beta_map, mu_N) + sum( mu_N^2 + v_N ))
  theta$phi <- phi_map # update phi
  # gamma: ----
  gamma_map <- qr.solve(crossprod(U), crossprod(U,mu_M))
  theta$gamma <- gamma_map # update gamma
  # psi: ----
  psi_map <- (1/M) *
    (crossprod(U %*% gamma_map) + sum( mu_M^2 + v_M ) - 2 * crossprod(U %*% gamma_map, mu_M))
  theta$psi <- psi_map # update psi
  return(theta)
}

update_theta <- function(model, theta, p) {
  UseMethod("update_theta")
}
