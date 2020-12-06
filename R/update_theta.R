update_theta.multilevel_model <- function(model, theta, p) {
  n_j <- model$n_j
  list2env(theta, envir = environment())
  # beta: ----
  X <- model$X
  y <- model$y
  Z <- cbind(1,model$u) %*% gamma
  beta_map <- qr.solve(crossprod(X), (crossprod(X,y) - crossprod(X, Z)))
  # phi: ----
  wgts <- p/sum(p)
  e_sq <- matrix(
    sapply(
      1:length(n_j),
      function(j) {
        y <- matrix(model$y[model$group==j])
        X <- matrix(model$X[model$group==j,], ncol = ncol(model$X))
        z_j <- as.numeric(matrix(model$U[j,], ncol = ncol(model$U)) %*% gamma)
        e_sq <- crossprod(y - z_j - X %*% beta)
        return(e_sq)
      }
    )
  )
  phi_map <- (1/N) * sum(wgts * e_sq)
  # gamma: ----
  Z_M <- model$U %*% gamma
  gamma_map <- qr.solve(crossprod(model$U), crossprod(model$U,Z_M))
  # psi: ----
  psi_map <- (1/M) * crossprod(Z_M - model$U %*% gamma_map)
  # First order conditions ----
  theta <- list(
    beta = beta_map,
    phi = phi_map,
    gamma = gamma_map,
    psi = psi_map
  )
  return(theta)
}

update_theta <- function(model, theta, p) {
  UseMethod("update_theta")
}
