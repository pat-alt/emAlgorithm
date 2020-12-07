update_theta.multilevel_model <- function(model, theta, p) {
  n_j <- model$n_j
  list2env(theta, envir = environment())
  # beta: ----
  X <- model$X
  y <- model$y
  N <- nrow(y)
  M <- length(n_j)
  Z_N <- matrix(rep.int(z, times=n_j))
  beta_map <- qr.solve(crossprod(X), (crossprod(X,y) - crossprod(X, Z_N)))
  theta$beta <- beta_map # update
  # phi: ----
  wgts <- p/sum(p)
  e_sq <- matrix(
    sapply(
      1:length(n_j),
      function(j) {
        y <- matrix(model$y[model$group==j])
        X <- matrix(model$X[model$group==j,], ncol = ncol(model$X))
        z_j <- z[j,]
        e_sq <- crossprod(y - z_j - X %*% beta)
        return(e_sq)
      }
    )
  )
  phi_map <- (1/N) * sum(wgts * e_sq)
  theta$phi <- phi_map # update phi
  # gamma: ----
  Z_M <- z
  gamma_map <- qr.solve(crossprod(model$U), crossprod(model$U,Z_M))
  theta$gamma <- gamma_map # update gamma
  # psi: ----
  psi_map <- (1/M) * crossprod(Z_M - model$U %*% gamma_map)
  theta$psi <- psi_map # update psi
  return(theta)
}

update_theta <- function(model, theta, p) {
  UseMethod("update_theta")
}
