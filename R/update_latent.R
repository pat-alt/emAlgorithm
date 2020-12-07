update_latent.multilevel_model <- function(model, theta) {
  n_j <- model$n_j
  list2env(theta, envir = environment())
  z <- matrix(
    sapply(
      1:length(n_j),
      function(j) {
        U <- matrix(model$U[j,], ncol = ncol(model$U))
        mu_z <- U %*% gamma
        z_j <- rnorm(1,mean = mu_z,sd = sqrt(psi))
        return(z_j)
      }
    )
  )
  return(z)
}

update_latent <- function(model, theta) {
  UseMethod("update_latent")
}
