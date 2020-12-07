infer_latent.multilevel_model <- function(model, theta) {
  n_j <- model$n_j
  list2env(theta, envir = environment())
  z <- model$U %*% gamma
  return(z)
}

infer_latent <- function(model, theta) {
  UseMethod("infer_latent")
}
