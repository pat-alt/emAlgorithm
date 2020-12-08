prior.multilevel_model <- function(
  model,
  theta
) {
  prior_mean <- model$U %*% theta$gamma
  p <- cbind.data.frame(v=theta$psi, mu=prior_mean)
  return(p)
}

prior <- function(model, theta) {
  UseMethod("prior")
}
