## --------------------- Standard methods: --------------------- ##
predict.em_output <- function(em_output) {
  # Predictors: ----
  X <- em_output$model$X
  beta <- em_output$coefficients$beta
  # Latent variable estimates: ----
  Z_M <- infer_latent(em_output$model, em_output$coefficients)
  n_j <- em_output$model$n_j
  Z_N <- rep.int(Z_M, n_j)
  # Fit: ----
  fitted <- Z_N + X %*% beta
  return(fitted)
}

predict <- function(em_output) {
  UseMethod("predict")
}

residuals.em_output <- function(em_output) {
  y <- em_output$model$y
  fitted <- predict(em_output)
  res <- y - fitted
  return(res)
}

residuals <- function(em_output) {
  UseMethod("residuals")
}

coefficients.em_output <- function(em_output) {
  beta <- em_output$coefficients$beta
  if (!is.null(colnames(em_output$model$X))) {
    beta_names <- colnames(em_output$model$X)
  } else {
    beta_names <- sprintf("X_%i", 1:length(beta))
  }
  Z_M <- infer_latent(em_output$model, em_output$coefficients)
  Z_M_names <- sprintf("z_%i", 1:nrow(Z_M))
  coeffs <- data.table::data.table(
    Covariate = c(beta_names, Z_M_names),
    Coefficient = c(beta, Z_M)
  )
  return(coeffs)
}

coefficients <- function(em_output) {
  UseMethod("coefficients")
}

rsquared.em_output <- function(em_output) {
  y <- em_output$model$y
  fitted <- predict(em_output)
  rss <- sum((fitted - y)^2)
  tss <- sum((y-mean(y))^2)
  rsq <- 1 - rss/tss
  return(rsq)
}

rsquared <- function(em_output) {
  UseMethod("rsquared")
}

print.em_output <- function(em_output) {
  cat("Coefficient estimates: -----\n")
  print(coefficients(em_output))
  cat("Precision estimates: -----\n")
  cat(sprintf("Data: %0.2f\n",(em_output$coefficients$phi)^(-1)))
  cat(sprintf("Latent: %0.2f\n",(em_output$coefficients$psi)^(-1)))
  cat(sprintf("R-squared: %0.2f\n", rsquared(em_output)))
  cat("Performance: -----\n")
  cat(sprintf("Number of iterations: %0.2f\n", em_output$n_iter))
}
