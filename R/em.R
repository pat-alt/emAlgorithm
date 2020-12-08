## --------------------- EM algorithm: --------------------- ##
em.multilevel_model <- function(
  model,
  theta0=NULL,
  tol=1e-9,
  print_progress=T
) {
  # Initialization: ----
  if (is.null(theta0)) {
    theta0 <- list(
      beta = qr.solve(model$X, model$y), # initialize as pooled OLS
      phi = runif(1, 0, 100),
      gamma = matrix(rnorm(2)),
      psi = runif(1, 0, 100)
    )
  }
  converged <- FALSE # initialize convergence condition as false
  iter_count <- 1
  # Recursion: ----
  while (!converged) {
    if (print_progress) {
      print(iter_count)
    }
    # 1.) E-step: ----
    posterior_moments <- posterior(model, theta0) # returns and (M x 1) vectors of posteriors
    Q0 <- Q(model, theta0, posterior_moments) # to compare below
    # 2.) M-step: ----
    theta <- update_theta(model, theta0, posterior_moments)
    # Recalculate given MAP parameter estimates:
    Q1 <- Q(model, theta, posterior_moments)
    # Check for convergence:
    if (print_progress) {
      print(sprintf("Improvement of %0.2f",Q1-Q0))
    }
    converged <- Q1-Q0 < tol
    theta0 <- theta # new theta 0
    iter_count <- iter_count + 1
  }
  # Allocate and return output:
  output <- list(
    model = model,
    coefficients = theta
  )
  class(output) <- "em_output"
  return(output)
}

em <- function(model, theta0=NULL) {
  UseMethod("em")
}

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
  cat("Coefficient estimates:\n")
  print(coefficients(em_output))
  cat("Precision estimates:\n")
  cat(sprintf("Data: %0.2f\n",(em_output$coefficients$phi)^(-1)))
  cat(sprintf("Latent: %0.2f\n",(em_output$coefficients$psi)^(-1)))
  cat(sprintf("R-squared: %0.2f\n", rsquared(em_output)))
}
