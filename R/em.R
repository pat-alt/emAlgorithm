# EM algorithm:
em.multilevel_model <- function(
  model,
  theta0=NULL,
  tol=1e-9
) {
  # Initialization: ----
  if (is.null(theta0)) {
    theta0 <- list(
      beta = qr.solve(model$X, model$y), # initialize as pooled OLS
      phi = runif(1, 0, 100),
      gamma = matrix(c(0.1,1)),
      psi = runif(1, 0, 100)
    )
  }
  converged <- FALSE # initialize convergence condition as false
  # Recursion: ----
  while (!converged) {
    # 1.) E-step: ----
    p <- posterior(model, theta0) # returns and (M x 1) vectors of posteriors
    Q0 <- Q(model, theta0, p) # to compare below
    # 2.) M-step: ----
    theta <- update_theta(model, theta0, p)
    # Recalculate given MAP parameter estimates:
    Q1 <- Q(model, theta, p)
    # Check for convergence:
    converged <- Q1-Q0 < tol
    theta0 <- theta
  }
  # Allocate and return output:
  output <- list(
    model = multilevel_model,
    theta = theta
  )
  class(output) <- "em_output"
  return(output)
}

em <- function(multilevel_model, theta0=NULL) {
  UseMethod("em")
}

print.em_output <- function(output) {
  cat(sprintf("Coefficients are: %0.2f", 0.01))
}
