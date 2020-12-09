## --------------------- EM algorithm: --------------------- ##
em.multilevel_model <- function(
  model,
  theta0=NULL,
  tol=1e-9,
  print_progress=T,
  max_iter=1000
) {
  # Initialization: ----
  if (is.null(theta0)) {
    theta0 <- list(
      beta = qr.solve(model$X, model$y), # initialize as pooled OLS
      phi = runif(1, 0, 100),
      gamma = matrix(rnorm(ncol(model$U))),
      psi = runif(1, 0, 100)
    )
  }
  converged <- FALSE # initialize convergence condition as false
  iter_count <- 1
  # Recursion: ----
  while (!converged & iter_count <= max_iter) {
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
    coefficients = theta,
    n_iter = iter_count - 1
  )
  class(output) <- "em_output"
  return(output)
}

em <- function(
  model,
  theta0=NULL,
  tol=1e-9,
  print_progress=T,
  max_iter=100
) {
  UseMethod("em")
}
