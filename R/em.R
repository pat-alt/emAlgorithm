em.multilevel_model <- function(multilevel_model, theta0=NULL) {
  # Initialization: ----
  if (is.null(theta0)) {
    beta0 <- qr.solve(X, y)[,1] # initialize as pooled OLS
    gamma0 <- c(0,1) # initialize as u
  }
  converged <- FALSE
  # Recursion: ----
  while (!converged) {
    # 1.) E-step: ----

    # 2.) M-step: ----
  }
  output <- list()
  class(output) <- "em_output"
  return(output)
}

em <- function(multilevel_model, theta0=NULL) {
  UseMethod("em")
}

print.em_output <- function(output) {
  cat(sprintf("Coefficients are: %0.2f", 0.01))
}
