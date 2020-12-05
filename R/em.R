em.multilevel_model <- function(multilevel_model, theta0=NULL) {
  # Initial guess:
  if (is.null(theta0)) {
    beta0 <- qr.solve(X, y)[,1] # initialize as pooled OLS
    gamma0 <- c(0,1) # initialize as u
  }
}
