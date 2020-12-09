#' Multilevel model setup
#'
#' @details Sets up the multilevel model
#'
#' @param X
#' @param y
#' @param u
#' @param var_posterior
#' @param mean_posterior
#' @param focs
#' @param n_j
multilevel_model <- function(
  X,
  y,
  u,
  n_j
) {
  # Group index:
  group <- rep.int(1:nrow(u), times=n_j)
  U <- cbind(1, u)
  # u <- matrix(rep.int(u, times=n_j))
  u <- u[group,]
  model <- list(
    X=X,
    y=y,
    u=u,
    U=U,
    group=group,
    n_j=n_j
  )
  class(model) <- "multilevel_model"
  return(model)
}
