syn_multi_level_reg <- function(
  N=10000,
  M=1000,
  beta=c(1,0.1),
  phi=0.1,
  a=1,
  b=0.5,
  psi=0.05,
  seed=123
) {
  set.seed(seed)
  # 1.) Latent factors: ----
  u <- matrix(rnorm(M))
  z <- a + b * u + rnorm(n=nrow(u),sd=sqrt(psi))
  # Group lengths:
  wgts <- runif(M)
  wgts <- wgts/sum(wgts)
  n_j <- pmax(round(wgts * N),1)
  to_allocate <- N - sum(n_j)
  while (to_allocate!=0) {
    if (to_allocate > 0) {
      idx <- which.min(n_j)
      n_j[idx] <- n_j[idx] + 1
      to_allocate <- to_allocate - 1
    } else {
      idx <- which.max(n_j)
      n_j[idx] <- n_j[idx] - 1
      to_allocate <- to_allocate + 1
    }
  }
  Z <- rep(0, N)
  from <- 1
  for (j in 1:M) {
    idx <- from:(from+n_j[j]-1)
    Z[idx] <- z[j]
    from <- from + n_j[j]
  }
  # 2.) Output: ----
  K <- length(beta)
  X <- matrix(rnorm(N*K), nrow=N)
  y <- Z + X %*% beta + rnorm(n=nrow(X),sd=sqrt(phi))
  model <- list(
    data = list(
      X = X,
      y = y,
      u = u,
      n_j = n_j
    ),
    z = z,
    M = M,
    N = N,
    params = list(
      beta = beta,
      phi = phi,
      psi = psi,
      a = a,
      b = b
    )
  )
  return(model)
}

syn_data <- syn_multi_level_reg()
