syn_multi_level_reg <- function(
  N=10000,
  M=1000,
  beta=c(1,0.1),
  phi=0.5,
  a=1,
  b=0.5,
  psi=0.7,
  seed=123
) {
  set.seed(seed)
  # 1.) Latent factors: ----
  u <- matrix(rnorm(M))
  z <- a + b * u + rnorm(M, sd=sqrt(psi))
  # Group lengths:
  wgts <- runif(M)
  wgts <- wgts/sum(wgts)
  n_j <- pmax(round(wgts * N),10)
  to_allocate <- N - sum(n_j)
  while (to_allocate!=0) {
    if (to_allocate > 0) {
      idx <- sample(M,1)
      n_j[idx] <- n_j[idx] + 1
      to_allocate <- to_allocate - 1
    } else {
      idx <- sample(M,1)
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
  y <- Z + X %*% beta + rnorm(N,sd=sqrt(phi))
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
