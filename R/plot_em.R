## --------------------- Plotting: --------------------- ##
# Various plot methods for EM output.

plot_posterior.em_output <- function(em_output, groups=NULL, n_draws=1000) {
  n_j <- em_output$model$n_j
  M <- length(n_j)
  if(is.null(groups)) {
    qntls <- quantile(n_j)
    groups <- sapply(
      qntls,
      function(i) {
        sample(which(n_j==i),1)
      }
    )
  }
  # Get posterior moments:
  posterior_moments <- posterior(em_output$model, em_output$coefficients)
  posterior_moments <- data.table(
    cbind.data.frame(posterior_moments[groups,], type="posterior", n_j=qntls, group=groups)
  )
  # Get prior moments:
  prior_moments <- prior(em_output$model, em_output$coefficients)
  prior_moments <- data.table(
    cbind.data.frame(prior_moments[groups,], type="prior", n_j=qntls, group=groups)
  )
  # Join:
  dt_plot <- rbind(posterior_moments, prior_moments)
  dt_plot <- dt_plot[,.(z=rnorm(n=n_draws, mean = mu, sd=sqrt(v))),by=.(type,n_j,group)]
  dt_plot[,density:=density(z,n=.N)$y,by=.(type,group)]
  dt_plot[,z:=density(z,n=.N)$x,by=.(type,group)]
  # Get likelihood:
  likeli <- dt_plot[,.(z=seq(min(dt_plot$z),max(dt_plot$z),length.out=n_draws)),by=.(group,n_j)]
  likeli[,density:=likelihood(em_output$model, em_output$coefficients, z, group),by=.(group,z)]
  likeli[,type:="likelihood"]
  dt_plot <- rbind(dt_plot, likeli)
  dt_plot[,density:=(density/sum(density)),by=.(type, group)] # rescale for chart
  dt_plot[,type:=factor(type, level=c("prior", "likelihood", "posterior"))]
  gg <- ggplot2::ggplot(data=dt_plot, ggplot2::aes(x=z, y=density)) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(type),
      cols = ggplot2::vars(n_j),
      scales = "free_y"
    ) +
    ggplot2::labs(
      x="Latent variable",
      y="Density"
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
  return(gg)
}

plot_posterior <- function(em_output, groups=NULL, n_draws=1000) {
  UseMethod("plot_posterior")
}
