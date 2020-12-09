
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Latent variable models

<!-- badges: start -->
<!-- badges: end -->

A brief exploration of a simple multilevel regression model as in
Gelman, Hill (2007). We will use the EM algorithm the uncover the latent
factors and fit the multilevel regression model. To do so we first need
to derive the *expected* complete data log-likelihood (CLL):

*Q*(*θ*, *θ*<sub>*t* − 1</sub>) = 𝔼<sub>*p*(**z**\|**y**, **X**, *θ*<sub>*t* − 1</sub>)</sub>\[logℓ(*θ*\|**y**,**X**,**z**)\]

All details can be found in the Jupyter notebook.
