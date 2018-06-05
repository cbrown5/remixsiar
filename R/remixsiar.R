#' Tools for model checking of Bayesian isotope mixing
#' models
#'
#' The \pkg{remixsiar} package provides functions for evaluation of Bayesian isotope
#' mixing models.
#' Refer to the vignette for more help by typing \code{vignette('remixsiar')}.
#' See also \href{https://link.springer.com/article/10.1007%2Fs00442-018-4138-y}{Brown et al. 2018 Quantifying learning in biotracer studies. Oecologia}
#' @section remixsiar vignettes: 
#'  \code{vignette('remixsiar')} for the basics
#'  \code{vignette('power-analysis')} for how to use this package to do power analysis
#'  \code{vignette('remixsiar-additional-methods')} for methods written up in appendix of the above paper. 
#' @section remixsiar functions  
#' To visualise prior and posterior distributions see:
#' \code{\link{default_prior}} for generating the simmr default prior.
#' \code{\link{drawpriors}} for drawing random variates from the prior.
#' \code{\link{plot_dists}} for plotting prior and posterior distributions.
#' To calculate statistics of prior influence on the posterior see:
#' \code{\link{remix_hellinger}} which calculates a metric of prior to
#' posterior divergence.
#' \code{\link{simmr_clone}} which re-runs a simmr model with cloned data,
#' to estimate maximum-likelihood values. This is neccessary for \{remix_shrink}
#' \code{\link{remix_shrink}} which calculates a metric of prior influence.
#' To examine model bias see:
#' \code{\link{simdata}} to simulate consumer isotope ratios from the posterior
#' distributions.
#' \code{\link{remix_2DPI}} to plot 2d predictive intervals of consumer isotope
#' ratios.
#' \code{\link{remix_PI}} to obtain predictive intervals on each tracer for
#' consumer isotope ratios and also metrics of model bias.
#' Some functions that work with the mixsiar package
#' \code{\link{mixsiar_fitsims}} to fit a simple mixsiar model to randomly generated data
#' \code{\link{calcinfo_mixsiar}} to calculate information metrics for a mixsiar model. 
#' @docType package
#' @name remixsiar
NULL
