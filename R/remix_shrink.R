#' Calculate posterior shrink on parameter estimates
#'
#' Posterior shrink is an estimate of how much the priors influence a posterior
#' parameter estimate.  This function implements the shrink equation for
#' \code{simmr_output} objects
#'
#' @Usage remix_shrink(simmr_out, clone_out, prior.control, stat = mean)
#'
#' @param simmr_out A \code{simmr_output} object, the output of simmr_mcmc
#' @param clone_out A \code{simmr_output} object using cloned data from
#' simmr_out
#' @param prior.control A \code{prior.control} data.frame for simmr_mcmc
#' @param stat A function name, giving the parameter you want to calculate
#' shrink on. e.g. mean, median, .est_mode.
#'
#' @return A \code{remix_shrink} object containing parameter estimates and
#' the shrink estimate. Specifically \code{remix_shrink} contains:
#' \item{dat}{A \code{data.frame} with the shrink estimate, the prior, ML and
#' posterior parameter estimates and the standard-error on the ML estimate.}
#' \item{stat}{returns the parameter used.}
#' \item{K}{returns the number of data clones.}
#'
#' @details Posterior shrinkage measures the degree to which the posterior
#'estimate has shrunk towards the maximum likelihood estimate and away from the
#'prior.
#'
#'Values close to 1 indicate the prior has little influence on the
#'posterior, whereas values close to 0 indicate the prior has a large
#'influence on the posterior.
#'
#' Some care must be taken in selecting parameter estimates to use in the
#'shrink equation and also in estimating the MLE (which is not always
#'straightfoward). For complex models the MLE may be estimated using data
#'cloning, see \code{\link{simmr_clone}}.
#'
#' Shrink values can occaisionally be >1 or <0. Values <0 occur when
#'the posterior parameter estimate has moved in the opposite direction from
#'the prior than the MLE.
#' Values >1 occur when the MLE is closer to the prior estimate than
#' the posterior.
#' Values not in 0-1 can occur if (1) your MLE estimate
#'is inaccurate, (2) your posterior is multi-model, or (3) your posterior
#'estimate is constrained by other parameters. If (1) then try other methods
#'for obtaining an MLE or increase replication if using data cloning.
#'If (2) or (3) posterior shrink may be inappropriate for your model, because
#'the posterior shrink cannot be characterised by a simple univariate measure.
#'
#' See: Berger JO (1985) Statistical Decision Theory and Bayesian Analysis,
#' Second Edition, Springer, New York.
#'
#' Thanks to Ed Boone for suggesting this one.
#' @author Christopher J. Brown christo.j.brown@gmail.com
#' @rdname remix_shrink
#' @export

remix_shrink <- function(simmr_out, clone_out, prior.control, nparms = simmr_out$input$n_sources, stat = mean){

	priors <- drawpriors(simmr_out $input, priorcontrol = prior.control, plot.priors = F)
	
	K <- nrow(clone_out$input$mixtures)/nrow(simmr_out$input$mixtures)

	theta_MLE <- .extract_stat(clone_out, stat)
	theta_post <- .extract_stat(simmr_out, stat)
	theta_prior <- apply(priors, 2,stat)

	theta_SE <- sqrt(.extract_stat(clone_out, var)*K)

	 alpha <- postshrink(theta_prior, theta_post, theta_MLE)

	 dat <- data.frame(shrink = alpha,
	 theta_post = theta_post,
	 theta_prior = theta_prior,
	 theta_MLE = theta_MLE,
	 theta_SE = theta_SE)

	 sout <- list(dat =dat,
	 stat = substitute(stat),
	 K = K)

	 class(sout) <- "remix_shrink"
	 return(sout)

	}
