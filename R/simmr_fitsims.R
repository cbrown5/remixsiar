#' Fits a simmr model to a randomly generated data-set
#'
#' Useful for obtaining fits from a sequence of randomly simulated datasets.
#' For instance, you could use this function to perform a sort of broad-sense
#' power analysis to ask how many samples are required to obtain a specific
#' confidence about an animal's diet.  
#'
#' @Usage simmr_fitsims(n, tracer_means, tracer_sds, snames, smeans, ssds,
#' sdmult = 1, seed = 42,
#' mcmc.control = list(iter = 50000, burn = 2000, thin = 10, n.chain = 4),
#' prior.control = NULL)
#'
#' @param n A \code{integer} giving the sample size to simulate.
#' @param tracer_means  A \code{numeric} vector giving means of the mixes.
#' @param tracer_sds  A \code{numeric} vector giving SDs of the mixes.
#' @param snames  A \code{character} giving source names.
#' @param smeans  A \code{numeric} vector giving means of the sources
#' @param ssds  A \code{numeric} vector giving SDs of the mixes
#' @param sdmult  A \code{numeric} that is multiplied by source SDs
#' @param sdmult  A \code{numeric} giving the random seed
#' @param mcmc.control  A list giving parameters for the MCMC chains.
#' @param priorcontrol A list with the means and standard deviations of the
#' priors.
#'
#' @return A list where the first object is a \code{simmr_input} object
#' giving the simulated data and the second object is a \code{simmr_ouptut}
#' object giving the model fit.
#'
#' @details
#' The mixes and sampled randomly from normal distributions.
#' Make sure the means and SDs of mixes and sources are given in the same
#' order (e.g. Carbon then Nitrogen for all of them).
#' Fixing the seed is useful if you want to compare runs across different
#' SDs, because the random samples will have the same pattern deviations.
#' See \code{simmr::simmr_load} \code{simmr::simmr_mcmc} for more details of
#' how the data is fit.
#' @author Christopher J. Brown
#' @rdname simmr_fitsims
#' @export


simmr_fitsims <- function(n, tracer_means, tracer_sds, snames, smeans, ssds, sdmult = 1, seed = 42,
 mcmc.control = list(iter = 50000, burn = 2000, thin = 10, n.chain = 4),
prior.control = NULL){

    set.seed(seed)
    mixes <- purrr::map2(tracer_means, tracer_sds, ~rnorm(n, .x, .y))
    mixes <- do.call(cbind, mixes)


	ssds2 <- ssds * sdmult

	dat <- list(mixes = mixes, s_names = snames, source_means = smeans, source_sds = ssds2)
	dat_in <- simmr_load(mixtures=dat$mixes,
                       source_names=dat$s_names,
                       source_means=dat$source_means,
                       source_sds=dat$source_sds)

    if (is.null(prior.control)){
        prior.control <- default_prior(dat_in)
    }

	dat_out <- simmr_mcmc(dat_in, prior.control = prior.control, mcmc.control = mcmc.control)

	return(list(dat_in = dat_in, dat_out = dat_out))
}
