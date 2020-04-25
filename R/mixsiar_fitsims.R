#' Fits a MixSIAR model to a randomly generated data-set
#'
#' Useful for obtaining fits from a sequence of randomly simulated datasets.
#' For instance, you could use this function to perform a sort of broad-sense
#' power analysis to ask how many samples are required to obtain a specific
#' confidence about an animal's diet.
#'
#' @usage mixsiar_fitsims <- function(n, tracer_means, tracer_sds,
#'    snames, modelfile, mixsiardat, sdmult = 1, seed = 42,
#'    mcmc.control = list(iter = 50000, burn = 2000, thin = 10, n.chain = 4),
#'    prior.control = NULL)
#'
#' @param n A \code{integer} giving the sample size to simulate.
#' @param tracer_means  A \code{numeric} vector giving means of the mixes.
#' @param tracer_sds  A \code{numeric} vector giving SDs of the mixes.
#' @param snames  A \code{character} giving source names.
#' @param modelfile  A \code{character} giving the location of the MixSIAR
#' model filename.
#' @param mixsiardat  A \code{list} giving MixSIAR input data that will be
#' modified for the re-run (should include objects mix, source, and disc, see
#' the MixSIAR manual for further info).
#' @param sdmult  A \code{numeric} that is multiplied by source SDs
#' @param mcmc.control  A list giving parameters for the MCMC chains.
#' @param priorcontrol A list with the means and standard deviations of the
#' priors.
#'
#' @return A list where the first object gives the simulated data and the
#' second object is a \code{rjags} object giving the model fit.
#'
#' @details
#' The mixes and sampled randomly from normal distributions.
#' Fixing the seed is useful if you want to compare runs across different
#' SDs, because the random samples will have the same pattern deviations.
#' @author Christopher J. Brown
#' @rdname mixsiar_fitsims
#' @export

mixsiar_fitsims <- function(n, tracer_means, tracer_sds, snames, modelfile, mixsiardat, sdmult = 1, seed = 42,
 mcmc.control = list(iter = 50000, burn = 10000, thin = 10, n.chain = 4),
prior.control = NULL){

    run <- list(chainLength = mcmc.control$iter, burn = mcmc.control$burn, thin = mcmc.control$thin, chains = mcmc.control$n.chain, calcDIC = FALSE)
    set.seed(seed)
    mixes <- purrr::map2(tracer_means, tracer_sds, ~rnorm(n, .x, .y))
    mixes <- do.call(cbind, mixes)
    dimnames(mixes)[[2]] <- dimnames(mixsiardat$mix$data_iso)[[2]]

    mix <- mixsiardat$mix
    mix$N <- n
    mix$data_iso <- mixes
    mix$data <- data.frame(mixes)

    source <- mixsiardat$source
    source$S_SIG <- mixsiardat$source$S_SIG * sdmult
    source$SIG2_array <- source$S_SIG^2

    dat_in <- list(mix = mix, source = source)

    if (is.null(prior.control)){
        prior.control <- 1
    }

	dat_out <- MixSIAR::run_model(run=run, mix, source, mixsiardat$disc, modelfile, alpha.prior = prior.control, mixsiardat$resid_err, mixsiardat$process_err)

	return(list(dat_in = dat_in, dat_out = dat_out))
}
