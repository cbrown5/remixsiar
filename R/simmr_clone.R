#' Clone a data-frame then run a simmr model
#'
#' Data cloning can be used to obtain maximum-likelihood estimates
#' of parameters from Bayesian MCMC models. See details.
#'
#' @Usage simmr_clone(dat, K = 20, mcmc.control, prior.control)
#'
#' @param dat A \code{list} containing the inputs for
#' \code{simmr::simmr_load} including the elements named
#' \code{mixes}, \code{s_names}, \code{source_means},
#' \code{source_sds}.
#' @param K A \code{numeric} integer giving the number of data-clones.
#' @param mcmc.control A \code{list} specifying the parameters for the
#' mcmc algorithm, including \code{iter}, \code{burn}, \code{thin},
#' \code{n.chain}. See the \code{simmr} documentation for more details.
#' @param prior.control A \code{list} specifying the parameters for the
#' models' priors, including \code{means} and \code{sds}. To
#' generate default priors see \code{\link{default_prior}}
#'
#' @return A \code{simmr::simmr_output} object. See the documentation
#' of that package for more details.
#'
#' @details Data cloning is a legitimate method for obtaining maximum-likelihood
#' estimates of parameters from Bayesian models. This may be useful for
#' estimating prior influence (see \code{\link{remix_shrink}}) or if
#' frequentist statistcs are desired instead of Bayesian statistics.
#'
#' It is very important to choose a large enough value of \code{K}. It is
#' recommended to run cloning with various K values and check parameter
#' estimates have converged.
#'
#' This algorithm may take considerable time to run if your data-set
#' is large.
#'
#' For more details, see:
#' Lele SR, Dennis B, Lutscher F. Data cloning: easy maximum likelihood
#'estimation for complex ecological models using Bayesian Markov chain Monte
#' Carlo methods. Ecology letters. 2007 Jul 1;10(7):551-63.
#'
#'
#' @author Christopher J. Brown christo.j.brown@gmail.com
#' @rdname simmr_clone
#' @export

simmr_clone <- function(dat, K = 20, mcmc.control, prior.control){

	print('Grab a cuppa - Cloning your data, MCMC runs may take a while with large data-sets', .immediate = T)
	datclone <- dat
	datclone$mixes <- dataclone(dat$mixes, K = K)

	clonedf <- simmr_load(mixtures= datclone$mixes,
                       source_names= datclone$s_names,
                       source_means= datclone$source_means,
                       source_sds= datclone$source_sds)

	clone_out <- simmr_mcmc(clonedf, prior.control = prior.control, mcmc.control = mcmc.control)

	return(clone_out)
}
