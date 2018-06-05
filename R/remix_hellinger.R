#' Estimate the Hellinger distance between priors and posteriors
#'
#' @Usage remix_hellinger(simmr_out, prior.control, plot.dens = TRUE)
#'
#' @param simmr_out A \code{simmr_output} object, the output of simmr_mcmc
#' @param prior.control A \code{prior.control} data.frame for simmr_mcmc
#' @param plot.dens A \code{logical} giving whether densities are plotted.
#'
#' @return A \code{data.frame} containing the Hellinger distances between
#' each source's prior and posterior distributions.
#'
#' @details Hellinger distance is a metric of the distance between two
#' distributions (densities).  Values of 0 indicate the distributions
#'are identical. A value of 1 indicates that one distribution takes a value
#' of zero everywhere that the other distribution takes a positive value.
#'
#' For isotope mixing models, values approaching 1 indicate the prior has
#' decreasing influence on the posterior.
#' In general
#'
#' Hellinger distance is approximated in two ways:
#'
#'(1) by binning the random variates and calculating the Hellinger distance
#'for discrete distributions and
#'
#' (2) by creating a continuous approximation of the distributions using
#'\code{density} and then using numerical integration to calculate the
#'Hellinger distance.
#'
#'Method (2) - continuous integration - should in genernal be more accurate
#'however, it may give poor approximations for multi-modal distributions.
#'
#'Continuous integration may return NaN if the distributions are near identical.
#'
#' In cases of large discrepencies, the discrete metric is recommended. Large
#' discrepencies probably indicate multi-modal distributions.
#'
#'It is recommended to visually
#'check distribution fits, particularly if the number of random variates is
#'small.
#' See \code{hellinger} from package \code{BayeSens}
#' for more details on Hellinger distance.
#' See \code{\link{plot_dists}} if you want to plot the densities too.
#'
#' In general these methods will be inaccurate if analysis is performed on
#'too few samples, e.g. <10 000. >100 000 would be ideal.
#' @author Christopher J. Brown christo.j.brown@gmail.com
#' @rdname remix_hellinger
#' @export

remix_hellinger <- function(simmr_out, prior.control, plot.dens = TRUE){

	postchains <- do.call("rbind",lapply(simmr_out$output[[1]], function(x) data.frame(x)))
	priors <- drawpriors(simmr_out[[1]], priorcontrol = prior.control)

	snames <- names(priors)
	n <- length(snames)

	hdist <- data.frame(source = snames, Hellinger_Continuous = rep(NA, n), Hellinger_Discrete = rep(NA, n))

	nrow <- n
	ncol <- 2
	# if (plot.dens) {par(mfrow = c(nrow, ncol))}

	for (i in 1:n){

		hout <- hellinger(postchains[,snames[i]], priors[,snames[i]])

		hdist[i,2] <- signif(hout$hdist_cont	, 2)
		hdist[i,3] <- signif(hout$hdist_disc, 2)

		if (plot.dens){
			dev.new()
			plot(hout)
		}
	}

	return(hdist)
	}
