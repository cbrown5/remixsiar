#' Estimate the Kullback-Leibler divergence between priors and posteriors
#'
#' @Usage remix_kldiv(simmr_out, prior.control, plot.dens = TRUE)
#'
#' @param simmr_out A \code{simmr_output} object, the output of simmr_mcmc
#' @param prior.control A \code{prior.control} data.frame for simmr_mcmc
#' @param plot.dens A \code{logical} giving whether densities are plotted.
#' @param ... other arguments to \code{BayeSens::kldiv}.
#'
#' @return A \code{data.frame} containing the Kullback-Leibler divergences between
#' each source's prior and posterior distributions.
#'
#' @details Kullback-Leibler divergence is a measure of the information
#' divergence between two distributions (densities).  Units are bits.
#'#'
#' Kullback-Leibler divergence is approximated in by binning the random
#' variates and calculating the Kullback-Leibler divergence
#'for discrete distributions.
#'
#'It is recommended to visually
#'check distribution fits, particularly if the number of random variates is
#'small.
#' See \code{kldiv} from package \code{BayeSens}
#' for more details on Kullback-Leibler divergence.
#' See \code{\link{plot_dists}} if you want to plot the densities too.
#'
#' In general these methods will be inaccurate if analysis is performed on
#'too few samples, e.g. <10 000. >100 000 would be ideal.
#' @author Christopher J. Brown christo.j.brown@gmail.com
#' @rdname remix_kldiv
#' @export

remix_kldiv <- function(simmr_out, prior.control, plot.dens = TRUE, ...){

	postchains <- do.call("rbind",lapply(simmr_out$output[[1]], function(x) data.frame(x)))
	priors <- drawpriors(simmr_out[[1]], priorcontrol = prior.control)

	snames <- names(priors)
	n <- length(snames)

	hdist <- data.frame(source = snames, kldiv = rep(NA, n))

	nrow <- n
	ncol <- 2

	for (i in 1:n){

		hout <- BayeSens::kldiv(postchains[,snames[i]], priors[,snames[i]], ...)
		hdist[i,2] <- hout$kd

		if (plot.dens){
			dev.new()
			plot(hout)
		}
	}

	return(hdist)
	}
