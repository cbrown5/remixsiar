#' Simulate consumer isotope ratios to evaluate posterior model fits
#'
#' Simulations of consumer isotope ratios are useful to evaluate
#' model fits for bias and precision (variance).
#' This function simulates data from  \code{simmr_output} objects to
#' estimate model bias and precision.
#' Multiple groups are allowed.
#'
#' @Usage simdata(simmr_in, simmr_out, fast = TRUE)
#'
#' @param simmr_in A \code{simmr_input} object, the output of simmr_mcmc
#' @param simmr_out A \code{simmr_output} object
#' @param fast A \code{logical} indicating whether to sub-sample the
#' full mcmc chain, to speed simulation of new data.
#'
#' @return A \code{remix_simdata} object containing for each tracer:
#' \item{bias}{A \code{data.frame} with estimates of model bias - mean deviation
#' of the consumer data from the mean estimate}
#' \item{rmse}{Variance of consumer isotope ratio predictions
#' estimated as root-mean-square error.}
#' \item{xmean}{Mean predicted consumer isotope ratio}
#' \item{xvar}{Variance in predicted consumer isotope ratios}
#'
#' @details
#' High bias is indicative of a mis-specified model, for instance
#' one that is missing an important source or an inaccurate fractionation.
#' See \code{\link{remix_PI}} and \code{\link{remix_2DPI}} for plotting
#' model predictions for 1 and 2 tracers.
#'
#' @author Christopher J. Brown christo.j.brown@gmail.com
#' @rdname simdata
#' @export

simdata <- function(simmr_in, simmr_out, fast = TRUE){

	s_mean <- simmr_in$source_means
	s_sd <- simmr_in$source_sds
	q <- simmr_in$concentration
	c_mean <- simmr_in$correction_means
	c_sd <- simmr_in$correction_sds
	tracer_names <- attr(simmr_in$mixtures, 'dimnames')[[2]]
	mixes <- data.frame(simmr_in$mixtures)
	nsamps <- nrow(simmr_out$output[[1]][[1]])
	nchains <- length(simmr_out$output[[1]])
	ntotal <- nsamps * nchains
	ngrps <- length(simmr_out$output)
	mcs <- rep(1:nchains, each = nsamps)
	samps <- rep(1:nsamps, nchains)

  if (fast == TRUE){
    if (ntotal > 5000){
      ndraws <- 5000
      idraws <- round(seq(1, ntotal, length.out = ndraws))
    }
  } else {
    ndraws <- ntotal
    idraws <- 1:ntotal

  }

	bias <- matrix(NA, nrow = simmr_in$n_tracers, ncol = ngrps)
	rmse <- matrix(NA, nrow = simmr_in$n_tracers, ncol = ngrps)

	xmean <- lapply(1:ngrps, function(x) data.frame(matrix(NA, nrow = ndraws, ncol = simmr_in$n_tracers)))
	xvar <- lapply(1:ngrps, function(x) data.frame(matrix(NA, nrow = ndraws, ncol = simmr_in$n_tracers)))


	for (igrp in 1:ngrps){

		names(xmean[[igrp]]) <- tracer_names
		names(xvar[[igrp]]) <- tracer_names

		for (j in 1:simmr_in$n_tracers){
			for (i in 1:ndraws){
				imc <- mcs[idraws[i]]
				isamp <- samps[idraws[i]]
				p <- simmr_out$output[[igrp]][[imc]][isamp,1:simmr_in$n_sources]
				sigma <- simmr_out$output[[igrp]][[imc]][isamp,simmr_in$n_sources+j]
				xmean[[igrp]][i,j] <- ((p*q[,j]) %*% (s_mean[,j]+c_mean[,j])) / (p %*%q[,j])
				xvar[[igrp]][i,j] <- (((p*q[,j])^2) %*% (s_sd[,j]^2 + c_sd[,j]^2))/((p%*%q[,j])^2) + sigma^2
			}
		}

		for (itrac in 1:simmr_in$n_tracers){
			bias[itrac, igrp] <- mean(mean(xmean[[igrp]][, itrac]) - mixes[, itrac])
			rmse[itrac, igrp] <- sqrt(mean((mean(xmean[[igrp]][, itrac]) - mixes[, itrac])^2))
		}
	}

	bias <- data.frame(bias)
	names(bias) <- paste0('Group', 1:ngrps)
	rownames(bias) <- tracer_names
	rmse <- data.frame(rmse)
	names(rmse) <- paste0('Group', 1:ngrps)
	rownames(rmse) <- tracer_names

	xout <-list(bias = bias, rmse = rmse, xmean = xmean, xvar = xvar)
	class(xout) <- 'remix_simdata'
	return(xout)

}
