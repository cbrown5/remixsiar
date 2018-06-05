#' Simulate consumer isotope ratios to evaluate priors  
#'
#' Simulations of consumer isotope ratios are useful to evaluate
#' model fits for bias and precision (variance).
#' This function evaluates simulates data from  \code{simmr_input} priors
#' to estimate model bias and precision.
#' Estimates can be compared to precision and bias from posteriors.
#' Multiple groups are allowed.
#'
#' @Usage simdata_priors(simmr_in, simmr_priors)
#'
#' @param simmr_in A \code{simmr_input} object, the output of simmr_mcmc
#' @param simmr_priors A \code{data.frame} object containing draws
#' of dietary proportions from the prior distributions.
#'
#' @return A \code{remix_simdata} object containing for each tracer:
#' \item{bias}{A \code{data.frame} with estimates of model bias - mean deviation
#' of the consumer data from the mean estimate}
#' \item{rmse}{Variance of consumer isotope ratio predictions
#' estimated as root-mean-square error.}
#' \item{xmean}{Mean predicted consumer isotope ratio}
#' \item{xvar}{Variance in predicted consumer isotope ratios}
#' \item{xmean_bar}{Mean predicted consumer isotope ratio for the mean
#'	dietary proportions}
#' \item{xvar_bar}{Variance in predicted consumer isotope ratios for the mean
#'	predicted dietary proportions}
#'
#'
#' @details
#' High bias is indicative of a mis-specified model, for instance
#' one that is missing an important source or an inaccurate fractionation.
#' See \code{\link{remix_PI}} and \code{\link{remix_2DPI}} for plotting
#' model predictions for 1 and 2 tracers.
#'
#' @author Christopher J. Brown christo.j.brown@gmail.com
#' @rdname simdata_priors
#' @export

simdata_priors <- function(simmr_in, simmr_priors){

	s_mean <- simmr_in$source_means
	s_sd <- simmr_in$source_sds
	q <- simmr_in$concentration
	c_mean <- simmr_in$correction_means
	c_sd <- simmr_in$correction_sds
	tracer_names <- attr(simmr_in$mixtures, 'dimnames')[[2]]
	mixes <- data.frame(simmr_in$mixtures)

	ngrps <- length(simmr_in$n_groups)
	ndraws <- nrow(simmr_priors)
	bias <- matrix(NA, nrow = simmr_in$n_tracers, ncol = ngrps)
	rmse <- matrix(NA, nrow = simmr_in$n_tracers, ncol = ngrps)

	xmean <- lapply(1:ngrps, function(x) data.frame(matrix(NA, nrow = ndraws, ncol = simmr_in$n_tracers)))
	xmean_bar <- lapply(1:ngrps, function(x) data.frame(matrix(NA, nrow = 1, ncol = simmr_in$n_tracers)))
	xvar <- lapply(1:ngrps, function(x) data.frame(matrix(NA, nrow = ndraws, ncol = simmr_in$n_tracers)))
	xvar_bar <- lapply(1:ngrps, function(x) data.frame(matrix(NA, nrow = 1, ncol = simmr_in$n_tracers)))

	 for (igrp in 1:ngrps){

		names(xmean[[igrp]]) <- tracer_names
		names(xvar[[igrp]]) <- tracer_names
		solo <- sum(simmr_in$group == igrp) == 1

		 for (j in 1:simmr_in$n_tracers){
			for (i in 1:ndraws){
				sigma <- runif(1, 0, ifelse(solo, 0.001, 10000)) #10000
				p <- as.numeric(simmr_priors[i,])
				xmean[[igrp]][i,j] <- ((p*q[,j]) %*% (s_mean[,j]+c_mean[,j])) / (p %*%q[,j])
				xvar[[igrp]][i,j] <- (((p*q[,j])^2) %*% (s_sd[,j]^2 + c_sd[,j]^2))/((p%*%q[,j])^2) + sigma^2
			 }
			 meanp <- as.numeric(colMeans(simmr_priors))
			 sigmamean <- ifelse(solo, 5E-4, 500) # 500
			 xmean_bar[[igrp]][j] <- ((meanp*q[,j]) %*% (s_mean[,j]+c_mean[,j])) / (meanp %*%q[,j])
			 xvar_bar[[igrp]][j] <- (((meanp*q[,j])^2) %*% (s_sd[,j]^2 + c_sd[,j]^2))/((meanp%*%q[,j])^2) + sigmamean^2
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

	xout <-list(bias = bias, rmse = rmse, xmean = xmean, xvar = xvar, xmean_bar = xmean_bar, xvar_bar = xvar_bar)
	class(xout) <- 'remix_simdata'
	return(xout)

}
