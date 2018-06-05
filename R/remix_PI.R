#' Estimate predictive intervals for consumer isotope ratios
#'
#' Plotting the consumer's isotope values against those predicted by the model
#' can be useful to detect model bias, or instance when consumers fall outside
#' the source polygon. This function also returns residuals for the consumers
#' as their deviations from the mean predicted value.
#'
#' @Usage remix_PI(simdata, simmr_in, simmr_out, groups = NULL,
#' plotresid = TRUE, probs = c(0.025, 0.975))
#'
#' @param simdata A \code{remix_PI} object.
#' @param simmr_in A \code{simmr_input} object.
#' @param simmr_in A \code{simmr_output} object.
#' @param groups A \code{numeric} with the groups to plot if they are used,
#' otherwise \code{NULL}.
#' @param plotresid A \code{logical} which determines whether credibility
#' residual plots are created.
#' @param probs A \code{numeric} vector of two numbers giving the upper and
#' lower predictive intervals.
#' @return A \code{remix_PI} object with
#' \item{predint}{predictive intervals for each group and tracer}
#' \item{resid}{residuals for each group and tracer.}
#'
#' @details
#' New samples should fall within the predictive intervals
#' with 95% (or other user-specified interval) probability.
#' Model bias is indicated by consumer data that fall outside the predictive
#' intervals. Bias may occur, among other things due to
#' missed sources or incorrect fractionation estimates.
#' @author Christopher J. Brown
#' @rdname remix_PI
#' @export

remix_PI <- function(simdat, simmr_in, simmr_out, groups = NULL, plotresid = TRUE, probs = c(0.025, 0.975)){

	if(class(simmr_out) != "simmr_output") stop("A simmr output object is required input")

	nxvals <- 100
	ngrps <- length(simmr_out$output)
	mixes <- data.frame(simmr_in$mixtures)
	nobs <- nrow(mixes)
	vpnorm <- Vectorize(pnorm, vectorize.args = c('mean', 'sd'))
	ntotal <- nrow(simdat$xmean[[1]])
	if(is.logical(plotresid)){
		 igrps <- 1:ngrps
		 } else {
		 igrps <- plotresid
		 plotresid <- TRUE
		}

	quants <- matrix(NA, nrow = 2, ncol = simmr_in$n_tracers)
	quants <- data.frame(quants)
	names(quants) <- colnames(simmr_in$mix)
	rownames(quants) <- paste0('Quant_', probs)

	quants <- lapply(igrps, function(x) quants)
	names(quants) <- paste0('Group_',igrps)
	yresid <- lapply(1:length(igrps), function(x) matrix(NA, nrow = nobs, ncol = simmr_in$n_tracers))
	names(yresid) <- paste0('Group_',igrps)

	for (igrp in igrps){

		for (itrc in 1:simmr_in$n_tracers){
			minx <- min(simdat$xmean[[igrp]][,itrc]) - max((2*sqrt(simdat$xvar[[igrp]][, itrc])))
			maxx <- max(simdat$xmean[[igrp]][, itrc]) + max((2*sqrt(simdat$xvar[[igrp]][, itrc])))

			xvals <- seq(minx, maxx, length.out = nxvals)

			pout <- vpnorm(xvals, mean = simdat$xmean[[igrp]][,itrc], sd = sqrt(simdat$xvar[[igrp]][,itrc]))
			xquant <- apply(pout, 1, function(x) sum(x)/ntotal)
			ilwr <- which.min(abs(xquant - probs[1]))
			iupr <- which.min(abs(xquant - probs[2]))
			quants[[igrp]][1, itrc] <- xvals[ilwr]
			quants[[igrp]][2, itrc] <- xvals[iupr]

		tmid <- median(simdat$xmean[[igrp]][,itrc])
		yresid[[igrp]][,itrc] <-  mixes[,itrc] - tmid

		if(plotresid){

			x <- 1:nobs
			iord <- order(yresid[[igrp]][,itrc])
			ymin <- min(c(quants[[igrp]][,itrc]-tmid, yresid[[igrp]][,itrc]))*1.1
			ymax <- max(c(quants[[igrp]][,itrc]-tmid, yresid[[igrp]][,itrc]))*1.1

			maint <- paste('Group',igrp,',',colnames(simmr_in$mixtures)[itrc])
			plot(x, yresid[[igrp]][iord,itrc], ylim = c(ymin, ymax), ylab = 'Residuals', xaxt = 'n', xlab = 'observations', pch = 16, main = maint)
			abline(h = 0)
			arrows(x, yresid[[igrp]][iord,itrc], x, rep(0, nobs), len = 0)
			abline(h=quants[[igrp]][iord,itrc]-tmid, lty = 2)

			}
		}
	}
	rout <- list(predint = quants, resid = yresid)
	class(rout) <- 'remix_PI'
	return(rout)
}
