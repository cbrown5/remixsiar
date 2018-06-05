#' Isospace plots of model predicted consumer isotope ratios
#'
#' Plotting the consumer's isotope values against those predicted by the model
#' can be useful to detect model bias, or instance when consumers fall outside
#' the source polygon.
#' @Usage remix_2DPI(x2, simmr_in, groups = NULL, tracerpairs = NULL,
#' nxvals = 200, xlab = colnames(simmr_in$mixtures)[1],
#' ylab = colnames(simmr_in$mixtures)[2], prob = c(0.05, 0.25),
#' xlims = NULL, ylims = NULL, plotCI = TRUE)
#'
#' @param x2 A \code{remix_simdata} object.
#' @param simmr_in A \code{simmr_input} object.
#' @param groups A \code{numeric} with the groups to plot if they are used,
#' otherwise \code{NULL}.
#' @param tracerpairs A two row \code{matrix} were columns are the tracer pairs
#' you want to plot.
#' @param nxvals A \code{numeric} giving the number of intervals on the
#' x and y axes.
#' @param xlab A \code{character} the x-axis label
#' @param ylab A \code{character} the y-axis label
#' @param prob A \code{numeric} vector giving the probability intervals
#' to plot.
#' @param xlims A \code{numeric} pair giving the x-axis limits
#' @param ylims A \code{numeric} pair giving the y-axis limits
#' @param plotCI A \code{logical} which determines whether credibility interavls
#' are plotted in addition to the predictive intervals.
#' @return Plots of the tracer pairs with predicted isotope
#'values of consumers.
#'
#' @details
#' Plots the output of \code{\link{remix_simdata}} on two axes. remix_2DPI also
#' plots predictive intervals at the given level, which are spaces where the
#' consumer isotope ratios have a 95% (or other probability interval) of
#' falling. Optionally can also plot 95% (or other interval) credibility
#' intervals, which indicate a 95% probability of the consumer's mean value.
#' Model bias is indicated by consumer data that fall outside the predictive
#' intervals. Bias may occur, among other things due to
#' missed sources or incorrect fractionation estimates.
#' @author Christopher J. Brown
#' @rdname remix_2DPI
#' @export

remix_2DPI <- function(x2, simmr_in, groups = NULL, tracerpairs = NULL, nxvals = 200, xlab = colnames(simmr_in$mixtures)[1], ylab = colnames(simmr_in$mixtures)[2], prob = c(0.05, 0.25), xlims = NULL, ylims = NULL, plotCI = TRUE){

	if(class(x2)!="remix_simdata") stop("A remix_simdata object is required input")
	if(class(simmr_in)!="simmr_input") stop("A simmr_input object is required input")

	vdnorm <- Vectorize(dnorm, vectorize.args = c('mean', 'sd'))
	vpnorm <- Vectorize(pnorm, vectorize.args = c('mean', 'sd'))
	ntotal <- nrow(x2$xmean[[1]])

	if(is.null(groups)){
		 igrps <- 1:simmr_in$n_groups
		 } else {
		 igrps <- groups
		 	}

	if (is.null(tracerpairs)){
		tracers <- 1:simmr_in$n_tracers
		tracerpairs <- combn(tracers,2)
		}
	npairs <- ncol(tracerpairs)

	for (igrp in igrps){
		for (itpairs in 1:npairs){


		itrc1 <- tracerpairs[1,itpairs]
		itrc2 <- tracerpairs[2,itpairs]


		#
		# Ellipse method
		#

		xmat <- diag(2)
		xmat[1,1] <- var(x2$xmean[[igrp]][, itrc1])
		xmat[2,2] <- var(x2$xmean[[igrp]][, itrc2])
		xmat[1,2] <- xmat[2,1] <- cov(x2$xmean[[igrp]][, itrc1], x2$xmean[[igrp]][, itrc2])
		xmeans <- c(mean(x2$xmean[[igrp]][, itrc1]), mean(x2$xmean[[igrp]][, itrc2]))
		pell <- ellipse::ellipse(xmat, centre = xmeans, level = prob)

		#
		# Contour method
		#

		xvals1 <- .limvals(x2, igrp, itrc1, n = nxvals)
		xvals2 <- .limvals(x2, igrp, itrc2, n = nxvals)

		ptrc1 <- vdnorm(xvals1, mean = x2$xmean[[igrp]][, itrc1], sd = sqrt(x2$xvar[[igrp]][, itrc1]))
		ptrc2 <- vdnorm(xvals2, mean = x2$xmean[[igrp]][, itrc2], sd = sqrt(x2$xvar[[igrp]][, itrc2]))

		ilwrtrc1 <- rep(NA, length(prob))
		pinttrc1 <- vpnorm(xvals1, mean = x2$xmean[[igrp]][, itrc1], sd = sqrt(x2$xvar[[igrp]][, itrc1]))
		pmargtrc1 <- rowSums(pinttrc1)/ntotal
		for (iprob in 1:length(prob)){
			ilwrtrc1[iprob] <- which.min(abs((prob[iprob]/2) - pmargtrc1))
		}

		cp <- matrix(0, nrow = nxvals, ncol = nxvals)
		for (imc in 1: ntotal){
			cp <- cp + (ptrc1[,imc] %*% t(ptrc2[,imc]))
		}
		cp <- cp/ntotal

		mdat <- data.frame(simmr_in$mixtures)
		scols <- rainbow(simmr_in$n_sources)

		if (is.null(xlims)){
		xlims <- c(min(c(simmr_in$mixtures[,itrc1],
			simmr_in$source_means[,itrc1] - simmr_in$source_sds[,itrc1])),
			max(c(simmr_in$mixtures[,itrc1],
			simmr_in$source_means[,itrc1] + simmr_in$source_sds[,itrc1])
			))
			}

		if (is.null(ylims)){
		ylims <- c(min(c(simmr_in$mixtures[,itrc2],
			simmr_in$source_means[, itrc2] - simmr_in$source_sds[, itrc2])),
			max(c(simmr_in$source_means[,1],
			simmr_in$source_means[, itrc2] + simmr_in$source_sds[, itrc2])
			))
			}

		densdat <- data.frame(x = rep(xvals1, nxvals), y = rep(xvals2, each = nxvals), z = as.numeric(cp))

		par(mar = c(5,5,4,7))
		# contour(xvals1, xvals2, cp, levels = pmargtrc1[ilwrtrc1], xlim = xlims, ylim = ylims, xlab = xlab, ylab = ylab, labels = prob)
		contour(xvals1, xvals2, cp, nlevels = 3, xlim = xlims, ylim = ylims, xlab = xlab, ylab = ylab)

		if (plotCI){
		lines(pell[,1], pell[,2], col = 'red')}

		#mean
		points(xmeans[1], xmeans[2], pch =3)

		points(mdat[,1], mdat[,2], pch = 16, cex = 0.8, col = grey(0.3,0.5))

		points(simmr_in$source_means, pch = 16, col = scols)
		arrows(simmr_in$source_means[,itrc1], simmr_in$source_means[,itrc2] + simmr_in$source_sds[,itrc2], simmr_in$source_means[,itrc1], simmr_in$source_means[,itrc2] - simmr_in$source_sds[,itrc2], len = 0, col = scols)

		arrows(simmr_in$source_means[,itrc1] + simmr_in$source_sds[,itrc1],simmr_in$source_means[,itrc2], simmr_in$source_means[,itrc1] - simmr_in$source_sds[,itrc1], simmr_in$source_means[,itrc2],len = 0, col = scols)

		lims <- par('usr')

		legend(lims[2] + 0.05*(lims[2] - lims[1]), lims[3] + 0.9*(lims[4] - lims[3]),
legend = simmr_in$source_names, pch = 16, col = scols, xpd = NA)

		}
	}
}
