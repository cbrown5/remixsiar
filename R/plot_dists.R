#' Calculates Hellinger distance and plot prior and posterior distributions
#'
#' This function calculates the Hellinger distance between prior and posterior
#' It can also plot the distributions.
#' @Usage plot_dists(simmr_in, simmr_out, priorcontrol, plotdist = FALSE)
#'
#' @param simmr_in A \code{simmr_input} object.
#' @param simmr_out  A \code{simmr_output} object.
#' @param priorcontrol A list with the means and standard deviations of the
#' priors.
#' @param plotdist A logical or integer vector indicating whether to plot the distributions. If an integer, then the group(s) to plot.
#'
#' @return A \code{remix_dists} object that contains the Hellinger distances.
#' \item{hellinger_discrete}{Hellinger distance estimated using a discrete
#' approximation of the densities}
#' \item{hellinger_continuous}{Hellinger distance estimated using a continuous
#' approximation of the densities}
#' \item{distfits}{A list for \code{BayeSens::helldist} objects containing details of the
#' functions fitted to the densities.}
#'
#' @details
#' Values are numerical approximations. See \code{BayeSens::hellinger} for more
#' details on estimation of Hellinger distances from MCMC chains.
#' In general the continuous estimation will be more accurate, however large
#' discrepencies between discrete and continuous estimates suggest bi-modality,
#' in which case we recommned the discrete estimate be used.
#' Items of the \code{distfits} list can be plotted to check distribution fits.
#' @author Christopher J. Brown
#' @rdname plot_dists
#' @export


plot_dists <- function(simmr_in, simmr_out, priorcontrol, plotdist = FALSE){

		if(class(simmr_in) != "simmr_input") stop("A simmr input object is required input")

		if(class(simmr_out) != "simmr_output") stop("A simmr output object is required input")

	priordat <- drawpriors(simmr_in,priorcontrol, plot.priors = FALSE)

	nsamps <- nrow(simmr_out$output[[1]][[1]])
	nchains <- length(simmr_out$output[[1]])
	ntotal <- nsamps * nchains

  if(ntotal < 5000) warning(paste('Number of samples from the posterior < 5000,
                                   which may result in unstable Hellinger distance estimates.',
                                   '\n','Try increasing the number of iterations in the MCMC run.'))

	x <- post_chains(simmr_out)
	ngrps <- length(simmr_out$output)

	hdisc <- matrix(NA, nrow = simmr_in$n_sources, ncol = ngrps)
	hcont <- matrix(NA, nrow = simmr_in$n_sources, ncol = ngrps)
	distfits <- NULL
	for (igrp in 1:ngrps){
		for (isrc in 1:simmr_in$n_sources){
			srcnam <- simmr_in$source_names[isrc]
			hout <- BayeSens::hellinger(x$postmcmc[[igrp]][x$postmcmc[[igrp]]$source==srcnam,'proportion'], priordat[,names(priordat)==srcnam], minx = 0, maxx=1)
			hdisc[isrc, igrp] <- hout$hdist_disc
			hcont[isrc, igrp] <- hout$hdist_cont
			hlist <- list(hout)
			names(hlist) <- paste0('Group_',igrp,'_',srcnam)
			distfits <- c(distfits, hlist)
		}
	}


	hdisc <- data.frame(hdisc)
	names(hdisc) <- paste0('Group', 1:ngrps)
	rownames(hdisc) <- simmr_in$source_names

	hcont <- data.frame(hcont)
	names(hcont) <- paste0('Group', 1:ngrps)
	rownames(hcont) <- simmr_in$source_names

	priordraws <- tidyr::gather(priordat, source, proportion)

	if(is.logical(plotdist)){
		 igrps <- 1:ngrps
		 } else {
		 igrps <- plotdist
		 plotdist <- TRUE
		 	}

	if(plotdist){
		for (igrp in igrps){
			x$postmcmc[[igrp]]$dist <- 'Posterior'
			priordraws$dist <- 'Prior'
			ppdf <- rbind(x$postmcmc[[igrp]], priordraws)

		ppdf$dist <- factor(ppdf$dist, levels = c("Prior","Posterior"))

		hdisct <- paste0('Hellinger','\n','distance = ',signif(hdisc[,igrp], 2))
		isort <- order(simmr_in$source_names)
		p <- ggplot2::ggplot(ppdf, aes(x = proportion, color = dist, fill = dist)) +
		ggplot2::geom_density(alpha = 0.3) +
		ggplot2::xlab('Diet proportion') +
		ggplot2::ylab('Density') +
		ggplot2::facet_grid(.~source) +
		ggplot2::annotate('text', x=-Inf, y = Inf, label = hdisct[isort], hjust = -0.1, vjust = 1.2, size = 5) +
		ggplot2::scale_fill_manual(values = c('navyblue', 'tomato')) +
		ggplot2::scale_colour_manual(values = c('navyblue', 'tomato'))
		print(p)

		}
	}
	hellout <- list(hellinger_discrete = hdisc, hellinger_continuous = hcont, distfits = distfits)
	class(hellout) <- ("remix_dists")
	return(hellout)
	}
