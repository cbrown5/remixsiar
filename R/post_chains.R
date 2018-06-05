#' Extract chains for the posterior distribution
#'
#' @Usage post_chains(simmr_out, plot.post = FALSE){
#'
#' @param simmr_out  A \code{simmr_output} object.
#' @param plot.post A logical or integer vector indicating whether to plot the
#' distributions. If an integer, then the group(s) to plot.
#'
#' @return A list that contains data.frames of the MCMC draws for each group.
#' @details
#' Normally, this function need not be called directly.
#' Instead use \code{\link{plot_dists}} to calculate Hellinger distances
#' and plot posterior distributions.
#' @author Christopher J. Brown
#' @rdname post_chains
#' @export


post_chains <- function(simmr_out, plot.post = FALSE){

	if(class(simmr_out) != "simmr_output") stop("A simmr_output object is required input")

	ngrps <- length(simmr_out$output)
	pdf2 <- NULL

	if(is.logical(plot.post)){
		 igrps <- 1:ngrps
		 } else {
		 igrps <- plot.post
		 plot.post <- TRUE
		 	}
	for (igrp in 1:ngrps){
		nchains <- length(simmr_out$output[[igrp]])

		postmcmc <- NULL
		for (i in 1:nchains){
			postmcmc <- rbind(postmcmc, data.frame(simmr_out $output[[igrp]][[i]][,1: simmr_out $input$n_sources]))
			}

		pdf2 <- c(pdf2, list(tidyr::gather(postmcmc, source, proportion)))

		if (plot.post & (igrp %in% igrps)){
			p <- ggplot2::ggplot(pdf2[[igrp]], aes(x = proportion)) +
			ggplot2::geom_density(fill = 'steelblue') +
			ggplot2::xlab('Diet proportion') +
			ggplot2::ylab('Density') +
			ggplot2::facet_wrap(~source)
			print(p)
		}
	}
	return(list(postmcmc = pdf2))
	}
