#' Random draws from simmr prior distributions
#'
#' @Usage drawpriors(simmr_in,priorcontrol, ndraws = 50000,
#' plot.priors = FALSE, cor.table = FALSE)
#'
#' @param simmr_in A \code{simmr_input} object with the data for input
#' to a simmr model
#' @param priorcontrol A list with the means and standard deviations of the
#' priors.
#' @param ndraws A \code{numeric} giving the number of draws from the prior
#'distributions.
#' @param plot.priors A \code{logical} should the prior densities be plotted?
#' @param cor.table A \code{logical} should a table of correlations among
#' sources be printed?

#'
#' @return A \code{data.frame} of random draws from the priors.
#'
#' @details
#' Newer isotope mixing models, including simmr, use logistic-normal
#' transformations. So \code{priorcontrol} should give means and sds in normal
#' space. If you want to specify priors as proportions see:
#' \code{simmr::simmr_elicit}
#'
#' @author Christopher J. Brown christo.j.brown@gmail.com
#' @rdname drawpriors
#' @export
drawpriors <- function(simmr_in,priorcontrol, ndraws = 50000, plot.priors = FALSE, cor.table = FALSE){
		if(class(simmr_in) != "simmr_input") stop("A simmr input object is required input")

	priordraws <- matrix(NA, nrow = ndraws, ncol = simmr_in$n_sources)
	for (isrc in 1:simmr_in$n_sources){
		priordraws[,isrc] <- rnorm(ndraws, mean = priorcontrol$means[isrc], sd = priorcontrol$sd[isrc])
	}

	priordf <- data.frame(priordraws)
	names(priordf) <- simmr_in$source_names
	priordf2 <- tidyr::gather(priordf, source, proportion)

	expf <- exp(priordraws)
	p_priors <- t(apply(expf,1, FUN = function(x) x/sum(x)))
	pdf <- data.frame(p_priors)
	names(pdf) <- simmr_in$source_names

	if (plot.priors){
	pdf2 <- tidyr::gather(pdf, source, proportion)

	p <- ggplot2::ggplot(pdf2, aes(x = proportion)) +
	ggplot2::geom_density(fill = 'purple') +
	ggplot2::xlab('Diet proportion') +
	ggplot2::ylab('Density') +
	ggplot2::facet_wrap(~source)
	print(p)
	}

	if(cor.table){
		print(cor(pdf))
	}

	return(pdf)
}
