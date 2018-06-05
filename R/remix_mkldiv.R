#' Estimate the multivariate Kullback-Leibler divergence between priors and posteriors
#'
#' @Usage remix_mkldiv(simmr_out, prior.control, plot.dens = TRUE)
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

remix_mkldiv <- function(simmr_out, prior.control, plot.dens = TRUE, ...){

    srcnames <- simmr_out$input$source_names
	postchains <- do.call("rbind",lapply(simmr_out$output[[1]], function(x) data.frame(x)))

    isrcs <- which(dimnames(postchains)[[2]] %in% srcnames)
    nscrs <- length(isrcs)
    #Need to backtransform postchains to gaussian space
    postclr <- data.frame(compositions::ilr(postchains[,isrcs]))
    #x2 <- clrInv(x) %>% data.frame() #inverse, same as cdata
    mu1 <- apply(postclr, 2, mean)
    S1 <- cov(postclr)

	priors <- drawpriors(simmr_out[[1]], priorcontrol = prior.control)
    priorclr <- data.frame(compositions::ilr(priors))
    mu2 <- apply(priorclr, 2, mean)
    S2 <- cov(priorclr)

    #KL div
    S1c <- chol(S1)
    S2c <- chol(S2)
    ld2 <- 2 * sum(log(diag(S2c)))
    ld1 <- 2 * sum(log(diag(S1c)))
    ldet <- ld1 - ld2
    S1i <- chol2inv(S1c)
    tr <- sum(diag(S1i %*% S2))
    m2mm1 <- mu2 - mu1
    qf <- as.numeric(t(m2mm1) %*% (S1i %*% m2mm1))
    kldist <- (0.5 * (ldet + tr + qf - nrow(S1)))/log(2)

    #hellinger distnace
    det1 <- det(S1)
    det2 <- det(S2)
    expterm <- -(1/8) * t(mu1-mu2) %*% solve((S1 + S2)/2) %*% (mu1 - mu2)
    hdist <- sqrt(1 - (((((det1^0.25) * (det2^0.25))/(det((S1 + S2)/2) ^ 0.5)) * exp(expterm))))
    xout <- list(hdist = hdist, kldist = kldist)
	return(xout)
	}
