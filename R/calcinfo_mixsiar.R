#' Functions to calculate information divergence measures for mixsiar models
#'
#' @Usage calcinfo_mixsiar(mixmodlist, prior.control = NULL, metric = c("hellinger", "kldivergence"))
#'
#' @param mixmodlist a \code{list} with a simmr_input and simmr_output object.
#' @param dp the prior used, generated with function default_prior
#' , if NULL, assumes the default.
#'
#' @return Relevant information or evaluation measure,
#' assuming default priors for simmr.
#'
#' @details take an object from \code{simmr_fitsims} and calculates and
#' returns evaluation metrics.
#'
#' @author Christopher J. Brown
#' @examples
#' @rdname calc_eval_measures
#' @export


calcinfo_mixsiar <- function(mixmodlist, prior.control = NULL, metric = c("hellinger", "kldivergence")){

    snames <- mixmodlist[[1]]$source$source_names
    n <- mixmodlist[[2]]$source$n.sources

    if (is.null(prior.control)) prior.control <- rep(1, n)

    post <- mixmodlist[[2]]$BUGSoutput$sims.matrix[,1:n]
    priors <- extraDistr::rdirichlet(100000, alpha = prior.control)

    xout <- NULL
    if ("hellinger" %in% metric){
        hdist <- data.frame(source = snames,
        Hellinger_Continuous = rep(NA, n), Hellinger_Discrete = rep(NA, n))
        for (i in 1:n){
            hout <- BayeSens::hellinger(post[,i], priors[,i])
            hdist[i,2] <- signif(hout$hdist_cont, 2)
            hdist[i,3] <- signif(hout$hdist_disc, 2)
        }
    xout <- c(xout, list(hdist))
    }
    if ("kldivergence" %in% metric){
            kldist <- data.frame(source = snames,
                kldiv = rep(NA, n))
            for (i in 1:n){
                klout <- BayeSens::kldiv(post[,i], priors[,i])
                kldist[i,2] <- signif(klout$kd, 2)
            }
        xout <- c(xout, list(kldist))
        }
    return(xout)
}
