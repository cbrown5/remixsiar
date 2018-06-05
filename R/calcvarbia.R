#' Functions to calculate evaluation measures
#' @usage calcvarbia(mod_run, dp = NULL)
#'
#' @param mod_run a \code{list} with a simmr_input and simmr_output object.
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
#'
#' @rdname calcvarbia
#' @export
calcvarbia <- function(mod_run){
    sims <- simdata(mod_run[[1]], mod_run[[2]], fast = T)
    dat <- c(sims$bias[,1], sims$rmse[,1])
    names(dat) <- c('bias_c', 'bias_n', "rmse_c", "rmse_n")
    return(dat)
}
