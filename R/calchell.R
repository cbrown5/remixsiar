#' Functions to calculate evaluation measures
#'
#' @usage calchell(mod_run, dp = NULL)
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
#' @rdname calchell
#' @export
calchell <- function(mod_run, dp = NULL){
    if (is.null(dp)){
    dp <- default_prior(mod_run[[1]])
        }
    rh <- remix_hellinger(mod_run[[2]], dp, plot.dens = FALSE)
    return(rh[,2])
}

