#' Functions to calculate evaluation measures
#'
#' @usage calckl(mod_run, dp = NULL)
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
#' @rdname calckl
#' @export
calckl <- function(mod_run, dp = NULL){
    if (is.null(dp)){
    dp <- default_prior(mod_run[[1]])
        }
    rh <- remix_kldiv(mod_run[[2]], dp, plot.dens = FALSE,nbreaks = 101, minx = 0, maxx = 1)
    return(rh[,2])
}
