#' Default priors for the simmr package
#'
#' @Usage default_prior(simmr_in)
#'
#' @param simmr_in A \code{simmr_input} object.
#'
#' @return Prior means and standard deviations.
#' \item{means}{Prior means}
#' \item{sd}{Standard deviations}
#' @author Christopher J. Brown
#' @rdname default_prior
#' @export
#' 
default_prior <- function(simmr_in){
list(means=rep(0,simmr_in$n_sources),sd=rep(1,simmr_in$n_sources))
}
