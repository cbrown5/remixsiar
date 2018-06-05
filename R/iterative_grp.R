#' Iteratively apply \code{group_sources} or \code{rm_sources}
#'
#' @Usage iterative_grp(nsources, mingrps, s_means, s_sds, s_names)
#'
#' @param nsources \code{integer} giving number of sources
#' @param mingrps \code{integer} the minimum number of groups
#' @param s_means \code{matrix} giving tracer means (columns) for each source
#' @param s_sds \code{matrix} giving tracer SD (columns) for each source
#' @param s_names \code{character} giving source names
#' @param method \code{character} giving method
#'
#' @return a \code{list} of outputs from \code{group_sources}
#'
#' @details Method can either be "group" or "rm". Group aggregates 
#' similar sources, rm removes one of them. 
#'
#' @author Christopher J. Brown
#' @examples
#' @rdname iterative_grp
#' @export

iterative_grp <- function(nsources, mingrps, s_means, s_sds, s_names, method = "group"){
    if (!(method %in% c("group","rm"))) stop("Method must be either 'group' or 'rm'")
    igrps <- seq(nsources, mingrps+1, by=-1)
    ngrps <- length(igrps)
    groupdf <- list(list(s_means = s_means, s_sds = s_sds, grouped = NULL,
 grpids = 1:nsources, s_names = s_names))

    for (i in 1:ngrps){
        if (method == "group"){ 
          groupdf <- c(groupdf, list(group_sources(igrps[i], groupdf[[i]]$s_means,
            groupdf[[i]]$s_sds, s_names = groupdf[[i]]$s_names)))}
          else if(method == "rm"){
            groupdf <- c(groupdf, list(rm_sources(igrps[i], groupdf[[i]]$s_means,
                                                     groupdf[[i]]$s_sds, s_names = groupdf[[i]]$s_names)))
            
          }
    }
    return(groupdf)
}
