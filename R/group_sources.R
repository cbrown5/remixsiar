#' Regroup sources into n-1 groups based on Pianka's measure
#'
#' @Usage group_sources(nsources, s_means, s_sds, s_names)
#'
#' @param nsources \code{integer} giving number of sources
#' @param s_means \code{matrix} giving tracer means (columns) for each source
#' @param s_sds \code{matrix} giving tracer SD (columns) for each source
#' @param s_names \code{character} giving source names
#'
#' @return \code{list} containing grouped sources data with objects:
#' s_means (source means), s_sds (source SDs), grouped (two groups that were
#' lumped), grpids (IDs of new groups), s_names (new source names).
#'
#' @details identifies the two sources with highest similarity then combines
#' their isotopic values as the mean of the two distributions.  
#'
#' @author Christopher J. Brown
#' @examples
#' @rdname group_sources
#' @export

group_sources <- function(nsources, s_means, s_sds, s_names){
    pvals <- pianka(nsources, s_means, s_sds)
    maxp <- max(pvals)
    ijmax <- which(pvals == maxp, arr.ind = T)
    dimnames(s_means)[[2]] <- NULL
    dimnames(s_sds)[[2]] <- NULL
    newgrps <- data.frame(id = 1:nsources, s_means = s_means, s_sds = s_sds)
    newgrps$id[newgrps$id == ijmax[2]] <- ijmax[1]
    newgrps2 <- dplyr::summarize_all(dplyr::group_by(newgrps, id), function(x) sum(x)/length(x))
    s_means2 <- as.matrix(newgrps2[,grepl("s_means",names(newgrps2))])
    s_sds2 <- as.matrix(newgrps2[,grepl("s_sds",names(newgrps2))])
    s_names2 <- unique(s_names[newgrps$id])
    rout <- list(s_means = s_means2, s_sds = s_sds2, grouped = ijmax,
grpids = newgrps$id, s_names = s_names2)
    return(rout)
}
