#' Calculate Pianka's similarity measure on sources
#'
#' @Usage pianka(nsources, s_means, s_sds)
#'
#' @param nsources \code{integer} giving number of sources
#' @param s_means \code{matrix} giving tracer means (columns) for each source
#' @param s_sds \code{matrix} giving tracer SD (columns) for each source
#'
#' @return \code{matrix} giving Pianka's similarity measure.
#'
#' @details Calculates Pianka's measure assuming isotope values of sources
#' are multivariate normal. Note that equation as given in Lu et al. 1989
#' returns a square matrix, so I am taking value (1,1).
#'
#' @author Christopher J. Brown
#' @examples
#' @rdname pianka
#' @export

pianka <- function(nsources, s_means, s_sds){
    pvals <- matrix(0, nrow = nsources, ncol = nsources)
    for (i in 1:(nsources-1)){
        for (j in (i+1):nsources){
            mui <- matrix(s_means[i, ], ncol = 1)
            muj <- matrix(s_means[j, ], ncol = 1)
            sigmai <- diag(s_sds[i,]^2, 2)
            sigmaj <- diag(s_sds[j,]^2, 2)
            sqrmahal <-
                t(mui - muj) %*% solve(sigmai + sigmaj) %*% (mui-muj)
            numer <- ((abs(sigmai %*% sigmaj)) ^ (1/4))
            denom <- (abs(0.5 * (sigmai + sigmaj))) ^ (1/2)
            alpha <- (numer %*% solve(denom)) * as.numeric(exp(-(1/2) * sqrmahal))
            #which value do I use? It outputs a square matrix
            pvals[i,j] <- alpha[1,1]
        }
    }
    return(pvals)
}
