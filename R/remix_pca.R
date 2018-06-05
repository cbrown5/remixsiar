#' Perform a principal components analysis on posterior chains
#'
#' PCA can be used as a tool to visualise correlations among
#' estimates of source contributions.  
#'
#' @Usage remix_pca(simmr_out, transform = F)
#'
#' @param simmr_out A \code{simmr_output} object.
#' @param transform A \code{logical} indicating whether to transform diet proportions
#' using the isometric log-ratio transform. 
#'
#' @return A \code{prcomp} object
#' @details
#' See \code{prcomp} for more details. \code{remix_pca} has a print method, to view the variance components. Use \code{remix_biplot} to view a biplot. 
#' Transforming the data will tend to increase emphasis on correlations among smaller values. 
#' @author Christopher J. Brown
#' @rdname remix_pca
#' @export

remix_pca <- function(simmr_out, transform = F){

	smcmc <- post_chains(simmr_out)
	dat <- smcmc$postmcmc[[1]]
	ndraws <- nrow(dat)/length(unique(dat$source))
	dat$num <- rep(1:ndraws, length(unique(dat$source)))
	dats <- tidyr::spread(dat,key = source, value = proportion)
	dats <- dplyr::select(dats, -num)
	if (transform == T) {
		datc <- compositions::ilr(dats) 
		colnames(datc) <- names(dats)[1:(ncol(dats)-1)]
	} else {
		datc <- dats
	}

	pc <- prcomp(datc)
	return(pc)
	}