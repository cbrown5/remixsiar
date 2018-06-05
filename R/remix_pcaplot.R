#' Plot the axes loadings from a PCA
#'
#' Plots vectors representing the loadings of each source
#' on two PCA axes. 
#'
#' @Usage remix_pcaplot(pc, axes = c(1,2))
#'
#' @param pc A \code{prcomp} object.
#' @param axes A \code{numeric} vector giving the two 
#' axes to plot.
#'
#' @return NULL
#' @details
#' This function is similar to a PCA biplot, except that the 
#' observations are not plotted. This speeds up plotting for
#' when a PCA has been run on MCMC chains, which may have
#' 100 000s of observations.  
#' @author Christopher J. Brown
#' @rdname remix_pcaplot
#' @export

remix_pcaplot <- function(pc, axes = c(1,2)){
	ranges <- c(range(pc$rotation[,axes[1]]), range(pc$rotation[,axes[2]]))
	xlim <- ylim <- c(min(ranges), max(ranges))
	
	int <- rep(0, nrow(pc$rotation))
	plot(0,0, xlim = xlim, ylim = ylim, type = 'n',
	xlab = paste('PC',axes[1]), ylab = paste('PC',axes[2]))
	arrows(int, int, pc$rotation[,axes[1]], pc$rotation[,axes[2]], len = 0.1, col = 'grey')
	text(pc$rotation[,axes[1]]*1.03, pc$rotation[,axes[2]]*1.03, row.names(pc$rotation))
	}