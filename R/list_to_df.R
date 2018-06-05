#' @rdname .list_to_df

.list_to_df <- function(x){
	ncols <- length(dimnames(x[[1]])[[2]])
  	z <- data.frame(matrix(unlist(x), ncol = ncols, byrow = T))
	names(z) <- dimnames(x[[1]])[[2]]
	z
}
