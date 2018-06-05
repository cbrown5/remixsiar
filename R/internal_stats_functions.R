
#' @rdname .est_mode
.est_mode <- function(x) {
  d <- density(x)
  d$x[which.max(d$y)]
}


#' @rdname .extract_stat
.extract_stat <- function(simmr_out, stat, nparms = simmr_out$input$n_sources){
	postchains <- do.call("rbind",lapply(simmr_out$output[[1]], function(x) data.frame(x)))
	apply(postchains, 2,stat)[1:nparms]
	}
