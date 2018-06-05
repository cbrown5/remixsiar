#' @rdname .limvals

.limvals <- function(x, group, tracer, n = 100){
	minx <- min(x$xmean[[group]][, tracer]) - max((1.5*sqrt(x$xvar[[group]][, tracer])))
	maxx <- max(x$xmean[[group]][, tracer]) + max((1.5*sqrt(x$xvar[[group]][, tracer])))
	seq(minx, maxx, length.out = n)
	}
