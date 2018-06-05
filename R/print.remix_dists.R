print.remix_dists <- function(rout){
	cat('Hellinger - Discrete', '\n')
	print(signif(rout$hellinger_discrete, 2))
	cat('\n')
	cat('Hellinger - Continuous', '\n')
	print(signif(rout$hellinger_continuous, 2))
}
