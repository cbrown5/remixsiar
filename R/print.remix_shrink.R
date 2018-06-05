print.remix_shrink <- function(rout){

	cat('Estimates for the',rout$stat)
	cat('\n', rout$K, 'data replicates used to estimate MLE', '\n','\n')
	print(signif(rout$dat,2))



}
