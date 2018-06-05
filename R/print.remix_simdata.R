print.remix_simdata <- function(rout){
	cat('Bias and RMSE for mixture values', '\n','\n')
	cat('Bias', '\n')
	print(signif(rout$bias,3))
	cat('\n','Root mean squared error', '\n')
	print(signif(rout$rmse,3))
}
