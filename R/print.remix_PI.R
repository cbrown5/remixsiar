print.remixs_PI <- function(rout){
	cat('Predictive intervals for mixture values', '\n','\n')
	for (igrp in 1:length(rout$predint)){
		if (length(rout$predint) > 1){
		cat(names(rout$predint)[igrp], '\n')}
		for (i in 1:ncol(rout$predint[[igrp]])){
			cat(names(rout$predint[[igrp]])[i], '\n')
			for (j in 1:2){
				cat('	', rownames(rout$predint[[igrp]])[j],'= ', signif(rout$predint[[igrp]][j,i],4), '\n')
			}
			cat('\n')
		}
	}
}
