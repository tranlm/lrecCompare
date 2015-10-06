###############################################################################
# Description: Function to get coverage 
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: May 10, 2015
###############################################################################


#' @export
getCoverage = function(est, results, se, psi_0) {	
	estimates = results[,grep(paste0("^", est), colnames(results))]
	colnames(estimates) = gsub(paste0(est,"."), "", colnames(estimates), fixed=TRUE)
	coverage = matrix(nrow=nrow(results), ncol=ncol(estimates), dimnames=list(NULL, colnames(estimates)))
	for(i in colnames(estimates)) {
		coverage[,i] = abs(estimates[,i] - psi_0) <= qnorm(.975)*se[i]
	}
	return(apply(coverage, 2, mean))
}

