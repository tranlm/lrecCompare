###############################################################################
# Description: Function results mean of estimates from simulation
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Apr 29, 2015
###############################################################################

#' @export
getMean = function(est, results) {
	tmp = apply(results[,grep(paste0("^", est), colnames(results))], 2, mean)
	names(tmp) = gsub(paste(est,".",sep=""), "", names(tmp))
	return(tmp)
}

