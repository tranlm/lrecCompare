###############################################################################
# Description: Function returns maximum estimate from simulation
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Apr 29, 2015
###############################################################################


#' @export
getMax = function(est, results) {
	tmp = apply(results[,grep(paste0("^", est), colnames(results))], 2, max)
	names(tmp) = gsub(paste0(est,"."), "", names(tmp), fixed=TRUE)
	return(tmp)
}



