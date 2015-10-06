###############################################################################
# Description: Function returns minimum estimate from simulation
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Apr 29, 2015
###############################################################################


#' @export
getMin = function(est, results) {
	tmp = apply(results[,grep(paste0("^", est), colnames(results))], 2, min)
	names(tmp) = gsub(paste0(est,"."), "", names(tmp), fixed=TRUE)
	return(tmp)
}


