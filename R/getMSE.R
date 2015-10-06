###############################################################################
# Description: Function to get mean squared error
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: May 10, 2015
###############################################################################


#' @export
getMSE = function(est, results, psi_0) {
	tmp = apply(results[,grep(paste0("^", est), colnames(results))], 2, function(x) mean((x-psi_0)^2))
	names(tmp) = gsub(paste0(est,"."), "", names(tmp), fixed=TRUE)
	return(tmp)
}


