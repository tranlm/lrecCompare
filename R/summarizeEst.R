###############################################################################
# Description: Gets summary estimates 
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Apr 30, 2015
###############################################################################


summarizeEst = function(psi, est, results, trueValue) {
	out = c(
			bias = getMean(psi, est, results) - trueValue,
			var = getVar(psi, est, results),
			min = getMin(psi, est, results),
			max = getMax(psi, est, results)
			)
	return(out)
}
