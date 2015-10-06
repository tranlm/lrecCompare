###############################################################################
# Description: Gets last visit before being considered LTFU
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Apr 30, 2015
###############################################################################


Lastvisit = function(x, limit) {
	xnext = rep(NA, length(x))
	if(length(x)==1) {
		ltfuDate = x
	} else {
		xnext[1:(length(x)-1)] = x[2:length(x)]
		xdiff = xnext - as.numeric(x)
		ltfuObs = suppressWarnings(min(which(xdiff>=limit)))
		if(ltfuObs==Inf) {
			ltfuDate = x[length(x)]
		} else {
			ltfuDate = as.Date(x[ltfuObs], origin="1970-01-01")
		}
	}
	return(ltfuDate)
}

