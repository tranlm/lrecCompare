###############################################################################
# Description: Calculates node specified IC
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: May 3, 2015
###############################################################################


# Copied and updated from ltmle package (v.0.9-6)
#' @export
calcIC = function(Q.kplus1, Q.k, h.g.ratio, uncensored, intervention.match) {
	
	IC = rep(0, times=length(Q.k))
	index = uncensored & intervention.match
	if (any(h.g.ratio[index] != 0)) {
		IC[index] = (Q.kplus1[index] - Q.k[index]) * h.g.ratio[index]
	}
	return(IC)
	
}

