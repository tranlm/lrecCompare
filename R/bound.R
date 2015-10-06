###############################################################################
# Description: Add comment
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Jun 17, 2015
###############################################################################


#' @export
bound = function(x, range) {
	newX = x
	newX[newX<min(range)] = min(range)
	newX[newX>max(range)] = max(range)
	return(newX)
}

