###############################################################################
# Description: Add comment
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Sep 2, 2015
###############################################################################


#' @export
psi.hat = function(vars.time, SL.library, data, indices, verbose=FALSE) {
	
	## Bootstraps ##
	d = data[indices,]
	rownames(d) = paste(1:nrow(d), rownames(d), sep=".")
	d = d[order(rownames(d)),]
	
	## Fits g ##
	gFit = estimateG(d, vars.time=vars.time, SL.library=SL.library, save=FALSE)
	
	## Estimates ##
	estimates = NULL
	for(time.pt in 1:7) {
		if(verbose) message(paste("### Time", time.pt, "###"))
		tmp = subset(d[,1:which(names(d)==paste0("dead.ltfu.", time.pt))])
		results = appliedResults(tmp=tmp, gmatrix.abar0=gFit$gmatrix.abar0, gmatrix.abar1=gFit$gmatrix.abar1, SL.library=SL.library, time.pt=time.pt, save=FALSE, verbose=TRUE)
		out = c(psi_0=results$psi_0$estimates, psi_1=results$psi_1$estimates, ate=results$ate$estimates)
		names(out) = paste("t", time.pt, ".", names(out), sep="")
		estimates = c(estimates, out)
	}
	
	## Outputs ##
	return(estimates)
	
}

