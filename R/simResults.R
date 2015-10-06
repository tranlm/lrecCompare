###############################################################################
# Description: Add comment
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Sep 2, 2015
###############################################################################


#' @export
simResults = function(file, results, psi) {

	estimates = do.call("rbind", mclapply(results, function(x) return(x$psi_0)))
	SEs = do.call("rbind", mclapply(results, function(x) return(x$se_0)))
	prob = do.call("rbind", mclapply(results, function(x) return(x$prob)))
	ltmle = ltmls = gcomp = iptw = ICEcs = ICEws = ICEcp = ICEwp = aipws = aipwp = matrix(nrow=5, ncol=4, dimnames=list(c("uadj", "true", "misQ", "misg", "misb"), c("bias", "se", "mse", "coverage")))
	
	ltmle[,"bias"] = getMean("tmle", estimates) - psi$abar0
	ltmle[,"se"] = sqrt(getVar("tmle", estimates))
	ltmle[,"mse"] = getMSE("tmle", estimates, psi$abar0)
	ltmle[,"coverage"] = getCoverage("tmle", estimates, ltmle[,"se"], psi$abar0)
	
	ltmls[,"bias"] = getMean("tmls", estimates) - psi$abar0
	ltmls[,"se"] = sqrt(getVar("tmls", estimates))
	ltmls[,"mse"] = getMSE("tmls", estimates, psi$abar0)
	ltmls[,"coverage"] = getCoverage("tmls", estimates, ltmls[,"se"], psi$abar0)
	
	gcomp[,"bias"] = getMean("gcomp", estimates) - psi$abar0
	gcomp[,"se"] = sqrt(getVar("gcomp", estimates))
	gcomp[,"mse"] = getMSE("gcomp", estimates, psi$abar0)
	gcomp[,"coverage"] = getCoverage("gcomp", estimates, gcomp[,"se"], psi$abar0)
	
	iptw[,"bias"] = getMean("iptw", estimates) - psi$abar0
	iptw[,"se"] = sqrt(getVar("iptw", estimates))
	iptw[,"mse"] = getMSE("iptw", estimates, psi$abar0)
	iptw[,"coverage"] = getCoverage("iptw", estimates, iptw[,"se"], psi$abar0)
	
	ICEcs[,"bias"] = getMean("ICEcs", estimates) - psi$abar0
	ICEcs[,"se"] = sqrt(getVar("ICEcs", estimates))
	ICEcs[,"mse"] = getMSE("ICEcs", estimates, psi$abar0)
	ICEcs[,"coverage"] = getCoverage("ICEcs", estimates, ICEcs[,"se"], psi$abar0)
	
	ICEws[,"bias"] = getMean("ICEws", estimates) - psi$abar0
	ICEws[,"se"] = sqrt(getVar("ICEws", estimates))
	ICEws[,"mse"] = getMSE("ICEws", estimates, psi$abar0)
	ICEws[,"coverage"] = getCoverage("ICEws", estimates, ICEws[,"se"], psi$abar0)
	
	ICEcp[,"bias"] = getMean("ICEcp", estimates) - psi$abar0
	ICEcp[,"se"] = sqrt(getVar("ICEcp", estimates))
	ICEcp[,"mse"] = getMSE("ICEcp", estimates, psi$abar0)
	ICEcp[,"coverage"] = getCoverage("ICEcp", estimates, ICEcp[,"se"], psi$abar0)
	
	ICEwp[,"bias"] = getMean("ICEwp", estimates) - psi$abar0
	ICEwp[,"se"] = sqrt(getVar("ICEwp", estimates))
	ICEwp[,"mse"] = getMSE("ICEwp", estimates, psi$abar0)
	ICEwp[,"coverage"] = getCoverage("ICEwp", estimates, ICEwp[,"se"], psi$abar0)
	
	aipws[,"bias"] = getMean("aipws", estimates) - psi$abar0
	aipws[,"se"] = sqrt(getVar("aipws", estimates))
	aipws[,"mse"] = getMSE("aipws", estimates, psi$abar0)
	aipws[,"coverage"] = getCoverage("aipws", estimates, aipws[,"se"], psi$abar0)
	
	aipwp[,"bias"] = getMean("aipwp", estimates) - psi$abar0
	aipwp[,"se"] = sqrt(getVar("aipwp", estimates))
	aipwp[,"mse"] = getMSE("aipwp", estimates, psi$abar0)
	aipwp[,"coverage"] = getCoverage("aipwp", estimates, aipwp[,"se"], psi$abar0)
	
	## OUTPUT ##
	simOutput(gcomp, file, "Simulation", 25, 2+time.pt)
	simOutput(iptw,  file, "Simulation", 31, 2+time.pt)
	simOutput(ICEcs, file, "Simulation", 37, 2+time.pt)
	simOutput(ICEws, file, "Simulation", 43, 2+time.pt)
	simOutput(ICEcp, file, "Simulation", 49, 2+time.pt)
	simOutput(ICEwp, file, "Simulation", 55, 2+time.pt)
	simOutput(aipws, file, "Simulation", 61, 2+time.pt)
	simOutput(aipwp, file, "Simulation", 67, 2+time.pt)
	simOutput(ltmls, file, "Simulation", 73, 2+time.pt)
	simOutput(ltmle, file, "Simulation", 79, 2+time.pt)
	rm(list=c("gcomp", "iptw", "ICEcs", "ICEws", "ICEcp", "ICEwp", "aipws", "aipwp", "ltmle", "ltmls"))
	
	## g(A|Pa) & TRUE VALUE ##
	parms = loadWorkbook(file=file)
	sheets = getSheets(parms)
	addDataFrame(x=rbind(mean(psi$gmat<0.001), cbind(summary(psi$gmat))), sheet=sheets[["Simulation"]], row.names = FALSE, col.names = FALSE, startColumn = 2+time.pt, startRow=12, colStyle=list('1'=CellStyle(parms) + DataFormat("#,##0.0000") + Alignment(h="ALIGN_CENTER")))
	addDataFrame(x=psi$abar0, sheet=sheets[["Simulation"]], row.names = FALSE, col.names = FALSE, startColumn = 2+time.pt, startRow=22, colStyle=list('1'=CellStyle(parms) + DataFormat("#,##0.000") + Alignment(h="ALIGN_CENTER")))
	
	## EXTRA INFO ##
	addDataFrame(x=mean(prob), sheet=sheets[["Simulation"]], row.names = FALSE, col.names = FALSE, startColumn = 2+time.pt, startRow=94, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE)))
	addDataFrame(x=matrix(apply(estimates[,grep("aipws.", colnames(estimates))], 2, function(x) mean(x<0 | x>1)), ncol=1), sheet=sheets[["Simulation"]], row.names = FALSE, col.names = FALSE, startColumn = 2+time.pt, startRow=97, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE)))
	addDataFrame(x=matrix(apply(estimates[,grep("aipwp.", colnames(estimates))], 2, function(x) mean(x<0 | x>1)), ncol=1), sheet=sheets[["Simulation"]], row.names = FALSE, col.names = FALSE, startColumn = 2+time.pt, startRow=103, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE)))
	addDataFrame(x=matrix(getMean("iptw", SEs), ncol=1), sheet=sheets[["Simulation"]], row.names = FALSE, col.names = FALSE, startColumn = 2+time.pt, startRow=110, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE) + DataFormat("#,##0.000")))
	addDataFrame(x=matrix(getMean("aipws", SEs), ncol=1), sheet=sheets[["Simulation"]], row.names = FALSE, col.names = FALSE, startColumn = 2+time.pt, startRow=116, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE) + DataFormat("#,##0.000")))
	addDataFrame(x=matrix(getMean("aipwp", SEs), ncol=1), sheet=sheets[["Simulation"]], row.names = FALSE, col.names = FALSE, startColumn = 2+time.pt, startRow=122, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE) + DataFormat("#,##0.000")))
	addDataFrame(x=matrix(getMean("tmls", SEs), ncol=1), sheet=sheets[["Simulation"]], row.names = FALSE, col.names = FALSE, startColumn = 2+time.pt, startRow=128, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE) + DataFormat("#,##0.000")))
	addDataFrame(x=matrix(getMean("tmle", SEs), ncol=1), sheet=sheets[["Simulation"]], row.names = FALSE, col.names = FALSE, startColumn = 2+time.pt, startRow=134, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE) + DataFormat("#,##0.000")))
	
	saveWorkbook(parms, file=file)
	
	return(NULL)
	
}
