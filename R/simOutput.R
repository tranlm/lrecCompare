###############################################################################
# Description: Outputs estimator summaries into Excel file
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Apr 29, 2015
###############################################################################


simOutput = function(est, file, sheet, row, col) {
	parms = loadWorkbook(file=file)
	sheets = getSheets(parms)		
	addDataFrame(x=matrix(paste(formatC(est[,"bias"], digits=3, format="f"), " (", formatC(est[,"se"], digits=3, format="f"), ") ", formatC(est[,"mse"], digits=3, format="f"), " [", formatC(est[,"coverage"], digits=2, format="f"), "]", sep=""), ncol=1), sheet=sheets[[sheet]], row.names = FALSE, col.names = FALSE, startColumn = col, startRow=row, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_RIGHT", wrapText=FALSE)))
	saveWorkbook(parms, file=file)
}

