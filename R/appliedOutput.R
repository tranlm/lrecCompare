###############################################################################
#
# Description: Outputs applied analysis to Excel.
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: May 14, 2015
###############################################################################


appliedOutput = function(file, results, SL.library=NULL, time.pt, row, col, fits) {
	
	parms = loadWorkbook(file=file)
	sheets = getSheets(parms)
	colStyle1 = list(
			'1'=CellStyle(parms) + DataFormat("#,##0.000") + Alignment(h="ALIGN_RIGHT")
	)
	colStyle2 = list(
			'1'=CellStyle(parms) + DataFormat("#,##0.000") + Alignment(h="ALIGN_CENTER")
	)
	colStyle3 = list(
			'1'=CellStyle(parms) + DataFormat("#,##0.000") + Alignment(h="ALIGN_CENTER"),
			'2'=CellStyle(parms) + DataFormat("#,##0.000") + Alignment(h="ALIGN_CENTER"),
			'3'=CellStyle(parms) + DataFormat("#,##0.000") + Alignment(h="ALIGN_CENTER"),
			'4'=CellStyle(parms) + DataFormat("#,##0.000") + Alignment(h="ALIGN_CENTER"),
			'5'=CellStyle(parms) + DataFormat("#,##0.000") + Alignment(h="ALIGN_CENTER"),
			'6'=CellStyle(parms) + DataFormat("#,##0.000") + Alignment(h="ALIGN_CENTER"),
			'7'=CellStyle(parms) + DataFormat("#,##0.000") + Alignment(h="ALIGN_CENTER")
	)
	
	## Cumulative g-fit ##
	addDataFrame(x=results$lowprob$psi_0, sheet=sheets[["g-fits"]], row.names = FALSE, col.names = FALSE, startColumn = col+time.pt-1+ifelse(is.null(SL.library), 0, 9), startRow=row-1, colStyle=colStyle2)
	addDataFrame(x=results$lowprob$psi_1, sheet=sheets[["g-fits"]], row.names = FALSE, col.names = FALSE, startColumn = col+time.pt-1+ifelse(is.null(SL.library), 0, 9), startRow=row+11-1, colStyle=colStyle2)	
	addDataFrame(x=t(results$cum.g$psi_0), sheet=sheets[["g-fits"]], row.names = FALSE, col.names = FALSE, startColumn = col+time.pt-1+ifelse(is.null(SL.library), 0, 9), startRow=row, colStyle=colStyle2)
	addDataFrame(x=t(results$cum.g$psi_1), sheet=sheets[["g-fits"]], row.names = FALSE, col.names = FALSE, startColumn = col+time.pt-1+ifelse(is.null(SL.library), 0, 9), startRow=row+11, colStyle=colStyle2)
	addDataFrame(x=t(results$cum.g.unb$psi_0), sheet=sheets[["g-fits"]], row.names = FALSE, col.names = FALSE, startColumn = col+time.pt-1+ifelse(is.null(SL.library), 0, 9), startRow=row+23, colStyle=colStyle2)
	addDataFrame(x=t(results$cum.g.unb$psi_1), sheet=sheets[["g-fits"]], row.names = FALSE, col.names = FALSE, startColumn = col+time.pt-1+ifelse(is.null(SL.library), 0, 9), startRow=row+11+23, colStyle=colStyle2)
	addDataFrame(x=results$lowprob$psi_0, sheet=sheets[["g-fits"]], row.names = FALSE, col.names = FALSE, startColumn = col+time.pt-1+ifelse(is.null(SL.library), 0, 9), startRow=row-1+23, colStyle=colStyle2)
	addDataFrame(x=results$lowprob$psi_1, sheet=sheets[["g-fits"]], row.names = FALSE, col.names = FALSE, startColumn = col+time.pt-1+ifelse(is.null(SL.library), 0, 9), startRow=row+11-1+23, colStyle=colStyle2)	
	
	## Unadjusted estimates ##
	if(is.null(SL.library)) {
	#if(FALSE) {
		unadj.psi_0.est = paste(formatC(results$psi_0$estimates["uadj"], digits=3, format="f"), " (", formatC(sd(results$bootEsts[,"psi_0.uadj"]), digits=3, format="f"), ") [", formatC(quantile(results$bootEsts[,"psi_0.uadj"], .025), digits=2, format="f"), ",", formatC(quantile(results$bootEsts[,"psi_0.uadj"], .975), digits=2, format="f"), "]", sep="")
		unadj.psi_1.est = paste(formatC(results$psi_1$estimates["uadj"], digits=3, format="f"), " (", formatC(sd(results$bootEsts[,"psi_1.uadj"]), digits=3, format="f"), ") [", formatC(quantile(results$bootEsts[,"psi_1.uadj"], .025), digits=2, format="f"), ",", formatC(quantile(results$bootEsts[,"psi_1.uadj"], .975), digits=2, format="f"), "]", sep="")
	} else {
		unadj.psi_0.est = paste(formatC(results$psi_0$estimates["uadj"], digits=3, format="f"), " (x.xxx) [", "x.xx,x.xx", "]", sep="")
		unadj.psi_1.est = paste(formatC(results$psi_1$estimates["uadj"], digits=3, format="f"), " (x.xxx) [", "x.xx,x.xx", "]", sep="")
	}
	addDataFrame(x=matrix(unadj.psi_0.est, ncol=1), sheet=sheets[["AppliedEstimates"]], row.names = FALSE, col.names = FALSE, startColumn = col+time.pt-1+ifelse(is.null(SL.library), 0, 9), startRow=row, colStyle=colStyle1)
	addDataFrame(x=matrix(unadj.psi_1.est, ncol=1), sheet=sheets[["AppliedEstimates"]], row.names = FALSE, col.names = FALSE, startColumn = col+time.pt-1+ifelse(is.null(SL.library), 0, 9), startRow=row+16, colStyle=colStyle1)
	
	## Adjusted estimates ##
	#adj = paste(formatC(estimates, digits=3, format="f"), " (", formatC(SEs, digits=3, format="f"), ")", sep="")
	if(is.null(SL.library)) {
	#if(FALSE) {
	psi_0.bootstrap = results$bootEsts[,paste("psi_0", names(results$psi_0$estimates)[-1], sep=".")]
		adj.psi_0.est = paste(formatC(results$psi_0$estimates[-1], digits=3, format="f"), " (", formatC(apply(psi_0.bootstrap, 2, sd), digits=3, format="f"), ") [", formatC(apply(psi_0.bootstrap, 2, function(x) quantile(x, .025)), digits=2, format="f"), ",", formatC(apply(psi_0.bootstrap, 2, function(x) quantile(x, .975)), digits=2, format="f"), "]", sep="")
		psi_1.bootstrap = results$bootEsts[,paste("psi_1", names(results$psi_1$estimates)[-1], sep=".")]
		adj.psi_1.est = paste(formatC(results$psi_1$estimates[-1], digits=3, format="f"), " (", formatC(apply(psi_1.bootstrap, 2, sd), digits=3, format="f"), ") [", formatC(apply(psi_1.bootstrap, 2, function(x) quantile(x, .025)), digits=2, format="f"), ",", formatC(apply(psi_1.bootstrap, 2, function(x) quantile(x, .975)), digits=2, format="f"), "]", sep="")		
	} else {
		adj.psi_0.est = paste(formatC(results$psi_0$estimates[-1], digits=3, format="f"), " (", "x.xxx", ") [", "x.xx,x.xx", "]", sep="")
		adj.psi_1.est = paste(formatC(results$psi_1$estimates[-1], digits=3, format="f"), " (", "x.xxx", ") [", "x.xx,x.xx", "]", sep="")
	}
	addDataFrame(x=matrix(adj.psi_0.est, ncol=1), sheet=sheets[["AppliedEstimates"]], row.names = FALSE, col.names = FALSE, startColumn = col+time.pt-1+ifelse(is.null(SL.library), 0, 9), startRow=row+2, colStyle=colStyle1)
	addDataFrame(x=matrix(adj.psi_1.est, ncol=1), sheet=sheets[["AppliedEstimates"]], row.names = FALSE, col.names = FALSE, startColumn = col+time.pt-1+ifelse(is.null(SL.library), 0, 9), startRow=row+2+16, colStyle=colStyle1)
	
	## Contrasts ##
	if(is.null(SL.library)) {
	#if(FALSE) {
		ate.uadj = paste(formatC(results$ate$estimates["uadj"], digits=3, format="f"), " (", formatC(sd(results$bootEsts[,"ate.uadj"]), digits=3, format="f"), ") [", formatC(quantile(results$bootEsts[,"ate.uadj"], .025), digits=2, format="f"), ",", formatC(quantile(results$bootEsts[,"ate.uadj"], .975), digits=2, format="f"), "]", sep="")
		ate.bootstrap = results$bootEsts[,paste("ate", names(results$ate$estimates)[-1], sep=".")]
		ate.est = paste(formatC(results$ate$estimates[-1], digits=3, format="f"), " (", formatC(apply(ate.bootstrap, 2, sd), digits=3, format="f"), ") [", formatC(apply(ate.bootstrap, 2, function(x) quantile(x, .025)), digits=2, format="f"), ",", formatC(apply(ate.bootstrap, 2, function(x) quantile(x, .975)), digits=2, format="f"), "]", sep="")
	} else {
		ate.uadj = paste(formatC(results$ate$estimates["uadj"], digits=3, format="f"), " (", "x.xxx", ") [", "x.xx,x.xx", "]", sep="")
		ate.est = paste(formatC(results$ate$estimates[-1], digits=3, format="f"), " (", "x.xxx", ") [", "x.xx,x.xx", "]", sep="")
	}
	addDataFrame(x=matrix(ate.uadj, ncol=1), sheet=sheets[["AppliedEstimates"]], row.names = FALSE, col.names = FALSE, startColumn = col+time.pt-1+ifelse(is.null(SL.library), 0, 9), startRow=row+2+16+17, colStyle=colStyle1)
	addDataFrame(x=matrix(ate.est, ncol=1), sheet=sheets[["AppliedEstimates"]], row.names = FALSE, col.names = FALSE, startColumn = col+time.pt-1+ifelse(is.null(SL.library), 0, 9), startRow=row+2+16+19, colStyle=colStyle1)

	## IF based SEs ##
	addDataFrame(x=matrix(results$ate$SE["uadj"], ncol=1), sheet=sheets[["AppliedEstimates"]], row.names = FALSE, col.names = FALSE, startColumn = col+time.pt-1+ifelse(is.null(SL.library), 0, 9), startRow=row+2+16+17+18, colStyle=colStyle2)
	addDataFrame(x=matrix(results$ate$SE[-1], ncol=1), sheet=sheets[["AppliedEstimates"]], row.names = FALSE, col.names = FALSE, startColumn = col+time.pt-1+ifelse(is.null(SL.library), 0, 9), startRow=row+2+16+19+18, colStyle=colStyle2)
	
	## COVERAGE ##
	if(is.null(SL.library)) {
	#if(FALSE) {
		psi_0.bootstrap = results$bootEsts[,paste("psi_0", names(results$psi_0$estimates), sep=".")]		
		psi_0.cover = ifelse(apply(psi_0.bootstrap, 2, function(x) quantile(x, .025)) <= results$psi_0$estimates & results$psi_0$estimates <= apply(psi_0.bootstrap, 2, function(x) quantile(x, .975)), 1, 0)
		psi_1.bootstrap = results$bootEsts[,paste("psi_1", names(results$psi_1$estimates), sep=".")]		
		psi_1.cover = ifelse(apply(psi_1.bootstrap, 2, function(x) quantile(x, .025)) <= results$psi_1$estimates & results$psi_1$estimates <= apply(psi_1.bootstrap, 2, function(x) quantile(x, .975)), 1, 0)
		ate.bootstrap = results$bootEsts[,paste("ate", names(results$ate$estimates), sep=".")]
		ate.cover = ifelse(apply(ate.bootstrap, 2, function(x) quantile(x, .025)) <= results$ate$estimates & results$ate$estimates <= apply(ate.bootstrap, 2, function(x) quantile(x, .975)), 1, 0)
		addDataFrame(x=matrix(psi_0.cover, ncol=1), sheet=sheets[["AppliedEstimates"]], row.names = FALSE, col.names = FALSE, startColumn = col+time.pt-1+ifelse(is.null(SL.library), 0, 9), startRow=row+2+16+17+27)
		addDataFrame(x=matrix(psi_1.cover, ncol=1), sheet=sheets[["AppliedEstimates"]], row.names = FALSE, col.names = FALSE, startColumn = col+time.pt-1+ifelse(is.null(SL.library), 0, 9), startRow=row+2+16+17+27+12)
		addDataFrame(x=matrix(ate.cover, ncol=1), sheet=sheets[["AppliedEstimates"]], row.names = FALSE, col.names = FALSE, startColumn = col+time.pt-1+ifelse(is.null(SL.library), 0, 9), startRow=row+2+16+17+27+12+12)
	}

	if(fits) {
		
		## READS FITS ##
		ltmle.psi_0 = readRDS(file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_0.ltmle.Rds", sep=""))
		ltmls.psi_0 = readRDS(file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_0.ltmls.Rds", sep=""))
		gcomp.psi_0 = readRDS(file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_0.gcomp.Rds", sep=""))
		ICEcs.psi_0 = readRDS(file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_0.ICEcs.Rds", sep=""))
		ICEws.psi_0 = readRDS(file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_0.ICEws.Rds", sep=""))
		ICEcp.psi_0 = readRDS(file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_0.ICEcp.Rds", sep=""))
		ICEwp.psi_0 = readRDS(file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_0.ICEwp.Rds", sep=""))
		aipws.psi_0 = readRDS(file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_0.aipws.Rds", sep=""))
		aipwp.psi_0 = readRDS(file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_0.aipwp.Rds", sep=""))
		ltmle.psi_1 = readRDS(file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_1.ltmle.Rds", sep=""))
		ltmls.psi_1 = readRDS(file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_1.ltmls.Rds", sep=""))
		gcomp.psi_1 = readRDS(file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_1.gcomp.Rds", sep=""))
		ICEcs.psi_1 = readRDS(file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_1.ICEcs.Rds", sep=""))
		ICEws.psi_1 = readRDS(file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_1.ICEws.Rds", sep=""))
		ICEcp.psi_1 = readRDS(file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_1.ICEcp.Rds", sep=""))
		ICEwp.psi_1 = readRDS(file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_1.ICEwp.Rds", sep=""))
		aipws.psi_1 = readRDS(file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_1.aipws.Rds", sep=""))
		aipwp.psi_1 = readRDS(file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_1.aipwp.Rds", sep=""))
		
		## INTIALIZES ##
		if(is.null(SL.library)) {
			covariates = c("(Intercept)", "ageb", "cd4arv.p100", "clintypeelig.ruralhealthcenter..sub.districthospital", "clintypeelig.ruralhealthcenter.refhospital", "male", "pibasedMR", "tbtx_arv", "urbanelig", "whoECmax.1.2", "whoECmax.1.3", "whoECmax.1.4", "whoatarvstart.1.2", "whoatarvstart.1.3", "whoatarvstart.1.4", "arvadhere.0", "cd4_nadir.p100.0", "cd4_zenith.p100.0", "cd4v_locf.cat.ge500.lt200.0", "cd4v_locf.cat.ge500.200to350.0", "cd4v_locf.cat.ge500.350to500.0", "clintype.ruralhealthcenter..sub.districthospital.0", "clintype.ruralhealthcenter.refhospital.0", "date2007.p180.0", "daysfromenroll.p180.0", "onARV.0", "pregnant_locf.l.0", "prevSeen.0", "stage34_locf.0", "tbtx_locf.l.0", "urbanclin.0", "enroll.0", "arvadhere.1", "cd4_nadir.p100.1", "cd4_zenith.p100.1", "cd4v_locf.cat.ge500.lt200.1", "cd4v_locf.cat.ge500.200to350.1", "cd4v_locf.cat.ge500.350to500.1", "clintype.ruralhealthcenter..sub.districthospital.1", "clintype.ruralhealthcenter.refhospital.1", "date2007.p180.1", "daysfromenroll.p180.1", "onARV.1", "pregnant_locf.l.1", "prevSeen.1", "stage34_locf.1", "tbtx_locf.l.1", "urbanclin.1", "enroll.1", "arvadhere.2", "cd4_nadir.p100.2", "cd4_zenith.p100.2", "cd4v_locf.cat.ge500.lt200.2", "cd4v_locf.cat.ge500.200to350.2", "cd4v_locf.cat.ge500.350to500.2", "clintype.ruralhealthcenter..sub.districthospital.2", "clintype.ruralhealthcenter.refhospital.2", "date2007.p180.2", "daysfromenroll.p180.2", "onARV.2", "pregnant_locf.l.2", "prevSeen.2", "stage34_locf.2", "tbtx_locf.l.2", "urbanclin.2", "enroll.2", "arvadhere.3", "cd4_nadir.p100.3", "cd4_zenith.p100.3", "cd4v_locf.cat.ge500.lt200.3", "cd4v_locf.cat.ge500.200to350.3", "cd4v_locf.cat.ge500.350to500.3", "clintype.ruralhealthcenter..sub.districthospital.3", "clintype.ruralhealthcenter.refhospital.3", "date2007.p180.3", "daysfromenroll.p180.3", "onARV.3", "pregnant_locf.l.3", "prevSeen.3", "stage34_locf.3", "tbtx_locf.l.3", "urbanclin.3", "enroll.3", "arvadhere.4", "cd4_nadir.p100.4", "cd4_zenith.p100.4", "cd4v_locf.cat.ge500.lt200.4", "cd4v_locf.cat.ge500.200to350.4", "cd4v_locf.cat.ge500.350to500.4", "clintype.ruralhealthcenter..sub.districthospital.4", "clintype.ruralhealthcenter.refhospital.4", "date2007.p180.4", "daysfromenroll.p180.4", "onARV.4", "pregnant_locf.l.4", "prevSeen.4", "stage34_locf.4", "tbtx_locf.l.4", "urbanclin.4", "enroll.4", "arvadhere.5", "cd4_nadir.p100.5", "cd4_zenith.p100.5", "cd4v_locf.cat.ge500.lt200.5", "cd4v_locf.cat.ge500.200to350.5", "cd4v_locf.cat.ge500.350to500.5", "clintype.ruralhealthcenter..sub.districthospital.5", "clintype.ruralhealthcenter.refhospital.5", "date2007.p180.5", "daysfromenroll.p180.5", "onARV.5", "pregnant_locf.l.5", "prevSeen.5", "stage34_locf.5", "tbtx_locf.l.5", "urbanclin.5", "enroll.5", "arvadhere.6", "cd4_nadir.p100.6", "cd4_zenith.p100.6", "cd4v_locf.cat.ge500.lt200.6", "cd4v_locf.cat.ge500.200to350.6", "cd4v_locf.cat.ge500.350to500.6", "clintype.ruralhealthcenter..sub.districthospital.6", "clintype.ruralhealthcenter.refhospital.6", "date2007.p180.6", "daysfromenroll.p180.6", "onARV.6", "pregnant_locf.l.6", "prevSeen.6", "stage34_locf.6", "tbtx_locf.l.6", "urbanclin.6", "enroll.6")
			coefs.ltmle.psi_0 = coefs.ltmls.psi_0 = coefs.gcomp.psi_0 = coefs.aipws.psi_0 = coefs.aipwp.psi_0 = coefs.ltmle.psi_1 = coefs.ltmls.psi_1 = coefs.gcomp.psi_1 = coefs.aipws.psi_1 = coefs.aipwp.psi_1 = matrix(nrow=length(covariates), ncol=time.pt, dimnames=list(covariates, paste("t", 1:time.pt, sep="")))
			coefs.ICEws.psi_0 = coefs.ICEwp.psi_0 = coefs.ICEcs.psi_0 = coefs.ICEcp.psi_0 = coefs.ICEws.psi_1 = coefs.ICEwp.psi_1 = coefs.ICEcs.psi_1 = coefs.ICEcp.psi_1 = matrix(nrow=length(covariates)+1, ncol=time.pt, dimnames=list(c(covariates, "cleverCov"), paste("t", 1:time.pt, sep="")))
		} else {
			candidates = names(ltmle.psi_0$fit$Q[[1]]$coef)
			coefs.ltmle.psi_0 = coefs.ltmls.psi_0 = coefs.gcomp.psi_0 = coefs.aipws.psi_0 = coefs.aipwp.psi_0 = coefs.ltmle.psi_1 = coefs.ltmls.psi_1 = coefs.gcomp.psi_1 = coefs.aipws.psi_1 = coefs.aipwp.psi_1 = coefs.ICEws.psi_0 = coefs.ICEwp.psi_0 = coefs.ICEcs.psi_0 = coefs.ICEcp.psi_0 = coefs.ICEws.psi_1 = coefs.ICEwp.psi_1 = coefs.ICEcs.psi_1 = coefs.ICEcp.psi_1 = matrix(nrow=length(candidates), ncol=time.pt, dimnames=list(candidates, paste("t", 1:time.pt, sep="")))
			risks.ltmle.psi_0 = risks.ltmls.psi_0 = risks.gcomp.psi_0 = risks.aipws.psi_0 = risks.aipwp.psi_0 = risks.ltmle.psi_1 = risks.ltmls.psi_1 = risks.gcomp.psi_1 = risks.aipws.psi_1 = risks.aipwp.psi_1 = risks.ICEws.psi_0 = risks.ICEwp.psi_0 = risks.ICEcs.psi_0 = risks.ICEcp.psi_0 = risks.ICEws.psi_1 = risks.ICEwp.psi_1 = risks.ICEcs.psi_1 = risks.ICEcp.psi_1 = matrix(nrow=length(candidates), ncol=time.pt, dimnames=list(candidates, paste("t", 1:time.pt, sep="")))
		}
		
		for(i in 1:time.pt) {
			if(is.null(SL.library)) {
				
				coefs.ltmle.psi_0[names(coef(ltmle.psi_0$fit$Q[[i]])),i] = coef(ltmle.psi_0$fit$Q[[i]])
				coefs.ltmls.psi_0[names(coef(ltmls.psi_0$fit$Q[[i]])),i] = coef(ltmls.psi_0$fit$Q[[i]])
				coefs.gcomp.psi_0[names(coef(gcomp.psi_0$fit$Q[[i]])),i] = coef(gcomp.psi_0$fit$Q[[i]])
				coefs.ICEcs.psi_0[names(coef(ICEcs.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]])),i] = coef(ICEcs.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]])
				coefs.ICEws.psi_0[names(coef(ICEws.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]])),i] = coef(ICEws.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]])
				coefs.ICEcp.psi_0[names(coef(ICEcp.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]])),i] = coef(ICEcp.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]])
				coefs.ICEwp.psi_0[names(coef(ICEwp.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]])),i] = coef(ICEwp.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]])
				coefs.aipws.psi_0[names(coef(aipws.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]])),i] = coef(aipws.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]])
				coefs.aipwp.psi_0[names(coef(aipwp.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]])),i] = coef(aipwp.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]])
				coefs.ltmle.psi_1[names(coef(ltmle.psi_1$fit$Q[[i]])),i] = coef(ltmle.psi_1$fit$Q[[i]])
				coefs.ltmls.psi_1[names(coef(ltmls.psi_1$fit$Q[[i]])),i] = coef(ltmls.psi_1$fit$Q[[i]])
				coefs.gcomp.psi_1[names(coef(gcomp.psi_1$fit$Q[[i]])),i] = coef(gcomp.psi_1$fit$Q[[i]])
				coefs.ICEcs.psi_1[names(coef(ICEcs.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]])),i] = coef(ICEcs.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]])
				coefs.ICEws.psi_1[names(coef(ICEws.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]])),i] = coef(ICEws.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]])
				coefs.ICEcp.psi_1[names(coef(ICEcp.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]])),i] = coef(ICEcp.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]])
				coefs.ICEwp.psi_1[names(coef(ICEwp.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]])),i] = coef(ICEwp.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]])
				coefs.aipws.psi_1[names(coef(aipws.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]])),i] = coef(aipws.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]])
				coefs.aipwp.psi_1[names(coef(aipwp.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]])),i] = coef(aipwp.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]])
				
			} else {
				
				risks.ltmle.psi_0[names(ltmle.psi_0$fit$Q[[i]]$cvRisk),i] = ltmle.psi_0$fit$Q[[i]]$cvRisk
				risks.ltmls.psi_0[names(ltmls.psi_0$fit$Q[[i]]$cvRisk),i] = ltmls.psi_0$fit$Q[[i]]$cvRisk
				risks.gcomp.psi_0[names(gcomp.psi_0$fit$Q[[i]]$cvRisk),i] = gcomp.psi_0$fit$Q[[i]]$cvRisk
				risks.ICEcs.psi_0[names(ICEcs.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk),i] = ICEcs.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk
				risks.ICEws.psi_0[names(ICEws.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk),i] = ICEws.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk
				risks.ICEcp.psi_0[names(ICEcp.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk),i] = ICEcp.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk
				risks.ICEwp.psi_0[names(ICEwp.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk),i] = ICEwp.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk
				risks.aipws.psi_0[names(aipws.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk),i] = aipws.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk
				risks.aipwp.psi_0[names(aipwp.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk),i] = aipwp.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk
				risks.ltmle.psi_1[names(ltmle.psi_1$fit$Q[[i]]$cvRisk),i] = ltmle.psi_1$fit$Q[[i]]$cvRisk
				risks.ltmls.psi_1[names(ltmls.psi_1$fit$Q[[i]]$cvRisk),i] = ltmls.psi_1$fit$Q[[i]]$cvRisk
				risks.gcomp.psi_1[names(gcomp.psi_1$fit$Q[[i]]$cvRisk),i] = gcomp.psi_1$fit$Q[[i]]$cvRisk
				risks.ICEcs.psi_1[names(ICEcs.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk),i] = ICEcs.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk
				risks.ICEws.psi_1[names(ICEws.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk),i] = ICEws.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk
				risks.ICEcp.psi_1[names(ICEcp.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk),i] = ICEcp.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk
				risks.ICEwp.psi_1[names(ICEwp.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk),i] = ICEwp.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk
				risks.aipws.psi_1[names(aipws.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk),i] = aipws.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk
				risks.aipwp.psi_1[names(aipwp.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk),i] = aipwp.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$cvRisk

				coefs.ltmle.psi_0[names(ltmle.psi_0$fit$Q[[i]]$coef),i] = ltmle.psi_0$fit$Q[[i]]$coef
				coefs.ltmls.psi_0[names(ltmls.psi_0$fit$Q[[i]]$coef),i] = ltmls.psi_0$fit$Q[[i]]$coef
				coefs.gcomp.psi_0[names(gcomp.psi_0$fit$Q[[i]]$coef),i] = gcomp.psi_0$fit$Q[[i]]$coef
				coefs.ICEcs.psi_0[names(ICEcs.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef),i] = ICEcs.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef
				coefs.ICEws.psi_0[names(ICEws.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef),i] = ICEws.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef
				coefs.ICEcp.psi_0[names(ICEcp.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef),i] = ICEcp.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef
				coefs.ICEwp.psi_0[names(ICEwp.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef),i] = ICEwp.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef
				coefs.aipws.psi_0[names(aipws.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef),i] = aipws.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef
				coefs.aipwp.psi_0[names(aipwp.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef),i] = aipwp.psi_0$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef
				coefs.ltmle.psi_1[names(ltmle.psi_1$fit$Q[[i]]$coef),i] = ltmle.psi_1$fit$Q[[i]]$coef
				coefs.ltmls.psi_1[names(ltmls.psi_1$fit$Q[[i]]$coef),i] = ltmls.psi_1$fit$Q[[i]]$coef
				coefs.gcomp.psi_1[names(gcomp.psi_1$fit$Q[[i]]$coef),i] = gcomp.psi_1$fit$Q[[i]]$coef
				coefs.ICEcs.psi_1[names(ICEcs.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef),i] = ICEcs.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef
				coefs.ICEws.psi_1[names(ICEws.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef),i] = ICEws.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef
				coefs.ICEcp.psi_1[names(ICEcp.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef),i] = ICEcp.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef
				coefs.ICEwp.psi_1[names(ICEwp.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef),i] = ICEwp.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef
				coefs.aipws.psi_1[names(aipws.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef),i] = aipws.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef
				coefs.aipwp.psi_1[names(aipwp.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef),i] = aipwp.psi_1$Qfits[[paste("dead.ltfu", i, sep=".")]]$coef
				
			}
		}

		if(is.null(SL.library)) {
			
			addDataFrame(x=coefs.gcomp.psi_0, sheet=sheets[["Qbar-GLM"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=6, colStyle=colStyle3)
			addDataFrame(x=coefs.ICEcs.psi_0, sheet=sheets[["Qbar-GLM"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=148, colStyle=colStyle3)
			addDataFrame(x=coefs.ICEws.psi_0, sheet=sheets[["Qbar-GLM"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=291, colStyle=colStyle3)
			addDataFrame(x=coefs.ICEcp.psi_0, sheet=sheets[["Qbar-GLM"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=434, colStyle=colStyle3)
			addDataFrame(x=coefs.ICEwp.psi_0, sheet=sheets[["Qbar-GLM"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=577, colStyle=colStyle3)
			addDataFrame(x=coefs.aipws.psi_0, sheet=sheets[["Qbar-GLM"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=720, colStyle=colStyle3)
			addDataFrame(x=coefs.aipwp.psi_0, sheet=sheets[["Qbar-GLM"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=862, colStyle=colStyle3)
			addDataFrame(x=coefs.ltmls.psi_0, sheet=sheets[["Qbar-GLM"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=1004, colStyle=colStyle3)
			addDataFrame(x=coefs.ltmle.psi_0, sheet=sheets[["Qbar-GLM"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=1146, colStyle=colStyle3)
			addDataFrame(x=coefs.gcomp.psi_1, sheet=sheets[["Qbar-GLM"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=6, colStyle=colStyle3)
			addDataFrame(x=coefs.ICEcs.psi_1, sheet=sheets[["Qbar-GLM"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=148, colStyle=colStyle3)
			addDataFrame(x=coefs.ICEws.psi_1, sheet=sheets[["Qbar-GLM"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=291, colStyle=colStyle3)
			addDataFrame(x=coefs.ICEcp.psi_1, sheet=sheets[["Qbar-GLM"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=434, colStyle=colStyle3)
			addDataFrame(x=coefs.ICEwp.psi_1, sheet=sheets[["Qbar-GLM"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=577, colStyle=colStyle3)
			addDataFrame(x=coefs.aipws.psi_1, sheet=sheets[["Qbar-GLM"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=720, colStyle=colStyle3)
			addDataFrame(x=coefs.aipwp.psi_1, sheet=sheets[["Qbar-GLM"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=862, colStyle=colStyle3)
			addDataFrame(x=coefs.ltmls.psi_1, sheet=sheets[["Qbar-GLM"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=1004, colStyle=colStyle3)
			addDataFrame(x=coefs.ltmle.psi_1, sheet=sheets[["Qbar-GLM"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=1146, colStyle=colStyle3)
			
		} else {
			
			addDataFrame(x=risks.gcomp.psi_0, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=6, colStyle=colStyle3)
			addDataFrame(x=risks.ICEcs.psi_0, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=40, colStyle=colStyle3)
			addDataFrame(x=risks.ICEws.psi_0, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=74, colStyle=colStyle3)
			addDataFrame(x=risks.ICEcp.psi_0, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=108, colStyle=colStyle3)
			addDataFrame(x=risks.ICEwp.psi_0, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=142, colStyle=colStyle3)
			addDataFrame(x=risks.aipws.psi_0, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=176, colStyle=colStyle3)
			addDataFrame(x=risks.aipwp.psi_0, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=210, colStyle=colStyle3)
			addDataFrame(x=risks.ltmls.psi_0, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=244, colStyle=colStyle3)
			addDataFrame(x=risks.ltmle.psi_0, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=278, colStyle=colStyle3)
			addDataFrame(x=risks.gcomp.psi_1, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=6, colStyle=colStyle3)
			addDataFrame(x=risks.ICEcs.psi_1, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=40, colStyle=colStyle3)
			addDataFrame(x=risks.ICEws.psi_1, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=74, colStyle=colStyle3)
			addDataFrame(x=risks.ICEcp.psi_1, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=108, colStyle=colStyle3)
			addDataFrame(x=risks.ICEwp.psi_1, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=142, colStyle=colStyle3)
			addDataFrame(x=risks.aipws.psi_1, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=176, colStyle=colStyle3)
			addDataFrame(x=risks.aipwp.psi_1, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=210, colStyle=colStyle3)
			addDataFrame(x=risks.ltmls.psi_1, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=244, colStyle=colStyle3)
			addDataFrame(x=risks.ltmle.psi_1, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=278, colStyle=colStyle3)
			
			addDataFrame(x=coefs.gcomp.psi_0, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=6+15, colStyle=colStyle3)
			addDataFrame(x=coefs.ICEcs.psi_0, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=40+15, colStyle=colStyle3)
			addDataFrame(x=coefs.ICEws.psi_0, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=74+15, colStyle=colStyle3)
			addDataFrame(x=coefs.ICEcp.psi_0, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=108+15, colStyle=colStyle3)
			addDataFrame(x=coefs.ICEwp.psi_0, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=142+15, colStyle=colStyle3)
			addDataFrame(x=coefs.aipws.psi_0, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=176+15, colStyle=colStyle3)
			addDataFrame(x=coefs.aipwp.psi_0, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=210+15, colStyle=colStyle3)
			addDataFrame(x=coefs.ltmls.psi_0, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=244+15, colStyle=colStyle3)
			addDataFrame(x=coefs.ltmle.psi_0, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2), startRow=278+15, colStyle=colStyle3)
			addDataFrame(x=coefs.gcomp.psi_1, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=6+15, colStyle=colStyle3)
			addDataFrame(x=coefs.ICEcs.psi_1, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=40+15, colStyle=colStyle3)
			addDataFrame(x=coefs.ICEws.psi_1, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=74+15, colStyle=colStyle3)
			addDataFrame(x=coefs.ICEcp.psi_1, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=108+15, colStyle=colStyle3)
			addDataFrame(x=coefs.ICEwp.psi_1, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=142+15, colStyle=colStyle3)
			addDataFrame(x=coefs.aipws.psi_1, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=176+15, colStyle=colStyle3)
			addDataFrame(x=coefs.aipwp.psi_1, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=210+15, colStyle=colStyle3)
			addDataFrame(x=coefs.ltmls.psi_1, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=244+15, colStyle=colStyle3)
			addDataFrame(x=coefs.ltmle.psi_1, sheet=sheets[["Qbar-SL"]], row.names = FALSE, col.names = FALSE, startColumn = round(1+0.5*time.pt+0.5*time.pt^2)+35, startRow=278+15, colStyle=colStyle3)
			
		}
		
	}
	
	saveWorkbook(parms, file=file)
	
}

