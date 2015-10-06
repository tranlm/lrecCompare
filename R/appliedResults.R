###############################################################################
# Description: Gets applied data estimatse
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: May 14, 2015
###############################################################################

#' @export
appliedResults = function(tmp, gmatrix.abar0, gmatrix.abar1, SL.library, time.pt, save=TRUE, verbose=FALSE) {	
	
	####################
	## ARGS for LTMLE ##
	####################
	Lnodes = Cnodes = gform.uadj = Qform.uadj = Qform = cum.g.name = NULL
	for(i in 1:time.pt){
		## NODES ##
		Lnodes = c(Lnodes, paste0(vars.time,".", i))
		Cnodes = c(Cnodes, paste0("transfer.",i-1), paste0("eofu.",i-1))
		cum.g.name = c(cum.g.name, paste0("enroll.",i-1), paste0("transfer.",i-1), paste0("eofu.",i-1))
		
		## UNADJUSTED MODELS ##
		gform.uadj = c(gform.uadj, paste("enroll.", i-1, " ~ 1", sep=""))
		gform.uadj = c(gform.uadj, paste("transfer.", i-1, " ~ 1", sep=""))
		gform.uadj = c(gform.uadj, paste("eofu.", i-1, " ~ 1", sep=""))
		Qform.uadj = c(Qform.uadj, paste("Q.kplus1 ~ 1 ", sep=""))
		Qform = c(Qform, paste("Q.kplus1 ~ ", paste(c(vars.base, paste0(c(vars.time, "enroll"),".",i-1)), collapse=" + ")))
		names(Qform.uadj)[length(Qform.uadj)] = names(Qform)[length(Qform)] = paste("arvadhere.", i, sep="")
	}
	Ynodes = grep("^dead.ltfu.", names(tmp)) 
	Anodes = grep("^enroll.", names(tmp))
	Qform = NULL	
	
	####################
	## ESTIMATE PSI_0 ##
	####################
	unadj.psi_0 = ltmle(data=tmp, Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes, survivalOutcome=TRUE, Qform=Qform.uadj, gform=gform.uadj, abar=rep(0,time.pt), gbounds=c(0.001,1), deterministic.g.function=MaintainTreatment, estimate.time=FALSE, stratify=TRUE, SL.library=SL.library, IC.variance.only=TRUE)
	if(verbose) message("### Fitting psi_0 ltmle (pooled) ###")
	ltmle.psi_0 = ltmle(data=tmp, Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes, survivalOutcome=TRUE, Qform=Qform, gform=gmatrix.abar0[,1:length(c(Anodes, Cnodes))], abar=rep(0,time.pt), gbounds=c(0.001,1), estimate.time=FALSE, stratify=FALSE, SL.library=SL.library, IC.variance.only=TRUE)
	if(verbose) message("### Fitting psi_0 ltmle (stratified) ###")
	ltmls.psi_0 = ltmle(data=tmp, Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes, survivalOutcome=TRUE, Qform=Qform, gform=gmatrix.abar0[,1:length(c(Anodes, Cnodes))], abar=rep(0,time.pt), gbounds=c(0.001,1), estimate.time=FALSE, stratify=TRUE, SL.library=SL.library, IC.variance.only=TRUE)
	if(verbose) message("### Fitting psi_0 gcomp (pooled) ###")
	gcomp.psi_0 = ltmle(data=tmp, Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes, survivalOutcome=TRUE, Qform=Qform, gform=gmatrix.abar0[,1:length(c(Anodes, Cnodes))], abar=rep(0,time.pt), gbounds=c(0.001,1), estimate.time=FALSE, stratify=FALSE, SL.library=SL.library, gcomp=TRUE, IC.variance.only=TRUE)
	if(verbose) message("### Fitting psi_0 icedr (cov, stratified) ###")
	ICEcs.psi_0 = icedr(data=tmp, Ynodes=Ynodes, Anodes=Anodes, Cnodes=Cnodes, abar=rep(0,time.pt), cum.g=ltmle.psi_0$cum.g[,which(cum.g.name %in% names(tmp)[Anodes])+2], Qform=ltmle.psi_0$formulas$Qform, SL.library=SL.library, g.weight=FALSE, stratify=TRUE)
	if(verbose) message("### Fitting psi_0 icedr (weight, stratified) ###")
	ICEws.psi_0 = icedr(data=tmp, Ynodes=Ynodes, Anodes=Anodes, Cnodes=Cnodes, abar=rep(0,time.pt), cum.g=ltmle.psi_0$cum.g[,which(cum.g.name %in% names(tmp)[Anodes])+2], Qform=ltmle.psi_0$formulas$Qform, SL.library=SL.library, g.weight=TRUE, stratify=TRUE)
	if(verbose) message("### Fitting psi_0 icedr (cov, pooled) ###")
	ICEcp.psi_0 = icedr(data=tmp, Ynodes=Ynodes, Anodes=Anodes, Cnodes=Cnodes, abar=rep(0,time.pt), cum.g=ltmle.psi_0$cum.g[,which(cum.g.name %in% names(tmp)[Anodes])+2], Qform=ltmle.psi_0$formulas$Qform, SL.library=SL.library, g.weight=FALSE, stratify=FALSE)
	if(verbose) message("### Fitting psi_0 icedr (weight, pooled) ###")
	ICEwp.psi_0 = icedr(data=tmp, Ynodes=Ynodes, Anodes=Anodes, Cnodes=Cnodes, abar=rep(0,time.pt), cum.g=ltmle.psi_0$cum.g[,which(cum.g.name %in% names(tmp)[Anodes])+2], Qform=ltmle.psi_0$formulas$Qform, SL.library=SL.library, g.weight=TRUE, stratify=FALSE)
	if(verbose) message("### Fitting psi_0 aiptw (stratified) ###")
	aipws.psi_0 = aiptw(data=tmp, Ynodes=Ynodes, Anodes=Anodes, Cnodes=Cnodes, abar=rep(0,time.pt), cum.g=ltmle.psi_0$cum.g[,which(cum.g.name %in% names(tmp)[Anodes])+2], Qform=ltmle.psi_0$formulas$Qform, SL.library=SL.library, stratify=TRUE)
	if(verbose) message("### Fitting psi_0 aiptw (pooled) ###")
	aipwp.psi_0 = aiptw(data=tmp, Ynodes=Ynodes, Anodes=Anodes, Cnodes=Cnodes, abar=rep(0,time.pt), cum.g=ltmle.psi_0$cum.g[,which(cum.g.name %in% names(tmp)[Anodes])+2], Qform=ltmle.psi_0$formulas$Qform, SL.library=SL.library, stratify=FALSE)
	## SAVES ##
	if(save) {
		if(is.null(SL.library)) {
			saveRDS(unadj.psi_0, file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_0.unadj.Rds", sep=""))
		}
		saveRDS(ltmle.psi_0, file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_0.ltmle.Rds", sep=""))
		saveRDS(ltmls.psi_0, file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_0.ltmls.Rds", sep=""))
		saveRDS(gcomp.psi_0, file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_0.gcomp.Rds", sep=""))
		saveRDS(ICEcs.psi_0, file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_0.ICEcs.Rds", sep=""))
		saveRDS(ICEws.psi_0, file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_0.ICEws.Rds", sep=""))
		saveRDS(ICEcp.psi_0, file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_0.ICEcp.Rds", sep=""))
		saveRDS(ICEwp.psi_0, file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_0.ICEwp.Rds", sep=""))
		saveRDS(aipws.psi_0, file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_0.aipws.Rds", sep=""))
		saveRDS(aipwp.psi_0, file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_0.aipwp.Rds", sep=""))
	}
	
	####################
	## ESTIMATE PSI_1 ##
	####################
	unadj.psi_1 = ltmle(data=tmp, Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes, survivalOutcome=TRUE, Qform=Qform.uadj, gform=gform.uadj, abar=rep(1,time.pt), gbounds=c(0.001,1), deterministic.g.function=MaintainTreatment, estimate.time=FALSE, stratify=TRUE, SL.library=SL.library, IC.variance.only=TRUE)
	if(verbose) message("### Fitting psi_1 ltmle (pooled) ###")
	ltmle.psi_1 = ltmle(data=tmp, Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes, survivalOutcome=TRUE, Qform=Qform, gform=gmatrix.abar1[,1:length(c(Anodes, Cnodes))], abar=rep(1,time.pt), gbounds=c(0.001,1), estimate.time=FALSE, stratify=FALSE, SL.library=SL.library, IC.variance.only=TRUE)
	if(verbose) message("### Fitting psi_1 ltmle (stratified) ###")
	ltmls.psi_1 = ltmle(data=tmp, Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes, survivalOutcome=TRUE, Qform=Qform, gform=gmatrix.abar1[,1:length(c(Anodes, Cnodes))], abar=rep(1,time.pt), gbounds=c(0.001,1), estimate.time=FALSE, stratify=TRUE, SL.library=SL.library, IC.variance.only=TRUE)
	if(verbose) message("### Fitting psi_1 gcomp (pooled) ###")
	gcomp.psi_1 = ltmle(data=tmp, Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes, survivalOutcome=TRUE, Qform=Qform, gform=gmatrix.abar1[,1:length(c(Anodes, Cnodes))], abar=rep(1,time.pt), gbounds=c(0.001,1), estimate.time=FALSE, stratify=FALSE, SL.library=SL.library, gcomp=TRUE, IC.variance.only=TRUE)
	if(verbose) message("### Fitting psi_1 icedr (cov, stratified) ###")
	ICEcs.psi_1 = icedr(data=tmp, Ynodes=Ynodes, Anodes=Anodes, Cnodes=Cnodes, abar=rep(1,time.pt), cum.g=ltmle.psi_1$cum.g[,which(cum.g.name %in% names(tmp)[Anodes])+2], Qform=ltmle.psi_1$formulas$Qform, SL.library=SL.library, g.weight=FALSE, stratify=TRUE)
	if(verbose) message("### Fitting psi_1 icedr (weight, stratified) ###")
	ICEws.psi_1 = icedr(data=tmp, Ynodes=Ynodes, Anodes=Anodes, Cnodes=Cnodes, abar=rep(1,time.pt), cum.g=ltmle.psi_1$cum.g[,which(cum.g.name %in% names(tmp)[Anodes])+2], Qform=ltmle.psi_1$formulas$Qform, SL.library=SL.library, g.weight=TRUE, stratify=TRUE)
	if(verbose) message("### Fitting psi_1 icedr (cov, pooled) ###")
	ICEcp.psi_1 = icedr(data=tmp, Ynodes=Ynodes, Anodes=Anodes, Cnodes=Cnodes, abar=rep(1,time.pt), cum.g=ltmle.psi_1$cum.g[,which(cum.g.name %in% names(tmp)[Anodes])+2], Qform=ltmle.psi_1$formulas$Qform, SL.library=SL.library, g.weight=FALSE, stratify=FALSE)
	if(verbose) message("### Fitting psi_1 icedr (weight, pooled) ###")
	ICEwp.psi_1 = icedr(data=tmp, Ynodes=Ynodes, Anodes=Anodes, Cnodes=Cnodes, abar=rep(1,time.pt), cum.g=ltmle.psi_1$cum.g[,which(cum.g.name %in% names(tmp)[Anodes])+2], Qform=ltmle.psi_1$formulas$Qform, SL.library=SL.library, g.weight=TRUE, stratify=FALSE)
	if(verbose) message("### Fitting psi_1 aiptw (stratified) ###")
	aipws.psi_1 = aiptw(data=tmp, Ynodes=Ynodes, Anodes=Anodes, Cnodes=Cnodes, abar=rep(1,time.pt), cum.g=ltmle.psi_1$cum.g[,which(cum.g.name %in% names(tmp)[Anodes])+2], Qform=ltmle.psi_1$formulas$Qform, SL.library=SL.library, stratify=TRUE)
	if(verbose) message("### Fitting psi_1 aiptw (pooled) ###")
	aipwp.psi_1 = aiptw(data=tmp, Ynodes=Ynodes, Anodes=Anodes, Cnodes=Cnodes, abar=rep(1,time.pt), cum.g=ltmle.psi_1$cum.g[,which(cum.g.name %in% names(tmp)[Anodes])+2], Qform=ltmle.psi_1$formulas$Qform, SL.library=SL.library, stratify=FALSE)
	## SAVES ##
	if(save) {
		if(is.null(SL.library)) {
			saveRDS(unadj.psi_1, file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_1.unadj.Rds", sep=""))
		}
		saveRDS(ltmle.psi_1, file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_1.ltmle.Rds", sep=""))
		saveRDS(ltmls.psi_1, file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_1.ltmls.Rds", sep=""))
		saveRDS(gcomp.psi_1, file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_1.gcomp.Rds", sep=""))
		saveRDS(ICEcs.psi_1, file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_1.ICEcs.Rds", sep=""))
		saveRDS(ICEws.psi_1, file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_1.ICEws.Rds", sep=""))
		saveRDS(ICEcp.psi_1, file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_1.ICEcp.Rds", sep=""))
		saveRDS(ICEwp.psi_1, file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_1.ICEwp.Rds", sep=""))
		saveRDS(aipws.psi_1, file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_1.aipws.Rds", sep=""))
		saveRDS(aipwp.psi_1, file=paste("./inst/results/fits/t", time.pt, ifelse(is.null(SL.library), "-glm", "-SL"), "-psi_1.aipwp.Rds", sep=""))
	}
		
	#############
	## RESULTS ##
	#############
	psi_0 = list(
			estimates = c(
					uadj=unadj.psi_0$estimates[["tmle"]],
					gcomp=gcomp.psi_0$estimates[["gcomp"]],
					iptw=ltmle.psi_0$estimates[["iptw"]],
					ICEcs=ICEcs.psi_0$estimate,
					ICEws=ICEws.psi_0$estimate,
					ICEcp=ICEcp.psi_0$estimate,
					ICEwp=ICEwp.psi_0$estimate,
					aipws=aipws.psi_0$estimate,
					aipwp=aipwp.psi_0$estimate,
					ltmls=ltmls.psi_0$estimates[["tmle"]],
					ltmle=ltmle.psi_0$estimates[["tmle"]]
			),
			SE = c(
					uadj=sqrt(var(unadj.psi_0$IC$tmle)/nrow(tmp)),
					iptw=sqrt(var(ltmle.psi_0$IC$iptw)/nrow(tmp)),
					aipws=sqrt(var(aipws.psi_0$IC)/nrow(tmp)),
					aipwp=sqrt(var(aipwp.psi_0$IC)/nrow(tmp)),
					ltmls=sqrt(var(ltmls.psi_0$IC$tmle)/nrow(tmp)),
					ltmle=sqrt(var(ltmle.psi_0$IC$tmle)/nrow(tmp))
			),
			IC = list(
					uadj=unadj.psi_0$IC$tmle,
					iptw=ltmle.psi_0$IC$iptw,
					aipws=aipws.psi_0$IC,
					aipwp=aipwp.psi_0$IC,
					ltmls=ltmls.psi_0$IC$tmle,
					ltmle=ltmle.psi_0$IC$tmle
			)
	)
	psi_1 = list(
			estimates = c(
					uadj=unadj.psi_1$estimates[["tmle"]],
					gcomp=gcomp.psi_1$estimates[["gcomp"]],
					iptw=ltmle.psi_1$estimates[["iptw"]],
					ICEcs=ICEcs.psi_1$estimate,
					ICEws=ICEws.psi_1$estimate,
					ICEcp=ICEcp.psi_1$estimate,
					ICEwp=ICEwp.psi_1$estimate,
					aipws=aipws.psi_1$estimate,
					aipwp=aipwp.psi_1$estimate,
					ltmls=ltmls.psi_1$estimates[["tmle"]],
					ltmle=ltmle.psi_1$estimates[["tmle"]]
			),
			SE = c(
					uadj=sqrt(var(unadj.psi_1$IC$tmle)/nrow(tmp)),
					iptw=sqrt(var(ltmle.psi_1$IC$iptw)/nrow(tmp)),
					aipws=sqrt(var(aipws.psi_1$IC)/nrow(tmp)),
					aipwp=sqrt(var(aipwp.psi_1$IC)/nrow(tmp)),
					ltmls=sqrt(var(ltmls.psi_1$IC$tmle)/nrow(tmp)),
					ltmle=sqrt(var(ltmle.psi_1$IC$tmle)/nrow(tmp))
			),
			IC = list(
					uadj=unadj.psi_1$IC$tmle,
					iptw=ltmle.psi_1$IC$iptw,
					aipws=aipws.psi_1$IC,
					aipwp=aipwp.psi_1$IC,
					ltmls=ltmls.psi_1$IC$tmle,
					ltmle=ltmle.psi_1$IC$tmle
			)
	)
	ate = list(
			estimates = psi_1$estimates - psi_0$estimates,
			SE = c(
					uadj = sqrt(var(unadj.psi_1$IC$tmle - unadj.psi_0$IC$tmle)/nrow(tmp)),
					iptw = sqrt(var(ltmle.psi_1$IC$iptw - ltmle.psi_0$IC$iptw)/nrow(tmp)),
					aipws = sqrt(var(aipws.psi_1$IC - aipws.psi_0$IC)/nrow(tmp)),
					aipwp = sqrt(var(aipwp.psi_1$IC - aipwp.psi_0$IC)/nrow(tmp)),
					ltmls = sqrt(var(ltmls.psi_1$IC$tmle - ltmls.psi_0$IC$tmle)/nrow(tmp)),
					ltmle = sqrt(var(ltmle.psi_1$IC$tmle - ltmle.psi_0$IC$tmle)/nrow(tmp))
			),
			IC = list(
					uadj = unadj.psi_1$IC$tmle - unadj.psi_0$IC$tmle,
					iptw = ltmle.psi_1$IC$iptw - ltmle.psi_1$IC$iptw,
					aipws = aipws.psi_1$IC - aipws.psi_0$IC,
					aipwp = aipwp.psi_1$IC - aipwp.psi_0$IC,
					ltmls = ltmls.psi_1$IC$tmle - ltmls.psi_0$IC$tmle,
					ltmle = ltmle.psi_1$IC$tmle - ltmle.psi_0$IC$tmle
			)	
	)
	
	## Cumulative g matrix ##
	psi_0.cum.g = summary(ltmle.psi_0$cum.g[,ncol(ltmle.psi_0$cum.g)])
	psi_1.cum.g = summary(ltmle.psi_1$cum.g[,ncol(ltmle.psi_1$cum.g)])
	length(psi_0.cum.g) = length(psi_1.cum.g) = 7
	psi_0.cum.g = matrix(psi_0.cum.g, nrow=1, ncol=7, dimnames=list(NULL, c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "NAs")))
	psi_1.cum.g = matrix(psi_1.cum.g, nrow=1, ncol=7, dimnames=list(NULL, c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "NAs")))
	
	## Cumulative gUnbounded matrix ##
	psi_0.cum.g.unb = summary(ltmle.psi_0$cum.g.unbounded[,ncol(ltmle.psi_0$cum.g.unbounded)])
	psi_1.cum.g.unb = summary(ltmle.psi_1$cum.g.unbounded[,ncol(ltmle.psi_1$cum.g.unbounded)])
	length(psi_0.cum.g.unb) = length(psi_1.cum.g.unb) = 7
	psi_0.cum.g.unb = matrix(psi_0.cum.g.unb, nrow=1, ncol=7, dimnames=list(NULL, c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "NAs")))
	psi_1.cum.g.unb = matrix(psi_1.cum.g.unb, nrow=1, ncol=7, dimnames=list(NULL, c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "NAs")))

	## Low probabilities ##
	psi_0.lowprob = mean(ltmle.psi_0$cum.g[,ncol(ltmle.psi_0$cum.g.unbounded)]<=.001, na.rm=TRUE)
	psi_1.lowprob = mean(ltmle.psi_1$cum.g[,ncol(ltmle.psi_1$cum.g.unbounded)]<=.001, na.rm=TRUE)
	
	output = list(psi_0=psi_0, psi_1=psi_1, ate=ate, cum.g=list(psi_0=psi_0.cum.g, psi_1=psi_1.cum.g), cum.g.unb=list(psi_0=psi_0.cum.g.unb, psi_1=psi_1.cum.g.unb), lowprob=list(psi_0=psi_0.lowprob, psi_1=psi_1.lowprob))
	
	return(output)

}

