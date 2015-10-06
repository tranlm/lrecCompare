###############################################################################
# Description: Add comment
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Jun 17, 2015
###############################################################################


#' @export 
estimateG = function(data, vars.time, SL.library, save=TRUE) {

	## TRANSPOSES ##
	vars.time2 = c(vars.time, "enroll", "transfer", "eofu", "dead.ltfu")
	variables = c("patient_id", "rank", "enroll", "transfer", "eofu", "dead.ltfu", "ageb", "cd4arv.p100", "clintypeelig.ruralhealthcenter..sub.districthospital", "clintypeelig.ruralhealthcenter.refhospital", "date2007.p180", "male", "pibasedMR", "tbtx_arv", "urbanelig", "whoECmax.1.2", "whoECmax.1.3", "whoECmax.1.4", "whoatarvstart.1.2", "whoatarvstart.1.3", "whoatarvstart.1.4", "arvadhere", "cd4_nadir.p100", "cd4_zenith.p100", "cd4v_locf.cat.ge500.lt200", "cd4v_locf.cat.ge500.200to350", "cd4v_locf.cat.ge500.350to500", "clintype.ruralhealthcenter..sub.districthospital", "clintype.ruralhealthcenter.refhospital", "daysfromenroll.p180", "onARV", "pregnant_locf.l", "prevSeen", "stage34_locf", "tbtx_locf.l", "urbanclin")
	data$patient_id = rownames(data)
	data$dead.ltfu.0 = 0
	long.data = reshape(data, direction="long", idvar="patient_id", varying=lapply(vars.time2, function(x) grep(paste("^",x,sep=""), names(data))), v.names=vars.time2)
	long.data$transfer = ifelse(long.data$transfer=="censored", 1, 0)
	long.data$eofu = ifelse(long.data$eofu=="censored", 1, 0)
	long.data = reshape::rename(long.data, c(time="rank"))
	long.data$rank = long.data$rank - 1
	long.data = long.data[order(long.data$patient_id, long.data$rank),]
	data = subset(long.data, !(is.na(enroll) & is.na(eofu) & is.na(transfer)), select=variables)
	
	## SUBSETS ##
	#nb. from the transpose, dead.ltfu corresponds to the next time point for all obs
	#	 I therefore just ignored it when subsetting
	enrollPrev = rep(0, nrow(data))
	enrollPrev[2:nrow(data)] = data$enroll[1:(nrow(data)-1)]
	enrollPrev[data$rank==0] = 0
	A1.data = subset(data, enrollPrev==0)[,c(1,2,3,7:ncol(data))]
	C1.data = subset(data)[,c(1,2,3,4,7:ncol(data))]
	C2.data = subset(data, transfer==0)[,setdiff(names(data), vars.time)]
	C2.data = C2.data[,-c(3,4,6)]
	# Treatments of interest
	A1.data.psi = subset(data)[,c(1,2,7:ncol(data))]
	C1.data.psi = subset(data)[,c(1,2,3,7:ncol(data))]
	C2.data.psi = subset(data)[,setdiff(names(data), vars.time)]
	C2.data.psi = C2.data.psi[,-c(3,4,5,6)]

	if(!is.null(SL.library)) {

		## Enrollment ##
		message("### Estimating Enrollment Mechanism ###")
		A1.SL = mcSuperLearner(Y=A1.data$enroll, X=A1.data[,-c(1,3)], newX=A1.data.psi[,-c(1)], id=A1.data$patient_id, SL.library=SL.library, method="method.NNloglik.LT", family="binomial", control = list(trimLogit=.001, saveFitLibrary=FALSE), cvControl=list(V=8), verbose=TRUE)
		library.predict = A1.SL$library.predict
		library.predict[is.na(library.predict)] = 0
		A1.SL$SL.predict = library.predict %*% A1.SL$coef
		rm(list="library.predict")

		message("### Estimating Transfer Mechanism ###")
		if(any(C1.data$transfer>0, na.rm=TRUE)) {
			
			## Transfer (abar=0) ##
			C1.data.psi$enroll = 0
			C1.SL.abar0 = mcSuperLearner(Y=C1.data$transfer, X=C1.data[,-c(1,4)], newX=C1.data.psi[,-c(1)], id=C1.data$patient_id, SL.library=SL.library, method="method.NNloglik.LT", family="binomial", control=list(trimLogit=0.001, saveFitLibrary=FALSE), cvControl=list(V=8), verbose=TRUE)
			library.predict = C1.SL.abar0$library.predict
			library.predict[is.na(library.predict)] = 0
			C1.SL.abar0$SL.predict = library.predict %*% C1.SL.abar0$coef
			rm(list="library.predict")
			
			## Transfer (abar=1) ##
			C1.data.psi$enroll = 1
			C1.SL.abar1 = mcSuperLearner(Y=C1.data$transfer, X=C1.data[,-c(1,4)], newX=C1.data.psi[,-c(1)], id=C1.data$patient_id, SL.library=SL.library, method="method.NNloglik.LT", family="binomial", control=list(trimLogit=0.001, saveFitLibrary=FALSE), cvControl=list(V=8), verbose=TRUE)
			library.predict = C1.SL.abar1$library.predict
			library.predict[is.na(library.predict)] = 0
			C1.SL.abar1$SL.predict = library.predict %*% C1.SL.abar1$coef
			rm(list="library.predict")
		} else {
			C1.SL.abar0 = C1.SL.abar1 = list("No fit made, since all observations were uncensored.")
			C1.SL.abar0$SL.predict = C1.SL.abar1$SL.predict = rep(0, nrow(C1.data.psi))
		}
		
		## EOFU ##
		message("### Estimating EOFU Mechanism ###")
		C2.SL = mcSuperLearner(Y=C2.data$eofu, X=C2.data[,-c(1,3)], newX=C2.data.psi[,-c(1)], id=C2.data$patient_id, SL.library=SL.library, method="method.NNloglik.LT", family="binomial", control=list(trimLogit=0.001, saveFitLibrary=FALSE), cvControl=list(V=8), verbose=TRUE)
		library.predict = C2.SL$library.predict
		library.predict[is.na(library.predict)] = 0
		C2.SL$SL.predict = library.predict %*% C2.SL$coef
		rm(list="library.predict")
		
		## PREDICTIONS
		gA1.pred = as.vector(A1.SL$SL.predict)
		gC1.abar0.pred = as.vector(C1.SL.abar0$SL.predict)
		gC1.abar1.pred = as.vector(C1.SL.abar1$SL.predict)
		gC2.pred = as.vector(C2.SL$SL.predict)
		
		## SAVES ##
		if(save) {
			saveRDS(A1.SL, paste0("./inst/results/fits/A1.SL.RDS"))
			saveRDS(C1.SL.abar0, paste0("./inst/results/fits/C1.SL.abar0.RDS"))
			saveRDS(C1.SL.abar1, paste0("./inst/results/fits/C1.SL.abar1.RDS"))
			saveRDS(C2.SL, paste0("./inst/results/fits/C2.SL.RDS"))
		}
		
	} else {
		
		A1.glm = glm(enroll ~ ., data=A1.data[,-1], family="binomial")
		gA1.pred = predict(A1.glm, newdata=A1.data.psi[,-c(1)], type="response")
		
		if(any(C1.data$transfer>0, na.rm=TRUE)) {
			C1.glm = glm(transfer ~ ., data=C1.data[,-1], family="binomial")
			C1.data.psi$enroll = 0
			gC1.abar0.pred = predict(C1.glm, newdata=C1.data.psi[,-c(1)], type="response")
			C1.data.psi$enroll = 1
			gC1.abar1.pred = predict(C1.glm, newdata=C1.data.psi[,-c(1)], type="response")
		} else {
			C1.glm = "No fit made, since all observations were uncensored."
			gC1.abar0.pred = gC1.abar1.pred = rep(0, nrow(C1.data.psi))
		}
		
		C2.glm = glm(eofu ~ ., data=C2.data[,-1], family="binomial")
		gC2.pred = predict(C2.glm, newdata=C2.data.psi[,-c(1)], type="response")

		## SAVES ##
		if(save) {
			saveRDS(A1.glm, paste0("./inst/results/fits/A1.glm.RDS"))
			saveRDS(C1.glm, paste0("./inst/results/fits/C1.glm.RDS"))
			saveRDS(C2.glm, paste0("./inst/results/fits/C2.glm.RDS"))
		}
	}
	
	##############
	## g-matrix ##
	##############
	
	## abar_0 ##
	# n.b. We want P(not enroll), but ltmle package takes P(enroll)
	gA1.abar0 = gA1.pred
	gC1.abar0 = 1-gC1.abar0.pred
	gC2.abar0 = 1-gC2.pred
	
	## abar_1 ##
	gA1.abar1 = gA1.pred
	gA1.abar1[A1.data.psi$rank>0] = 1
	gC1.abar1 = 1-gC1.abar1.pred
	gC2.abar1 = 1-gC2.pred
	
	## Combines ##
	gA1 = cbind(A1.data.psi[,c("patient_id", "rank")], gA1.abar0, gA1.abar1)
	gC1 = cbind(C1.data.psi[,c("patient_id", "rank")], gC1.abar0, gC1.abar1)
	gC2 = cbind(C2.data.psi[,c("patient_id", "rank")], gC2.abar0, gC2.abar1)
	merge.data = list(subset(data, select=c('patient_id','rank','dead.ltfu')), gA1, gC1, gC2)
	gALL = Reduce(function(x, y) merge(x, y, all=T, by=c("patient_id", "rank")), merge.data, accumulate=F)
	gALL = gALL[order(gALL$patient_id, gALL$rank),]
	
	# Accounts for events
#	index = gALL$dead.ltfu==1 | is.na(gALL$dead.ltfu)
#	gALL$gA1.abar0[index] = gALL$gA1.abar1[index] = 1
#	gALL$gA2.abar0[index] = gALL$gA2.abar1[index] = 1
#	gALL$gC1.abar0[index] = gALL$gC1.abar1[index] = 1
#	gALL$gC2.abar0[index] = gALL$gC2.abar1[index] = 1
	
	## Reshapes to wide ##
	gmatrix.abar0 = gALL %>%
			subset(rank<7, select=c("patient_id", "rank", "gA1.abar0", "gC1.abar0", "gC2.abar0")) %>%
			rename(c(gA1.abar0="A1", gC1.abar0="C1", gC2.abar0="C2")) %>%
			reshape(direction="wide", idvar="patient_id", timevar="rank")
	gmatrix.abar1 = gALL %>%
			subset(rank<7, select=c("patient_id", "rank", "gA1.abar1", "gC1.abar1", "gC2.abar1")) %>%
			rename(c(gA1.abar1="A1", gC1.abar1="C1", gC2.abar1="C2")) %>%
			reshape(direction="wide", idvar="patient_id", timevar="rank") 
	rownames(gmatrix.abar0) = gmatrix.abar0$patient_id; rownames(gmatrix.abar1) = gmatrix.abar1$patient_id
	gmatrix.abar0$patient_id = gmatrix.abar1$patient_id = NULL
	gmatrix.abar0 = as.matrix(gmatrix.abar0); gmatrix.abar1 = as.matrix(gmatrix.abar1)
	
	## OUTPUT ##
	out = list(gmatrix.abar0=gmatrix.abar0, gmatrix.abar1=gmatrix.abar1)
	if(save) {
		saveRDS(out, paste("./data/gmatrix", ifelse(is.null(SL.library), "GLM", "SL"), ".RDS", sep=""))
	}
	return(out)
	
}

