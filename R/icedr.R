###############################################################################
# Description: Implements the double robust estimator from Bang and Robins 2005 
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Apr 28, 2015
###############################################################################


#' Double Robust Sequential Regression Estimation
#' 
#' \code{icedr} Estimates the parameter of interest (E[Y_d]) by using the sequential regression approach as presented by Bang and Robins (2005). This function only works in the setting where the outcome is binary or the outcome is continuous with a censoring intervention
#' 
#' @param data data frame following the time-ordering of the nodes.
#' @param Ynodes column names or indicies in \code{data} of outcome nodes.
#' @param Anodes column names or indicies in \code{data} of treatment nodes. 
#' @param Cnodes column names or indicies in \code{data} of censoring nodes.
#' @param abar binary vector (numAnodes x 1) of counterfactual treatment
#' @param cum.g a matrix of the cumulative probabilities of treatment (and being uncensored) given the parents. 
#' @param Qform character vector of regression formulas for \code{Qbar}.
#' @param SL.library optional character vector of libraries to pass to SuperLearner. NULL indicates glm should be called instead of SuperLearner.
#' @param family Currently allows \code{gaussian} or \code{binomial} to describe the error distribution. Link function information will be ignored.
#' @param stratify if \code{TRUE} condition on following \code{abar} when estimating \code{Qbar}. If \code{FALSE}, pool over all subjects.
#' @param g.weight if \code{TRUE}, then use the g fits as weights in the \code{Qbar} fit, rather than as a covariate.
#' 
#' @return \code{icedr} returns a list of items as an object of class \code{icedr}, which include
#' \itemize{
#' 	\item {The estimate of the parameter value under the intervention \code{abar}}
#' 	\item {A matrix of the condition expectation fit \code{Qbar} under \code{abar} for each time point.}
#' 	\item {The conditional expectation fits for \code{Qbar}}
#' 	\item {The call to the function}
#' }
#' 
#' @export
icedr = function(data, Ynodes, Anodes, Cnodes=NULL, abar, cum.g, Qform, SL.library=NULL, family="quasibinomial", g.weight=FALSE, stratify=TRUE) {

	## Initializes ##
	Q.kplus1 = data[,Ynodes[length(Ynodes)]]
	Qbar.hat = matrix(nrow=nrow(data), ncol=length(Ynodes))
	if(is.null(dim(cum.g))) {
		pi.inverse = matrix(1/cum.g, ncol=1)
	} else {
		pi.inverse = 1/cum.g
	}
	colnames(pi.inverse) = paste("pi.inverse.", 1:length(Ynodes)-1, sep="")
	Qfits = list()
	
	## Recursive regression ##
	for(i in length(Ynodes):1) {
		## Initializes ##
		if(family %in% c("quasibinomial", "binomial")) {
			# nb. In binary setting, rather than using previous outcome as predictor, we stratify
			Q.kplus1[data[,Ynodes[i]]==1] = 1
			if(i==1) {
				at.risk = rep(TRUE, nrow(data))
			} else {
				at.risk = data[,Ynodes[i-1]]==0
			}
		} else if(family=="gaussian") {
			at.risk = rep(TRUE, nrow(data))
		} else {
			stop("Function will only work with two scenarios (see help file).")
		}
		
		## Followed regime of interest ##		
		followed.abar = apply(data[,Anodes[1:i], drop=FALSE], 1, function(x) all(x==abar[1:i]))
		if(is.null(Cnodes)) {
			uncensored = rep(TRUE, nrow(data))
		} else {
			uncensored = apply(data[,Cnodes[1:(i*2)], drop=FALSE], 1, function(x) all(x=="uncensored", na.rm=T))
		}
		followed.abar = followed.abar*uncensored

		## Counterfactual data ##
		P_na.data = data
		for(a in 1:length(Anodes)) {
			P_na.data[,names(data)[Anodes][a]] = abar[a]
		}
		P_n = cbind(Q.kplus1, data[,all.vars(as.formula(Qform[i])[[3]])], pi.inverse[,i,drop=FALSE], followed.abar=followed.abar)
		P_na = cbind(Q.kplus1, P_na.data[,all.vars(as.formula(Qform[i])[[3]])], pi.inverse[,i,drop=FALSE], followed.abar=followed.abar)
		
		## Fits Qbar ##
		if(stratify) {
			fitData = P_n[at.risk & followed.abar & uncensored,, drop=FALSE]
		} else {
			fitData = P_n[at.risk & uncensored,, drop=FALSE]
		}
		if(is.null(SL.library)) {
			if(g.weight) {
				fitData$cleverCov = fitData$followed.abar * 1
				P_na$cleverCov = 1
				Qfit.abar = glm(as.formula(paste(Qform[i], "+ cleverCov")), data=fitData, family=family, weight=fitData[,paste("pi.inverse.", i-1, sep="")])
			} else {
				fitData$cleverCov = fitData$followed.abar * fitData[,paste("pi.inverse.", i-1, sep="")]
				P_na$cleverCov = P_na[,paste("pi.inverse.", i-1, sep="")]
				Qfit.abar = glm(as.formula(paste(Qform[i], "+ cleverCov")), data=fitData, family=family)
			}
		} else {
			tmp = complete.cases(P_na[,-1, drop=FALSE])
			if(family=="quasibinomial") family = "binomial"
			if(g.weight) {
				fitData$cleverCov = fitData$followed.abar * 1
				P_na$cleverCov = 1
				Qfit.abar = mcSuperLearner(Y=fitData$Q.kplus1, X=fitData[,all.vars(as.formula(paste(Qform[i], "+ cleverCov"))[[3]]), drop=FALSE], newX=P_na[tmp, all.vars(as.formula(paste(Qform[i], "+ cleverCov"))[[3]]), drop=FALSE], SL.library=SL.library, family=family, obsWeights = fitData[,paste("pi.inverse.", i-1, sep="")], control = list(trimLogit=.001, saveFitLibrary=FALSE), cvControl=list(V=8), method="method.NNloglik.LT", verbose=TRUE)
			} else {
				fitData$cleverCov = fitData$followed.abar * fitData[,paste("pi.inverse.", i-1, sep="")]
				P_na$cleverCov = P_na[,paste("pi.inverse.", i-1, sep="")]
				Qfit.abar = mcSuperLearner(Y=fitData$Q.kplus1, X=fitData[,all.vars(as.formula(paste(Qform[i], "+ cleverCov"))[[3]]), drop=FALSE], newX=P_na[tmp, all.vars(as.formula(paste(Qform[i], "+ cleverCov"))[[3]]), drop=FALSE], SL.library=SL.library, family=family, control = list(trimLogit=.001, saveFitLibrary=FALSE), cvControl=list(V=8), method="method.NNloglik.LT", verbose=TRUE)
			}
		}
		
		## Updates Q.kplus1 ##
		if(is.null(SL.library)) {
			Q.kplus1 = suppressWarnings(predict(Qfit.abar, newdata=P_na, type="response"))
		} else {
			## Need to correct the bug with SuperLearner
			library.predict = Qfit.abar$library.predict
			library.predict[is.na(library.predict)] = 0
			Qfit.abar$SL.predict = library.predict %*% Qfit.abar$coef
			Q.kplus1[tmp] = Qfit.abar$SL.predict
		}
		Qfits = c(Qfits, list(Qfit.abar))
		Qbar.hat[,i] = Q.kplus1
		names(Qfits)[length(Qfits)] = colnames(Qbar.hat)[i] = names(data)[Ynodes[i]]
		
	}

	out = list(estimate=mean(Q.kplus1), Qbar.hat=Qbar.hat, Qfits=Qfits, call=match.call())
	class(out) = "icedr"
	return(out)
	
}

#'@export
print.icedr = function(x, ...) {
	cat("Call:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")	
	cat("ICE-DR Estimate:\t", x$estimate, "\n")
	invisible(x)
}
