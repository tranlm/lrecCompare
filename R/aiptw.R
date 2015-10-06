###############################################################################
# Description: Augmented IPTW estimator
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Apr 28, 2015
###############################################################################


#' Augmented Inverse Probability of Treatment Weighted Estimation
#' 
#' \code{aiptw} Estimates the parameter of interest (E[Y_d]) by directly solving the efficient influence function as an estimating equation. 
#' 
#' Please refer to van der Laan and Gruben (2007) for details regarding derivation of the efficient influence function, which this estimator solves.
#' 
#' @param data data frame following the time-ordering of the nodes.
#' @param cum.g a matrix of the cumulative probabilities of treatment (and being uncensored) given the parents. 
#' @param Ynodes column names or indicies in \code{data} of outcome nodes.
#' @param Anodes column names or indicies in \code{data} of treatment nodes. 
#' @param Cnodes column names or indicies in \code{data} of censoring nodes.
#' @param abar binary vector (numAnodes x 1) of counterfactual treatment
#' @param Qform character vector of regression formulas for Q.
#' @param SL.library optional character vector of libraries to pass to SuperLearner. NULL indicates glm should be called instead of SuperLearner.
#' @param stratify if \code{TRUE} condition on following \code{abar} when estimating Q and g. If \code{FALSE}, pool over all subjects.
#' 
#' @return \code{BangRobinsDR} returns a list of items as an object of class \code{aiptw}, which include
#' \itemize{
#' 	\item {The estimate of the parameter value under the intervention \code{abar}}
#' 	\item {The empirical influence function for the point estimate}
#' 	\item {The time-specific empirical influence functions}
#' 	\item {The conditional expectation fits for \code{Qbar}}
#' 	\item {The call to the function}
#' }
#' 
#' @export
aiptw = function(data, cum.g, Ynodes, Anodes, Cnodes=NULL, abar, Qform, SL.library=NULL, family="quasibinomial", stratify=TRUE) {

	## Initializes ##
	Q.kplus1 = data[,Ynodes[length(Ynodes)]]
	if(is.null(dim(cum.g))) {
		cum.g = matrix(cum.g, ncol=1)
	}
	IC.all = matrix(nrow=nrow(data), ncol=length(Ynodes), dimnames=list(NULL, Ynodes))	
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
		P_na.data = data
		for(a in 1:length(Anodes)) {
			P_na.data[,names(data)[Anodes][a]] = abar[a]
		}
		P_n = cbind(Q.kplus1, data[,all.vars(as.formula(Qform[i])[[3]])])
		P_na = cbind(Q.kplus1, P_na.data[,all.vars(as.formula(Qform[i])[[3]])])
		
		## Followed regime of interest ##		
		followed.abar = apply(data[,Anodes[1:i], drop=FALSE], 1, function(x) all(x==abar[1:i], na.rm=T))
		if(is.null(Cnodes)) {
			uncensored = rep(TRUE, nrow(data))
		} else {
			uncensored = apply(data[,Cnodes[1:(i*2)], drop=FALSE], 1, function(x) all(x=="uncensored", na.rm=T))
		}
		followed.abar = followed.abar * uncensored 
		
		## Fits Qbar ##
		if(stratify) {
			fitData = P_n[at.risk & followed.abar & uncensored,, drop=FALSE]
		} else {
			fitData = P_n[at.risk & uncensored,, drop=FALSE]
		}
		if(is.null(SL.library)) {
			Qfit.abar = glm(as.formula(Qform[i]), data=fitData, family=family)
		} else {
			tmp = complete.cases(P_n[,-1, drop=FALSE])
			if(family=="quasibinomial") family = "binomial"
			if(ncol(fitData[,-1, drop=FALSE])>0) {
				Qfit.abar = mcSuperLearner(Y=fitData$Q.kplus1, X=fitData[,all.vars(as.formula(paste(Qform[i]))[[3]]), drop=FALSE], newX=P_na[tmp,all.vars(as.formula(paste(Qform[i]))[[3]]), drop=FALSE], SL.library=SL.library, family=family, control = list(trimLogit=.001, saveFitLibrary=FALSE), cvControl=list(V=8), method="method.NNloglik.LT", verbose=TRUE)
			}
		}

		## Gets Q.k ##
		if(is.null(SL.library)) {
			Q.k = suppressWarnings(predict(Qfit.abar, newdata=P_na, type="response"))
		} else {
			Q.k = rep(NA, length(tmp))
			if(ncol(fitData[,-1, drop=FALSE])>0) {
				library.predict = Qfit.abar$library.predict
				library.predict[is.na(library.predict)] = 0
				Qfit.abar$SL.predict = library.predict %*% Qfit.abar$coef
				Q.k[tmp] = Qfit.abar$SL.predict
			} else {
				Q.k[tmp] = mean(fitData$Q.kplus1)
			}
		}
		if(family %in% c("quasibinomial", "binomial")) {
			Q.k[!at.risk] = 1
		}
		
		## IC and Updates Q.kplus1 ##
		IC.all[,i] = calcIC(Q.kplus1=Q.kplus1, Q.k=Q.k, h.g.ratio=1/cum.g[,i], uncensored=uncensored, intervention.match=followed.abar)
		Q.kplus1 = Q.k
		Qfits = c(Qfits, list(Qfit.abar))
		names(Qfits)[length(Qfits)] = names(data)[Ynodes[i]]
	}
	
	## Temp IC ##
	tmpIC = apply(IC.all, 1, sum) + Q.kplus1
	out = list(estimate=mean(tmpIC), IC=tmpIC-mean(tmpIC), IC.all=IC.all, Qfits=Qfits, call=match.call())
	class(out) = "aiptw"

	return(out)

}

print.aiptw = function(x, ...) {
	PrintCall(x$call)
	cat("AIPTW Estimate:\t", x$estimate, "\n")
	invisible(x)
}
