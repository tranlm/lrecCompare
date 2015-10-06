###############################################################################
# Description: Add comment
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Jun 17, 2015
###############################################################################


#' @export
method.NNloglik.LT = function() {
	out = list(
			require = NULL,
			computeCoef = function(Z, Y, libraryNames, verbose, obsWeights, control, ...) {
				# compute cvRisk
				cvRisk = apply(Z, 2, function(x) { -mean(obsWeights * (Y*log(x) + (1-Y)*log(1-x))) } )
				names(cvRisk) = libraryNames
				# compute coef
				.NNloglik = function(x, y, wt, start = rep(0, ncol(x)), method="nlminb") {
					# adapted from MASS pg 445 (takes logit transformation)
					loss = function(beta, X, y, w) {
						p = plogis(crossprod(t(X), beta))
						loss = -(2 * w * (y*log(p) + (1-y)*log(1-p)))
						return(sum(loss))
					}
					# nb. Gradient computed under assumption of logistic distribution 
					grad = function(beta, X, y, w) {
						eta = crossprod(t(X), beta)
						p = plogis(eta)
						grad = -2 * crossprod(w * dlogis(eta) * (y/p + -(1-y)/(1-p)), X)
						return(grad)
					}
					if(method=="nlminb") {
						fit = nlminb(start, loss, grad, X = x, y = y, w = wt, lower=0, control=list(iter.max=500, step.max=(1/length(start))))
					} else if(method=="optim") {
						fit = optim(start, loss, grad, X = x, y = y, w = wt, method = "L-BFGS-B", lower = 0, control=list(parscale= rep(1/length(start), length(start))))
					} else {
						stop("Invalid optimization method.")
					}
					invisible(fit)
				}
				trimLogit = function (x, trim = 0.001) {
					x[x < trim] = trim
					x[x > (1 - trim)] = (1 - trim)
					return(qlogis(x))
				}
				fit.nnloglik = .NNloglik(x = trimLogit(Z), y = Y, wt = obsWeights, method="nlminb")
				if(verbose) {
					message(paste("Non-Negative log-likelihood convergence: ", fit.nnloglik$convergence == 0))
				}
				initCoef = fit.nnloglik$par
				initCoef[is.na(initCoef)] = 0.0
				# normalize so sum(coef) = 1 if possible
				if(sum(initCoef) > 0) {
					coef = initCoef/sum(initCoef)
				} else {
					warning("All algorithms have zero weight", call. = FALSE)
					coef = initCoef
				}
				out = list(cvRisk = cvRisk, coef = coef)
				return(out)
			},
			computePred = function(predY, coef, control, ...) {
				out = crossprod(t(plogis(trimLogit(predY))), coef)
				return(out)
			}
	)
	invisible(out)
}

