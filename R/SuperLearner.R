###############################################################################
# Description: Add comment
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Aug 27, 2015
###############################################################################

#' @export
SL.svm.LT = function (Y, X, newX, family, type.reg = "eps-regression", type.class = "C-classification", nu = 0.5, gamma = 0.1, ...) {
	if (family$family == "binomial" & !all(Y %in% c(0,1))) {
		fit.svm = try(svm(y = Y, x = X, nu = nu, type = type.reg, fitted = FALSE, gamma=gamma), silent=TRUE)
		if(inherits(fit.svm, "try-error")) {
			pred = rep(mean(Y), nrow(newX))
			fit = list(object="Algorithm failed")
		} else {
			pred = bound(predict(fit.svm, newdata = newX), c(0,1))
			fit = list(object = fit.svm)
		}
	}
	else if (family$family == "binomial") {
		newY = as.factor(Y)
		fit.svm = try(svm(y = newY, x = X, nu = nu, type = type.class, fitted = FALSE, gamma=gamma, probability = TRUE), silent=TRUE)
		if(inherits(fit.svm, "try-error")) {
			pred = rep(mean(Y), nrow(newX))
			fit = list(object="Algorithm failed")
		} else {
			pred = try(attr(predict(fit.svm, newdata = newX, probability = TRUE), "prob")[, "1"])
			if(inherits(pred, "try-error")) {
				pred = rep(mean(Y), nrow(newX))
			}
			fit = list(object = fit.svm)
		}
	}
	out = list(pred = pred, fit = fit)
	class(out$fit) = c("SL.svm")
	return(out)
}

#' @export
SL.polymars.LT = function(Y, X, newX, family, obsWeights, cv=2, seed=1000, ...){
	if (family$family == "binomial" & !all(Y %in% c(0,1))) {
		fit.mars = try(polymars(Y, X, weights = obsWeights), silent=TRUE)
		if(inherits(fit.mars, "try-error")) {
			pred = rep(mean(Y), nrow(newX))
			fit = list(object="Algorithm failed")
		} else {
			pred = bound(predict(fit.mars, x = newX), c(0,1))
			fit = list(object = fit.mars)
		}
	}
	else if (family$family == "binomial") {
		newY = Y
		fit.mars = try(polyclass(newY, X, cv = cv, weight = obsWeights, seed=seed), silent=TRUE)
		if(inherits(fit.mars, "try-error")) {
			pred = rep(mean(Y), nrow(newX))
			fit = list(object="Algorithm failed")
		} else {
			pred = ppolyclass(cov = newX, fit = fit.mars)[, 2]
			fit = list(fit = fit.mars)
		}
	}
	out = list(pred = pred, fit = fit)
	class(out$fit) = c("SL.polymars")
	return(out)
}

#' @export
SL.nnet.LT = function (Y, X, newX, family, obsWeights, size = 2, maxit = 1000, ...) {
	if (family$family == "binomial" & !all(Y %in% c(0,1))) {
		fit.nnet = try(nnet(x = X, y = Y, size = size, trace = FALSE, maxit = maxit, linout = TRUE, weights = obsWeights), silent=TRUE)
		if(inherits(fit.nnet, "try-error")) {
			pred = rep(mean(Y), nrow(newX))
			fit = list(object="Algorithm failed")
		} else {
			pred = bound(predict(fit.nnet, newdata = newX, type = "raw"), c(0,1))
			fit = list(object = fit.nnet)
		}
	}
	else if (family$family == "binomial") {
		newY = Y
		fit.nnet = try(nnet(x = X, y = newY, size = size, trace = FALSE, maxit = maxit, linout = FALSE, weights = obsWeights), silent=TRUE)
		if(inherits(fit.nnet, "try-error")) {
			pred = rep(mean(Y), nrow(newX))
			fit = list(object="Algorithm failed")
		} else {
			pred = predict(fit.nnet, newdata = newX, type = "raw")
			fit = list(object = fit.nnet)
		}
	}
	out = list(pred = pred, fit = fit)
	class(out$fit) = c("SL.nnet")
	return(out)
}

#' @export
SL.lasso.LT = function(Y, X, newX, family, obsWeights, id, alpha = 1, nfolds = 4, nlambda = 100, useMin = TRUE, ...) {
	# X must be a matrix, should we use model.matrix or as.matrix
	if(!is.matrix(X)) {
		X = model.matrix(~ -1 + ., X)
		newX = model.matrix(~ -1 + ., newX)
	}
	# now use CV to find lambda
	Y.matrix = cbind(1-Y,Y)
	fitCV = try(cv.glmnet(x = X, y = Y.matrix, weights = obsWeights, lambda = NULL, type.measure = 'deviance', nfolds = nfolds, family = family$family, alpha = alpha, nlambda = nlambda), silent=TRUE)
	if(inherits(fitCV, "try-error")) {
		pred = rep(mean(Y), nrow(newX))
		fit = list(object="Algorithm failed")
	} else {
		# two options for lambda, fitCV$lambda.min and fitCV$lambda.1se
		pred = predict(fitCV$glmnet.fit, newx = newX, s = ifelse(useMin, fitCV$lambda.min, fitCV$lambda.1se), type = 'response')
		fit = list(object = fitCV, useMin = useMin)
	}
	class(fit) = 'SL.glmnet'
	out = list(pred = pred, fit = fit)
	return(out)
}

#' @export
SL.ridge.LT = function (Y, X, newX, family, obsWeights, id, alpha = 0, nfolds = 4, nlambda = 100, useMin = TRUE, ...) {
	# X must be a matrix, should we use model.matrix or as.matrix
	if(!is.matrix(X)) {
		X = model.matrix(~ -1 + ., X)
		newX = model.matrix(~ -1 + ., newX)
	}
	# now use CV to find lambda
	Y.matrix = cbind(1-Y,Y)
	fitCV <- try(cv.glmnet(x = X, y = Y.matrix, weights = obsWeights, lambda = NULL, type.measure = "deviance", nfolds = nfolds, family = family$family, alpha = alpha, nlambda = nlambda), silent=TRUE)
	if(inherits(fitCV, "try-error")) {
		pred = rep(mean(Y), nrow(newX))
		fit = list(object="Algorithm failed")
	} else {
		# two options for lambda, fitCV$lambda.min and fitCV$lambda.1se
		pred <- predict(fitCV$glmnet.fit, newx = newX, s = ifelse(useMin, fitCV$lambda.min, fitCV$lambda.1se), type = "response")
		fit <- list(object = fitCV, useMin = useMin)
	}
	class(fit) <- "SL.glmnet"
	out <- list(pred = pred, fit = fit)
	return(out)
}

#' @export
SL.glmnet.LT = function (Y, X, newX, family, obsWeights, id, alpha = 0.5, nfolds = 4, nlambda = 100, useMin = TRUE, ...) {
	# X must be a matrix, should we use model.matrix or as.matrix
	if(!is.matrix(X)) {
		X = model.matrix(~ -1 + ., X)
		newX = model.matrix(~ -1 + ., newX)
	}
	# now use CV to find lambda
	Y.matrix = cbind(1-Y,Y)
	fitCV <- try(cv.glmnet(x = X, y = Y.matrix, weights = obsWeights, lambda = NULL, type.measure = "deviance", nfolds = nfolds, family = family$family, alpha = alpha, nlambda = nlambda), silent=TRUE)
	if(inherits(fitCV, "try-error")) {
		pred = rep(mean(Y), nrow(newX))
		fit = list(object="Algorithm failed")
	} else {
		# two options for lambda, fitCV$lambda.min and fitCV$lambda.1se
		pred <- predict(fitCV$glmnet.fit, newx = newX, s = ifelse(useMin, fitCV$lambda.min, fitCV$lambda.1se), type = "response")
		fit <- list(object = fitCV, useMin = useMin)
	}
	class(fit) <- "SL.glmnet"
	out <- list(pred = pred, fit = fit)
	return(out)
}

