###############################################################################
# Description: Generates simulation data, based on defined distribution
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Apr 27, 2015
###############################################################################


#' @export 
generateData = function(n, time.pt, abar=NULL) {

	#Time ordering: W, Y(t), L(t), A(t) : W=(W1,W2) and L(t) = (L2(t),L1(t))
	#n.b. Within L(t) there is no implied time-ordering...i.e. either of L2(t) or L1(t) can go first
	rexpit  = function(x) rbinom(n=length(x), size=1, prob=x)
	QW1     = function(n) rnorm(n, mean=0, sd=1)
	QW2     = function(n) rep(plogis(-1), n)
	QW3     = function(n) rnorm(n, mean=0, sd=1)
	QY.t    = function(prev_y, w1, w2, w3, prev_l1, prev_l2, prev_a) ifelse(prev_y==1, 1, plogis(-1.9 + 1.2*w1 - 2.4*w2 - 1.8*prev_l1 - 1.6*prev_l2 + 1*prev_l1*prev_l2 - 1*prev_a))
	QL1.t   = function(y, w1, prev_l1, prev_l2, prev_a) ifelse(y==1, prev_l1, 0.1 + 0.4*w1 + 0.6*prev_l1 - 0.7*prev_l2 - 0.45*prev_a - rnorm(length(w1), sd=0.5))
	QL2.t   = function(y, w1, w2, prev_l1, prev_l2, prev_a) ifelse(y==1, prev_l2, -0.55 + 0.5*w1 + 0.75*w2 + 0.1*prev_l1 + 0.3*prev_l2 - 0.75*prev_a - rnorm(length(w1), sd=0.5))
	gA.t    = function(y, w1, w2, l1, l2, prev_a) ifelse(y==1, prev_a, ifelse(prev_a==1, 1, plogis(-1 - 1.5*w1 + 1.75*w2 + 1.2*l1 - 1.8*l2 + 0.8*l1*l2)))
	# nb. Distribution is set up such that: 
	#	Y(0)=0 for everyone, ie. Everyone is alive at the beginning of follow-up
	#	if Y(t)=1, then all remaining covariate last values get carried forward
	#	if A(t-1)=1 then A(t)=1 
	
	g.matrix = matrix(ncol=time.pt, nrow=n, dimnames=list(NULL, paste0("A.", 0:(time.pt-1))))
	
	## CHECKS ##
	if(time.pt==0) stop("time.pt has to be greater than 0")
	if(any(cummax(abar)!=abar)) stop("A is a counting process & cannot decrease")
	if(!is.null(abar) & length(abar) != time.pt) stop("abar has to be either NULL or length of time.pt")

	## INITIALIZATION ##
	o.names = NULL
	for(i in 0:time.pt){
		if(i<time.pt) {
			o.names = c(o.names, paste0(c("Y", "L1","L2", "A"), ".", i))
		} else {
			o.names = c(o.names, paste0(c("Y"), ".", i))
		}	
	}
	O = matrix(nrow=n, ncol=length(o.names)+3, dimnames=list(NULL, c("W1", "W2", "W3", o.names)))
	
	## OBSERVED VALUES ##
	O[,"W1"] = QW1(n)
	O[,"W2"] = rexpit(QW2(n))
	O[,"W3"] = QW3(n)
	for(i in 0:time.pt){
		#nb. "prev" values are set to 0 for t=0
		if(i==0) {
			#Y(t)
			O[,"Y.0"] = rep(0,n)
			#L1(t)
			O[,"L1.0"] = QL1.t(y=O[,"Y.0"], w1=O[,"W1"], prev_l1=rep(0, n), prev_l2=rep(0, n), prev_a=rep(0, n))
			#L2(t)
			O[,"L2.0"] = QL2.t(y=O[,"Y.0"], w1=O[,"W1"], w2=O[,"W2"], prev_l1=rep(0, n), prev_l2=rep(0, n), prev_a=rep(0, n))
			#A(t)
			if(is.null(abar)) {
				g.matrix[,"A.0"] = gA.t(y=O[,"Y.0"], w1=O[,"W1"], w2=O[,"W2"], l1=O[,"L1.0"], l2=O[,"L2.0"], prev_a=rep(0, n))
				O[,"A.0"] = rexpit(g.matrix[,"A.0"])
			} else {
				g.matrix[,"A.0"] = gA.t(y=O[,"Y.0"], w1=O[,"W1"], w2=O[,"W2"], l1=O[,"L1.0"], l2=O[,"L2.0"], prev_a=rep(0, n))
				O[,"A.0"] = rep(abar[i+1], n)
			}
		} else if (i<time.pt) {
			#Y(t)
			O[,paste0("Y.",i)] = rexpit(QY.t(prev_y=O[,paste0("Y.",i-1)], w1=O[,"W1"], w2=O[,"W2"], w3=O[,"W3"], prev_l1=O[,paste0("L1.",i-1)], prev_l2=O[,paste0("L2.",i-1)], prev_a=O[,paste0("A.",i-1)]))
			#L1(t)
			O[,paste0("L1.",i)] = QL1.t(y=O[,paste0("Y.",i)], w1=O[,"W1"], prev_l1=O[,paste0("L1.",i-1)], prev_l2=O[,paste0("L2.",i-1)], prev_a=O[,paste0("A.",i-1)])
			#L2(t)
			O[,paste0("L2.",i)] = QL2.t(y=O[,paste0("Y.",i)], w1=O[,"W1"], w2=O[,"W2"], prev_l1=O[,paste0("L1.",i-1)], prev_l2=O[,paste0("L2.",i-1)], prev_a=O[,paste0("A.",i-1)])
			#A(t)
			if(is.null(abar)) {
				g.matrix[,paste0("A.",i)] = gA.t(y=O[,paste0("Y.",i)], w1=O[,"W1"], w2=O[,"W2"], l1=O[,paste0("L1.",i)], l2=O[,paste0("L2.",i)], prev_a=O[,paste0("A.",i-1)])
				O[,paste0("A.",i)] = rexpit(g.matrix[,paste0("A.",i)])
			} else {
				g.matrix[,paste0("A.",i)] = gA.t(y=O[,paste0("Y.",i)], w1=O[,"W1"], w2=O[,"W2"], l1=O[,paste0("L1.",i)], l2=O[,paste0("L2.",i)], prev_a=O[,paste0("A.",i-1)])
				O[,paste0("A.",i)] = rep(abar[i+1], n)
				O[O[,paste0("Y.",i)]==1,paste0("A.",i)] = O[O[,paste0("Y.",i)]==1,paste0("A.",i-1)]
			}
		} else if (i==time.pt) {
			#Y(t)
			O[,paste0("Y.",i)] = rexpit(QY.t(prev_y=O[,paste0("Y.",i-1)], w1=O[,"W1"], w2=O[,"W2"], w3=O[,"W3"], prev_l1=O[,paste0("L1.",i-1)], prev_l2=O[,paste0("L2.",i-1)], prev_a=O[,paste0("A.",i-1)]))
		}
	}
	O = data.frame(O)
	O$Y.0 = NULL
	
	return(list(O=O, g.matrix=g.matrix))
	
}
