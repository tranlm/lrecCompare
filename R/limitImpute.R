###############################################################################
# Description: Carries last observation forward with truncation
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Apr 30, 2015
###############################################################################


limitImpute = function(var, apptdate, limit.days) {
	preg = 0; preg.date = NA
	new.var = rep(NA, length(var))
	for(i in 1:length(var)) {
		#Becomes pregnant
		if(!is.na(var[i]) & var[i]==1 & preg==0) {
			preg = 1
			preg.date = apptdate[i]
			new.var[i] = var[i]
			#Not pregnant
		} else if(!is.na(var[i]) & var[i]==0) {
			preg = 0
			preg.date = NA
			new.var[i] = var[i]
			#Missing
		} else if(is.na(var[i])) {
			if(preg==0) new.var[i] = 0
			if(preg==1) {
				if(as.numeric(apptdate[i] - preg.date) <= limit.days) {
					new.var[i] = 1
				} else new.var[i] = 0
			}
		} else new.var[i] = var[i]
	}
	return(new.var)
}

