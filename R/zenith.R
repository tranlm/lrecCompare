###############################################################################
# Description: Running maximum value
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Apr 30, 2015
###############################################################################


zenith = function(x){
	maxvalue = x
	if(length(x)>1){
		for(i in 2:length(x)){
			if(!is.na(maxvalue[i]) & !is.na(maxvalue[i-1]) & maxvalue[i-1]>maxvalue[i] | is.na(maxvalue[i])) maxvalue[i] = maxvalue[i-1]
		}
	}
	return(maxvalue)
}

