###############################################################################
# Description: Running minimum value
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Apr 30, 2015
###############################################################################


nadir = function(x){
	minvalue = x
	if(length(x)>1){
		for(i in 2:length(x)){
			if(!is.na(minvalue[i]) & !is.na(minvalue[i-1]) & minvalue[i-1]<minvalue[i] | is.na(minvalue[i])) minvalue[i] = minvalue[i-1]
		}
	}
	return(minvalue)
}

