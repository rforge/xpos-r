##
 # FILE selection.r
 # AUTHOR olivier crespo
 # DATE july 2009 - july 2009, 9
 # promising region selection functions
 ####################################################################

##
 # SELECTION BASED ON 2x2 selCri
 ####################################################################
 # selCri[1,]: minimize col1, if two even minimize col2
 # if two even candidates
 # selCri[2,]: maximize col1, if two even maximize col2
 ####################################################################
select_fct <- function(penList)
{
	#this fct is not called if penList == 1
	# countdown to avoid to sort it when removing the promising region from the pending List
	indices <- array(penList$itemNo,dim=1);

	for (r in seq(penList$itemNo-1,1,-1)){
		index <- indices[1];
		if(penList$regEva[[r]]$selCri[1,1] < penList$regEva[[index]]$selCri[1,1]){
			index <- array(r,dim=1);
			break;
		}
		if(penList$regEva[[r]]$selCri[1,2] < penList$regEva[[index]]$selCri[1,2]){
			index <- array(r,dim=1);
			break;
		}
		if(penList$regEva[[r]]$selCri[2,1] > penList$regEva[[index]]$selCri[2,1]){
			index <- array(r,dim=1);
			break;
		}
		if(penList$regEva[[r]]$selCri[2,2] > penList$regEva[[index]]$selCri[2,2]){
			index <- array(r,dim=1);
			break;
		}
		# otherwise selCri[r] == selCri[index]
		indices <- rbind(indices,r);
	}
print(indices);
return(indices);
}

##
 # SELECTION
 ####################################################################
 # selection implies that the promising regions will be removed from the pending regions
 ####################################################################
select <- function(proList,penList)
{
	##### select promising regions
	# for now
	if (penList$itemNo<2){
		selectedReg <- array(1,dim=1);
	}else{
		selectedReg <- select_fct(penList);
	}

	##### add them to proList
	for (r in 1:dim(selectedReg)[1]){
		proList$itemNo <- proList$itemNo+1;
		proList$regEva <- c(proList$regEva,list(penList$regEva[[selectedReg[r]]]));
	}

	##### remove them from penList
	# should be sorted decreasing... if not
	# selectedReg <- sort(selectedReg,decreasing=TRUE);
	for (r in 1:1:dim(selectedReg)[1]){
		penList$regEva <- penList$regEva[-selectedReg[r]];
		penList$itemNo <- penList$itemNo-1;
	}

return(list("pro"=proList,"pen"=penList));
}