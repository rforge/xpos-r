##
 # FILE selection.r
 # AUTHOR olivier crespo
 # DATE july 2009 - july 2009, 9
 # promising region selection functions
 ####################################################################

##
 # SELECTION PROCESS BASED ON 2x2 selCri
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
			indices <- array(r,dim=1);
		}else{
			if(penList$regEva[[r]]$selCri[1,1] == penList$regEva[[index]]$selCri[1,1]
			&& penList$regEva[[r]]$selCri[1,2] < penList$regEva[[index]]$selCri[1,2]){
				indices <- array(r,dim=1);
			}else{
				if(penList$regEva[[r]]$selCri[1,1] == penList$regEva[[index]]$selCri[1,1]
				&& penList$regEva[[r]]$selCri[1,2] == penList$regEva[[index]]$selCri[1,2]
				&& penList$regEva[[r]]$selCri[2,1] > penList$regEva[[index]]$selCri[2,1]){
					indices <- array(r,dim=1);
				}else{
					if(penList$regEva[[r]]$selCri[1,1] == penList$regEva[[index]]$selCri[1,1]
					&& penList$regEva[[r]]$selCri[1,2] == penList$regEva[[index]]$selCri[1,2]
					&& penList$regEva[[r]]$selCri[2,1] == penList$regEva[[index]]$selCri[2,1]
					&& penList$regEva[[r]]$selCri[2,2] > penList$regEva[[index]]$selCri[2,2]){
						indices <- array(r,dim=1);
					}else{
						if(penList$regEva[[r]]$selCri[1,1] == penList$regEva[[index]]$selCri[1,1]
						&& penList$regEva[[r]]$selCri[1,2] == penList$regEva[[index]]$selCri[1,2]
						&& penList$regEva[[r]]$selCri[2,1] == penList$regEva[[index]]$selCri[2,1]
						&& penList$regEva[[r]]$selCri[2,2] == penList$regEva[[index]]$selCri[2,2]){
							indices <- rbind(indices,r);
						}
					}
				}
			}
		}
	}

return(indices);
}

##
 # UPDATE LISTS ACCORDINGLY TO SELECTED REGIONS
 ####################################################################
 # selection implies that the promising regions will be removed from the pending regions
 ####################################################################
select <- function(proList,penList)
{
	##### select promising regions
	if (penList$itemNo<2){
		selectedReg <- array(1,dim=1);
	}else{
		selectedReg <- select_fct(penList);
	}

	##### add them to proList
	for (r in 1:dim(selectedReg)[1]){
		proList$itemNo <- proList$itemNo +1;
		proList$regEva <- c(proList$regEva,list(penList$regEva[[selectedReg[r]]]));
	}

	##### remove them from penList
	# should be sorted decreasing... if not
	# selectedReg <- sort(selectedReg,decreasing=TRUE);
	for (r in 1:dim(selectedReg)[1]){
		penList$regEva <- penList$regEva[-selectedReg[r]];
		penList$itemNo <- penList$itemNo -1;
	}

return(list("pro"=proList,"pen"=penList));
}


##
 # UPDATE THE LIST OF CURRENT BEST REGIONS
 ####################################################################
update_bestList <- function(proList,besList,evalMeth,criterion)
{
	# probably useless... but
	if(proList$itemNo==0){
		return(besList);
	}
##
# BEST LEAF ONLY OTHERWISE YOU'LL NEVER GET BETTER THAN THE FIRST BEST
# TEST IF OFFSPRING ARE INCLUDED WITHIN THE CURRENT BEST
##
	# remove from besList the parent region of any region in proList
#	if(){
#
#	}

	for (reg in 1:proList$itemNo){
		besList$itemNo <- besList$itemNo +1;
		besList$regEva <- c(besList$regEva,list(proList$regEva[[reg]]));
	}
	
	if (besList$itemNo<2){
	}else{
		## update multicriteria evaluation of besList
		#
		#	MAKE SURE PROLIST IS NOT DISTURBED IN MAIN.r !!!
		#
		if (evalMeth==5){	# otherwise it is still up to date
			besList <- evaluate_proList(besList,evalMeth,criterion);
		}
		
		## select a region as best regarding to reg$selCri[1,1] only
		# countdown useless here, but not necessary to change
		indices <- array(besList$itemNo,dim=1);
print(paste("avant.. ",indices,sep=""));
print(besList$regEva[[besList$itemNo]]$selCri[1,1]);
		for (r in seq(besList$itemNo-1,1,-1)){
			index <- indices[1];
print(besList$regEva[[r]]$selCri[1,1]);
			if(besList$regEva[[r]]$selCri[1,1] < besList$regEva[[index]]$selCri[1,1]){
				indices <- array(r,dim=1);
			}else{
				if(besList$regEva[[r]]$selCri[1,1] == besList$regEva[[index]]$selCri[1,1]){
					indices <- rbind(indices,r);
				}
			}
		}
print(paste("after.. ",indices,sep=""));
		
		## keep only the best in the list
		for (r in seq(besList$itemNo,1,-1)){
			if(length(indices[indices==r])>0){
			}else{
				besList$regEva <- besList$regEva[-r];
				besList$itemNo <- besList$itemNo -1;
			}
		}
	}

return(besList);
}