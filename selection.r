##
 # FILE selection.r
 # AUTHOR olivier crespo
 # DATE july 2009 - july 2009, 9
 # promising region selection functions
 ####################################################################

##
 # SELECTION BASED ON 2x2 mulDom
 ####################################################################
 # mulDom[1,]: number of regions dominating reg
 # mulDom[2,]: number of regions dominated by reg 
 ####################################################################
select_fct <- function(penList)
{
	#this fct is not called if penList == 1
	indices <- array(penList$itemNo,dim=1);

	# countdown to avoid to sort it when removing the promising region from the pending List
	for (r in seq(penList$itemNo-1,1,-1)){
		if(sum(penList$regEva[[r]]$mulDom[1,]) < sum(penList$regEva[[indices]]$mulDom[1,])){	# reg_r is dominated by less reg than reg_indices is
			indices <- array(r,dim=1);
			break;
		}
		if(sum(penList$regEva[[r]]$mulDom[1,]) == sum(penList$regEva[[indices]]$mulDom[1,])){	# reg_r is dominated by as many reg as reg_indices is
			if(penList$regEva[[r]]$mulDom[1,1] < penList$regEva[[indices]]$mulDom[1,1]){		# reg_r is dominated definitely by less reg than reg_indices is
				indices <- array(r,dim=1);
				break;
			}
			if(penList$regEva[[r]]$mulDom[1,1] == penList$regEva[[indices]]$mulDom[1,1]){		# reg_r is dominated definitely by as many reg as reg_indices is
				if(sum(penList$regEva[[r]]$mulDom[2,]) > sum(penList$regEva[[indices]]$mulDom[2,])){	# reg_r is dominating more reg than reg_indices is
					indices <- array(r,dim=1);
					break;
				}
				if(sum(penList$regEva[[r]]$mulDom[2,]) == sum(penList$regEva[[indices]]$mulDom[2,])){	# reg_r is dominating as many reg than reg_indices is
					if(penList$regEva[[r]]$mulDom[2,1] > penList$regEva[[indices]]$mulDom[2,1]){		# reg_r is dominating definitely more reg than reg_indices is
						indices <- array(r,dim=1);
						break;
					}
					if(penList$regEva[[r]]$mulDom[2,1] == penList$regEva[[indices]]$mulDom[2,1]){		# reg_r is dominating definitely as many reg as reg_indices is
						# i.e. mulDom[r] == mulDom[indices]
						indices <- rbind(indices,r);
					}
				}
			}
		}
	}

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