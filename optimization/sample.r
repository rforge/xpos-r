##
 # FILE sample.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
 # sampling functions
 ####################################################################

##
 # COMPUTE THE NO OF DEC PER REGION
 ####################################################################
 # according to the relative size of the region
 ####################################################################
compute_decNo <- function (regDef)
{
	varNo <- dim(regDef)[2];
	relSize <- 1;
	min <- 1;
	max <- 2;
	step <- 3;
	
	for (var in 1:varNo){
		if((regDef[max,var]-regDef[min,var])/regDef[step,var] >= 1){
			relSize <- relSize*(regDef[max,var]-regDef[min,var])/regDef[step,var];
		}
	}
	
	#density <- 100;
	#decNo <- round(rexp(1,density/relSize)+1);	
	density <- 0.1;
	decNo <- round(relSize^density)+1;

return(decNo);
}

##
 # UNIFORMELY COMPLETE THE DECnO DECISIONS IN EACH REGIONS OF THE LIST
 ####################################################################
 # samMeth = 0: (default) unif
 # samMeth = 1: first in the middle then unif
 # samMeth = 2: norm
 ####################################################################
sample_List <- function (offList, decNo, varNo, perNo, criNo, samMeth=0)
{
	computeDecNo <- FALSE;
	if(decNo==0) computeDecNo <- TRUE;

	for (reg in 1:offList$itemNo){
		
		if(computeDecNo){## decNo is computed according to the region size
			decNo <- compute_decNo(offList$regEva[[reg]]$regDef);
		}

		if(samMeth==1){
			## the first in the middle, which should not exist
			## because you sample only the 'just' divided regions
			## yet it might produce a supplementary decision, and what?
	
			# create an NA decision vector
			# if offList$regEva[[reg]]$decDef NULL : c(offList$regEva[[reg]]$decDef,list(create_naDecDef(varNo))) == list(create_naDecDef(varNo))
			offList$regEva[[reg]]$decDef <- c(offList$regEva[[reg]]$decDef,list(create_naDecDef(varNo)));
			
			# create an NA evaluation array
			offList$regEva[[reg]]$decEva <- c(offList$regEva[[reg]]$decEva,list(create_naDecEva(perNo,criNo)));
			
			# increase itemNo
			offList$regEva[[reg]]$itemNo <- offList$regEva[[reg]]$itemNo +1;
	
			# set the decision vector in THE MIDDLE OF THE REGION
				for (v in 1:varNo){
				offList$regEva[[reg]]$decDef[[offList$regEva[[reg]]$itemNo]][v] <- offList$regEva[[reg]]$regDef[1,v]+(offList$regEva[[reg]]$regDef[2,v]-offList$regEva[[reg]]$regDef[1,v])/2;
		}	}
		## the rest randomly
		while(offList$regEva[[reg]]$itemNo < decNo){
			# create an NA decision vector
			# if offList$regEva[[reg]]$decDef NULL : c(offList$regEva[[reg]]$decDef,list(create_naDecDef(varNo))) == list(create_naDecDef(varNo))
			offList$regEva[[reg]]$decDef <- c(offList$regEva[[reg]]$decDef,list(create_naDecDef(varNo)));
		
			# create an NA evaluation array
			offList$regEva[[reg]]$decEva <- c(offList$regEva[[reg]]$decEva,list(create_naDecEva(perNo,criNo)));
			
			# increase itemNo
			offList$regEva[[reg]]$itemNo <- offList$regEva[[reg]]$itemNo +1;
			
			# set the decision vector into the region boundaries
			for (v in 1:varNo){
				if(samMeth<2){	## uniform
					offList$regEva[[reg]]$decDef[[offList$regEva[[reg]]$itemNo]][v] <- runif(
						1,
						min=offList$regEva[[reg]]$regDef[1,v],
						max=offList$regEva[[reg]]$regDef[2,v]);
				}
				if(samMeth==2){	## normal
					repeat{
						offList$regEva[[reg]]$decDef[[offList$regEva[[reg]]$itemNo]][v] <- rnorm(
							1,
							offList$regEva[[reg]]$regDef[1,v]+(offList$regEva[[reg]]$regDef[2,v]-offList$regEva[[reg]]$regDef[1,v])/2,
							(offList$regEva[[reg]]$regDef[2,v]-offList$regEva[[reg]]$regDef[1,v])/4);
						if(	offList$regEva[[reg]]$decDef[[offList$regEva[[reg]]$itemNo]][v] > offList$regEva[[reg]]$regDef[1,v]
							&& offList$regEva[[reg]]$decDef[[offList$regEva[[reg]]$itemNo]][v] < offList$regEva[[reg]]$regDef[2,v]) break;
					}
				}
			}
		}
	}

return(offList);
}