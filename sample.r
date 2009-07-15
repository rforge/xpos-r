##
 # FILE sample.r
 # AUTHOR olivier crespo
 # DATE july 2009 - july 2009, 10
 # sampling functions
 ####################################################################

##
 # UNIFORMELY COMPLETE THE DECnO DECISIONS IN EACH REGIONS OF THE LIST
 ####################################################################
sample_List <- function (offList, decNo, varNo, perNo, criNo)
{
	for (reg in 1:offList$itemNo){
		while(offList$regEva[[reg]]$itemNo < decNo){
			# create an NA decision vector
			# if offList$regEva[[reg]]$decDef NULL : c(offList$regEva[[reg]]$decDef,list(create_naDecDef(varNo))) == list(create_naDecDef(varNo))
			offList$regEva[[reg]]$decDef <- c(offList$regEva[[reg]]$decDef,list(create_naDecDef(varNo)));
			
			# create an NA evaluation array
			offList$regEva[[reg]]$decEva <- c(offList$regEva[[reg]]$decEva,list(create_naDecEva(perNo,criNo)));
			
			# incrase itemNo
			offList$regEva[[reg]]$itemNo <- offList$regEva[[reg]]$itemNo +1;
			
			# set the decision vector into the region boundaries
			for (v in 1:varNo){
				offList$regEva[[reg]]$decDef[[offList$regEva[[reg]]$itemNo]][v] <- runif(
					1,
					min=offList$regEva[[reg]]$regDef[1,v],
					max=offList$regEva[[reg]]$regDef[2,v]);
			}
		}
	}

return(offList);
}