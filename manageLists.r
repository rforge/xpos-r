##
 # FILE manageLists.r
 # AUTHOR olivier crespo
 # DATE july 2009 - july 2009, 9
 # managing list functions
 ####################################################################

##
 # CREATE OBJECTS
 ####################################################################
## create an NA decision location in the decision space:
create_naDecDef <- function(varNo)
{	return(array(NA,dim=c(varNo)));}

## create an NA decision evaluation location in the criteria space:
create_naDecEva <- function(perNo,criNo)
{	return(array(NA,dim=c(perNo,criNo)));}

## create an NA region boundarie definitions in the decision space:
create_naRegDef <- function(varNo)
{	return(array(NA,dim=c(3,varNo)));}

## create an NA region evaluation (including decNo decisions):
create_naRegEva <- function(varNo,decNo,perNo,criNo)
{
	decDef <- create_emptyDecDef(varNo);
	decEva <- create_emptyDecEva(perNo,criNo);
	regDef <- create_emptyRegDef(varNo);	
	selCri <- create_emptyselCri();
	regEva <- list("itemNo"=NA,"regDef"=regDef,"decDef"=list(decDef),"decEva"=list(decEva),"selCri"=selCri);
	for (d in 2:decNo){
		regEva <- list("itemNo"=regEva$itemNo,"regDef"=regEva$regDef,"decDef"=c(regEva$decDef,list(decDef)),"decEva"=c(regEva$decEva,list(decEva)),"selCri"=regEva$selCri);
	}
return(regEva);
}

## create an empty region evaluation (NULL elements):
create_emptyRegEva <- function()
{
	regEva <- list("itemNo"=0,"regDef"=NULL,"decDef"=NULL,"decEva"=NULL,"selCri"=NULL);

return(regEva);
}

##
 # PUT THE WHOLE DECISION SPACE AS THE ONLY REG IN A LIST
 ####################################################################
decSpaceAsOnlyRegInList <- function(decS,varNo,decNo,perNo,criNo)
{
	regEva <- create_emptyRegEva();
	
	# decision space boundaries are the whole decision space
	regEva$regDef <- decS;
	# multicriteria group dominance at origin:
	regEva$selCri <- array(0,dim=c(2,2));	

	# create the initial pending region List:
	temp <- list("itemNo"=1,"regEva"=list(regEva));

return(temp);
}

##
 # UPDATE PENDING LIST AND UNBREAKABLE LISTS
 ####################################################################
 # > INPUT:
 # previous pending list
 # previous unbreakable list
 # offspring list (called proList in the main)
 # > OUTPUT:
 # pending list and unbreakable list
 ####################################################################
mergeBreakable <- function(penList,unbList,offList,varNo)
{
	for (reg in 1:offList$itemNo){
		breakable <- FALSE;
		for (var in 1:varNo){
			if((offList$regEva[[reg]]$regDef[2,var]-offList$regEva[[reg]]$regDef[1,var]) > offList$regEva[[reg]]$regDef[3,var] ){
				breakable <- TRUE;
				break;
			}
		}
	
		if (breakable==TRUE){	# pending list is pending regions + breakable offspring regions
			penList$regEva <- c(penList$regEva,list(offList$regEva[[reg]]));
			penList$itemNo <- penList$itemNo+1;
		}else{	# unbreakable list is unbreakable regions + unbreakable offspring regions
			unbList$regEva <- c(unbList$regEva,list(offList$regEva[[reg]]));
			unbList$itemNo <- unbList$itemNo+1;
		}
	}

return(list("pen"=penList,"unb"=unbList));	
}