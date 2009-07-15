##
 # FILE diivision.r
 # AUTHOR olivier crespo
 # DATE july 2009 - july 2009, 9
 # divising functions
 ####################################################################

##
 # DETERMINE THE LARGEST (RELATIVELY TO STEP) DECISION PARAMETER
 ####################################################################
largestDecVar <- function (regDef)
{
	min <- 1;
	max <- 2;
	step <- 3;
	varNo <- dim(regDef)[2];
	largest <- 1;
	
	for (var in 1:varNo){
		if((regDef[max,var]-regDef[min,var])/regDef[step,var] >  (regDef[max,largest]-regDef[min,largest])/regDef[step,largest]){
			largest <- var;	
		}
	}
	
return(largest);
}

##
 # CUT ONE DECISION SPACE INTO 2 PARTS ALONG PARAMETER 'VAR'
 ####################################################################
 # cut following a normal distribution in between min and max of parameter 'var'
 ####################################################################
cutDecSpace  <- function(regDef,var)
{
	twinRegDef=list("left"=regDef,"right"=regDef);
	
	if( (regDef[2,var]-regDef[1,var])<= 2*regDef[3,var] ){
			pivot <- 0.5;	# if width is not greater than twice the minimal step, cut in the middle
	}else{
		pivot <- rnorm(1,mean=0.5,sd=0.1);
		# check borders
		if(pivot < regDef[3,var]/(regDef[2,var]-regDef[1,var])){	pivot <- regDef[3,var]/(regDef[2,var]-regDef[1,var]);}
		if(pivot > 1-regDef[3,var]/(regDef[2,var]-regDef[1,var])){	pivot <- 1-regDef[3,var]/(regDef[2,var]-regDef[1,var]);}
	}

	pivot <- regDef[1,var]+pivot*(regDef[2,var]-regDef[1,var]);
	twinRegDef$left[2,var] <- pivot;
	twinRegDef$right[1,var] <- pivot;

return(twinRegDef);
}

##
 # SELECTION BASED ON 2x2 mulDom
 ####################################################################
divide_List <- function(proList)
{
	offList <- list("itemNo"=0,"regEva"=NULL);

	for (reg in 1:proList$itemNo){
		# select the largest side
		var <- largestDecVar(proList$regEva[[reg]]$regDef);
	
		# cut region definition into 2 parts
		twinRegDef <- cutDecSpace(proList$regEva[[reg]]$regDef,var);

		# create the 2 new empty regions
		regLeft <- create_emptyRegEva();
		regLeft$regDef <- twinRegDef$left;
		regRight <- create_emptyRegEva();
		regRight$regDef <- twinRegDef$right;

		# allocate previous decision samples
		# if one right on the frontier, it belongs to none of them (others will be simulated during evaluation)
		if (proList$regEva[[reg]]$itemNo > 0){
		for (d in 1:proList$regEva[[reg]]$itemNo){
			if(proList$regEva[[reg]]$decDef[[d]][var] < regLeft$regDef[2,var]){
				regLeft$decDef <- c(regLeft$decDef,list(proList$regEva[[reg]]$decDef[[d]]));
				regLeft$decEva <- c(regLeft$decEva,list(proList$regEva[[reg]]$decEva[[d]]));
				regLeft$itemNo <- regLeft$itemNo +1;
			}
			if(proList$regEva[[reg]]$decDef[[d]][var] > regRight$regDef[1,var]){
				regRight$decDef <- c(regRight$decDef,list(proList$regEva[[reg]]$decDef[[d]]));
				regRight$decEva <- c(regRight$decEva,list(proList$regEva[[reg]]$decEva[[d]]));
				regRight$itemNo <- regRight$itemNo +1;
			}
		}}
		
		# add them to offList (breakable test will caome after evaluation)
		offList$itemNo <- offList$itemNo +2;
		offList$regEva <- c(offList$regEva,list(regLeft,regRight));
	}

rm(proList);
return(offList);
}