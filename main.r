##
 # FILE main.r (version alpha)
 # AUTHOR olivier crespo
 # DATE july 2009 - july 2009, 13
 ####################################################################
 # > REFERENCES
 # O. Crespo, J.É. Bergez, F. Garcia, P2 hierarchical decomposition procedure: application to irrigation strategies design, Operational Research: An International Journal (accepted on 31-03-2009)
 # D.R. Jones, C.D. Perttunen, and B.E. Stuckman, Lipschitzian Optimization Without the Lipschitz Constant, Journal of Optimization Theory and Application, vol. 79(1), pages 157-181, 1993
 ####################################################################
 # > INPUT:
 # > FUNCTION CALLS:
 # > OUTPUT:
 ####################################################################

## TO DO:
# > check ?file.path for windows path
# > pay attention to memory
# > reduce multicriteria comparison redondancy

xPos <- function(	mod,		## model to be simulated for evaluation
			decS,		## decision space definition		
			decNo,	## decision number per region
			perNo,	## perturbation param number
			simLimit,	## simulation number limit
			timLimit,	## time limit in sec
		      seeItThrough=NULL,## for graphics {"g","d"}
			seed=NULL)	## if needed (integer)
{

## to add as inputs
criS <- matrix(c(0,1,0,10),2);
evalMeth <- 5; # 1: reg mean, 2: reg min dec mean, 3: reg max dec mean, 5: multicriteria

##### CHECK INPUT PARAMETERS ########################################
source("exitFct.r");
checkInputs(mod,decS,decNo,perNo,simLimit,timLimit,seeItThrough="n",seed=NULL);

varNo <- dim(decS)[2];
switch(mod,
	criNo <- 2,
	criNo <- 2,
	criNo <- 2,
	criNo <- 2
);

##### INTIALISATION #################################################
##### source needed files
# for all
source("simulate.r");
source("manageLists.r");
source("selection.r");
source("division.r");
source("plotFct.r");
source("sample.r");
source("evaluation.r");
# for model simulation
source("apsimInterface.r");

##### random seed
if(is.null(seed)){	runif(1);
}else{			set.seed(seed);
}

##### MAIN LOOP #####################################################
# > MAIN OBJECTS DATA FORMAT
# decision location in the decision space:
#	decDef <- array(,dim=varNo)
# decision evaluations location in the criteria space:
#	decEva <- array(,dim=c(perNo,criNo))
# region boundaries in DS:
# 	regDef <- array(,dim=c(3,varNo))
# region evaluation:
#	regEva <- list(itemNo,regDef,list(decDef),list(decEva),selCri)
# list of pending regions:
#	penList <- list(itemNo,list(regEva))
# list of promising region (potentially optimal regions):
# 	proList <- list(itemNo,list(regEva))
# list of the unbreakable regions (according to the minimal step)
# 	unbList <- list(itemNo,list(regEva))
#####################################################################
# penList U proList U unbList
# is the exhaustive ensemble of the decomposition tree leaves
#####################################################################
simNo <- 0;
startingTime <- Sys.time();
endingTime <- startingTime + timLimit;
simulationTime <- 0;
proList <- decSpaceAsOnlyRegInList(decS,varNo,decNo,perNo,criNo);
unbList <- list("itemNo"=0,"regEva"=NULL);
penList <- list("itemNo"=0,"regEva"=NULL);
if (!is.null(seeItThrough)){
	scrList <- init_visualisation(seeItThrough,decS,criS);
}
repeat{
	##### divide every of the promising regions (i.e. proList)
	proList <- divide_List(proList);

	##### sample
	proList <- sample_List(proList,decNo,varNo,perNo,criNo);

	##### simulate	
	for (reg in 1:proList$itemNo){
		simTime <- Sys.time();
		black <- simulateMathModel(mod,proList$regEva[[reg]],perNo,criNo);
		simulationTime <- simulationTime+difftime(Sys.time(),simTime);
		proList$regEva[[reg]] <- black$eva;
		simNo <- simNo + black$simNo;
	}

	##### update lists
	temp <- mergeBreakable(penList,unbList,proList,varNo);
	penList <- temp$pen;
	unbList <- temp$unb;
	proList <- list("itemNo"=0,"regEva"=NULL);

	##### evaluate
	# should be able to do it smootherly by removing one region and adding two,
	# instead of re-computing everything?
	penList <- evaluate(penList,evalMeth,simNo);

	## stopping criteria
	if(	Sys.time()>=endingTime	# time limit
	 	|| simNo>=simLimit	# simulation number limit
		|| penList$itemNo<1	# still exits pending regions
	) {break;}

	##### select equally potentially optimal regions (proList)
	temp <- select(proList,penList);
	proList <- temp$pro;
	penList <- temp$pen;

	## watch it run
	if (!is.null(seeItThrough)){
		## messages
		print(paste("--  ",simNo,
			#" (",format(difftime(Sys.time(),startingTime),format="%S"),") ",
			" : ",
			proList$itemNo," + ",
			penList$itemNo," + ",
			unbList$itemNo,
			" : ",
			memory.size(),
		sep=""),quote=FALSE);

		update_visualisation(seeItThrough,scrList,proList,penList,unbList);
	}
}
# update unbreakable list evaluations

##### DEBUGGING OBSERVATIONS ########################################

##### EXIT ##########################################################
resolutionTime <- difftime(Sys.time(),startingTime);
stoppedBecauseOf <- "empty pending list";
if(Sys.time()>=endingTime){stoppedBecauseOf <- "time over";}
if(simNo>=simLimit){stoppedBecauseOf <- "simulation amount over";}

##### VISUAL ########################################################
if (!is.null(seeItThrough)){
	## messages
	print(paste("--  ",simNo,
		#" (",format(difftime(Sys.time(),startingTime),format="%S"),") ",
		" : ",
		proList$itemNo," + ",
		penList$itemNo," + ",
		unbList$itemNo,
		" : ",
		memory.size(),
	sep=""),quote=FALSE);

	last_visualisation(seeItThrough,scrList,proList,penList,unbList);
}

print("",quote=FALSE);
print(	"##########################################",quote=FALSE);
print(paste("# process stopped: ", stoppedBecauseOf,sep=""),quote=FALSE);
print(paste("# resolution took: ",format(resolutionTime,format="%S"),sep=""),quote=FALSE);
print(paste("# including ",format(simulationTime,format="%S")," for ",simNo," simu",sep=""),quote=FALSE);
print(	"##########################################",quote=FALSE);

}
