##
 # FILE main.r (version alpha)
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
 ####################################################################
 # > REFERENCES
 # O. Crespo, J.É. Bergez, F. Garcia, P2 hierarchical decomposition procedure: application to irrigation strategies design, Operational Research: An International Journal (accepted on 31-03-2009)
 ####################################################################

## TO DO:
# > check ?file.path for windows path
# > pay attention to memory
# > reduce multicriteria comparison redondancy
# > make it work with th R GUI

xPos <- function(	mod,		## model to be simulated for evaluation: Deb test functions{1,2,3,4}, apsim{10}
			partNo,	## No of divided parts per region {2,3}
			decNo,	## decision number per region
			perNo,	## perturbation param number
			simLimit,	## simulation number limit
			timLimit,	## time limit in sec
		      seeItThrough=NULL,## for graphics {"g","d"}
			seed=NULL)	## if needed (integer)
{

##### CHECK INPUT PARAMETERS ########################################
source("exitFct.r");
checkInputs(mod,partNo,decNo,perNo,simLimit,timLimit,seeItThrough,seed=NULL);

## to add as inputs (and thus to check above !!)
evalMeth <- 5; # 1: reg mean, 2: reg min dec mean, 3: reg max dec mean, 5: multicriteria
#criterion <- 1; # for monocriterion evaluation

##### INTIALISATION #################################################
##### source needed files
source("simulate.r");
source("manageLists.r");
source("selection.r");
source("division.r");
source("plotFct.r");
source("sample.r");
source("evaluation.r");

##### random seed
if(is.null(seed)){	runif(1);
}else{			set.seed(seed);
}

##### initialize models
if(mod==1 || mod==2 || mod==3 || mod==4){
	# Deb mathematical functions
	decS <- matrix(c(0,1,0.05,0,1,0.05),3);
	apsimSpec <- NULL;	# no need in mathematical fct
	criNo <- 2;
	switch(mod,
		{	criS <- matrix(c(0,1,0,10),2);	# Deb functions 1
		},{	criS <- matrix(c(0,1,-0.5,4),2);	# Deb functions 2
		},{	criS <- matrix(c(0,4,0,4),2);		# Deb functions 3
		},{	criS <- matrix(c(0,4,0,4),2);		# Deb functions 4
	});
}
if(mod==10){
	# APSIM
	# GO AND MODIFY apsimInterface.r TO ADAPT AT WILL
	source("../apsimPlugIn/apsimInterface.r");
	source("../apsimPlugIn/rwfileOp.r");
	apsimSpec <- apsim_userSettings();
	decS <- apsimSpec$decS;
	criS <- apsimSpec$criS;
}

# decision space validity check
if (!is.decSpaceValid(decS)) {
	print(	"##########################################",quote=FALSE);
	print("decS (decisions space definition): matrix collecting min, max, step (lin) for each decisions (col)");
	print(	"##########################################",quote=FALSE);
	stop();
}
varNo <- dim(decS)[2];
criNo <- dim(criS)[2];

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
proList <- decSpaceAsOnlyRegInList(decS,varNo,perNo,criNo);
unbList <- list("itemNo"=0,"regEva"=NULL);
penList <- list("itemNo"=0,"regEva"=NULL);
besList <- list("itemNo"=0,"regEva"=NULL);
if (!is.null(seeItThrough) && (varNo==2 || criNo==2)){
	scrList <- init_visualisation(seeItThrough,decS,criS);
}
repeat{
	##### divide every of the promising regions (i.e. proList)
#print("   divide");
	proList <- divide_List(proList,partNo);

	##### sample every of the promising regions (i.e. proList)
#print("   sample");
	proList <- sample_List(proList,decNo,varNo,perNo,criNo,2);

	## messages
	print(paste("--  ",simNo,
			" (",format(difftime(Sys.time(),startingTime,units="auto"),digits=4),") ",
			" : ",
			proList$itemNo," + ",
			penList$itemNo," + ",
			unbList$itemNo,
			" : mem ", memory.size(),
		sep=""),quote=FALSE
	);

	## watch it run
	if (!is.null(seeItThrough) && (varNo==2 || criNo==2)){
		update_visualisation(seeItThrough,scrList,proList,penList,unbList,besList);
	}

#browser();
	##### simulate every of the promising regions (i.e. proList)
#print("   simulate");
	for (reg in 1:proList$itemNo){
print(paste("   ###   reg ",reg," in ",proList$itemNo,sep=""));
		simTime <- Sys.time();
		temp <- simulateModel(mod,apsimSpec,proList$regEva[[reg]],perNo,criNo);
		simulationTime <- simulationTime+difftime(Sys.time(),simTime);
		proList$regEva[[reg]] <- temp$eva;
		simNo <- simNo + temp$simNo;
	}

	##### MULTICRITERIA
	##### keep only the best decisions
	# probably faster to the target and/but recompute lots of decisions
	# might be good, but at what cost?
	# in selection.r
#print("   keepTheBest");
#	proList <- keepTheBests(proList,2);

	##### evaluate every of the promising regions (i.e. proList)
	# should be able to do it smootherly by removing one region and adding two,
	# instead of re-computing everything?
print("   evaluate");
	proList <- evaluate_proList(proList,evalMeth,criterion);

	##### current best (selection.r)
	# !! check that prolist$selCri before and after does not change
	if (!is.null(seeItThrough) && (varNo==2 || criNo==2)){
#print("   compute the best");
		besList <- update_bestList(proList,besList,evalMeth,criterion);
	}

	##### MULTICRITERIA
	## add proList (offspring) regions comparisons to penList regions
	if(evalMeth==5){
print("   evaluate proPLUSpen");
		temp <- evaluate_penPLUSproList(proList,penList,evalMeth);
		proList <- temp$pro;
		penList <- temp$pen;
	} # has to be after evaluation and before updatelists

	##### update lists
print("   update");
	temp <- mergeBreakable(penList,unbList,proList,varNo);
	penList <- temp$pen;
	unbList <- temp$unb;
	proList <- list("itemNo"=0,"regEva"=NULL);

	## partial storage
	save(decS,unbList,penList,proList,file=paste(apsimSpec$path2out,"partialLists.rData",sep=""));

#browser();
	## stopping criteria
	if(	Sys.time()>=endingTime	# time limit
	 	|| simNo>=simLimit	# simulation number limit
		|| penList$itemNo<1	# still exits pending regions
	){	## watch it run
		if (!is.null(seeItThrough) && (varNo==2 || criNo==2)){
			## messages
			print(paste("--  ",simNo,
				#" (",format(difftime(Sys.time(),startingTime),format="%S"),") ",
				" : ",
				proList$itemNo," + ",
				penList$itemNo," + ",
				unbList$itemNo,
				" : mem ",
				memory.size(),
			sep=""),quote=FALSE);
	
			update_visualisation(seeItThrough,scrList,proList,penList,unbList,besList);
		}
		break;
	}

	##### select equally potentially optimal regions (proList)
#print("   select");
	temp <- select(proList,penList,0);
	proList <- temp$pro;
	penList <- temp$pen;

	##### MULTICRITERIA
	## remove proList regions comparisons from penList regions
	if(evalMeth==5){
#print("   evaluate proMINUSpen");
		temp <- evaluate_penMINUSproList(proList,penList,evalMeth);
		proList <- temp$pro;
		penList <- temp$pen;
	} # has to be after selection and before division, if at the loop top, then initial case would fuck up
}
##### DEBUGGING OBSERVATIONS ########################################
##### EXIT ##########################################################
print("   optimisation is done - final update in process");

##### update unbreakable, then unb + last pending list evaluations
evaluate_proList(unbList,evalMeth,criterion);
if (penList$itemNo >0){	evaluate_penPLUSproList(unbList,penList,evalMeth);}

##### current best (selection.r)
# !! check that prolist$selCri before and after does not change
if (unbList$itemNo >0){ besList <- update_bestList(unbList,besList,evalMeth,criterion);}
if (penList$itemNo >0){	besList <- update_bestList(penList,besList,evalMeth,criterion);}
if (proList$itemNo >0){ besList <- update_bestList(proList,besList,evalMeth,criterion);}

resolutionTime <- difftime(Sys.time(),startingTime);
if (penList$itemNo==0){
	stoppedBecauseOf <- "empty pending list";
}else{
	if(simNo>=simLimit){
		stoppedBecauseOf <- "simulation amount over";
	}else{
		stoppedBecauseOf <- "time over";
}	}

# if does not exists before
if(all(is.na(criS))){
# criS (from besList only ... compare with unbList?)
	## find the 'criNo' bests
	criS <- array(NA,dim=c(2,criNo));
	criS[1,] <- apply(besList$regEva[[1]]$decEva[[1]],2,min);
	criS[2,] <- apply(besList$regEva[[1]]$decEva[[1]],2,max);
	for (r in 1:besList$itemNo){
		for (d in 1:besList$regEva[[r]]$itemNo){
			for (c in 1:criNo){
				if(min(besList$regEva[[r]]$decEva[[d]][,c])<criS[1,c]){
					criS[1,c] <- min(besList$regEva[[r]]$decEva[[d]][,c]);
				}
				if(max(besList$regEva[[r]]$decEva[[d]][,c])>criS[2,c]){
					criS[2,c] <- max(besList$regEva[[r]]$decEva[[d]][,c]);
				}
			}
		}
	}
}

##### write/store outputs
outFile <- paste(apsimSpec$path2out,"listsAchieved",format(Sys.time(),"_%d-%m-%Y_%H-%M-%S"),".rData",sep="");
save(decS,criS,unbList,penList,proList,besList,file=outFile);
file.remove(paste(apsimSpec$path2out,"partialLists.rData",sep=""));
if(!is.null(apsimSpec)){
	write.bestList(besList,apsimSpec,simNo,resolutionTime,(unbList$itemNo+penList$itemNo+proList$itemNo));
}

##### VISUAL ########################################################
if (!is.null(seeItThrough) && (varNo==2 || criNo==2)){
	last_visualisation(seeItThrough,scrList,proList,penList,unbList,besList);
}

print("",quote=FALSE);
print(	"##########################################",quote=FALSE);
print(paste("# process stopped: ", stoppedBecauseOf,sep=""),quote=FALSE);
print(paste("# resolution took: ",format(resolutionTime),sep=""),quote=FALSE);
print(paste("# including ",format(simulationTime)," for ",simNo," simu",sep=""),quote=FALSE);
print(	"##########################################",quote=FALSE);
print(paste("# - the initial decision space definition",sep=""),quote=FALSE);
print(paste("# - the list of unbreakable regions",sep=""),quote=FALSE);
print(paste("# - the list of pending regions",sep=""),quote=FALSE);
print(paste("# - the list of promising regions",sep=""),quote=FALSE);
print(paste("# - the list of best regions",sep=""),quote=FALSE);
print(paste("# have been stored in here :",sep=""),quote=FALSE);
print(paste("# ",outFile,sep=""),quote=FALSE);
#print(paste("# use the fct ShowListInDecSpace to visualise those lists",sep=""),quote=FALSE);
print(	"##########################################",quote=FALSE);
}
