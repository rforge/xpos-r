##
 # FILE interfacing.r
 # AUTHOR olivier crespo
 # DATE june 2009 - july 2009
 ####################################################################

##
 # hand set
 ####################################################################
path2Templates <- "../Templates/";
path2Outputs <- "../Outputs/";
simTemplate <- "jody-SorgMungCLAY-template.sim"
varNo <- 4; decNo <- 3;

##
 # initializations
 ####################################################################
print("initialisation");
source("apsimInterface.r");
source("rwfileOp.r");
path2Origin <- getwd();

## set decisions manually in apsim_init.r
#decisionList <- apsim_initDecisions();

## change varaibles in .sim
#apsim_changeVar(decisionList)

## run simulation for all .sim files
#apsim_simulate(decisionList)

##
 # read outputs
 ####################################################################

