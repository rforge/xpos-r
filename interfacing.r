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
decisionList <- apsim_initDecisions();

##
 # variable change
 ####################################################################
print("change variables values");

for (dec in 1:decisionList$itemNo){
	file.copy(paste(path2Templates,simTemplate,sep=""),paste(path2Outputs,decisionList$var_title[dec],".sim",sep=""),overwrite=TRUE);
	changeVar("var_title",decisionList$var_title[dec],paste(path2Outputs,decisionList$var_title[dec],".sim",sep=""),paste(path2Outputs,decisionList$var_title[dec],".sim",sep=""));
	changeVar("var_startDate",decisionList$var_startDate[dec],paste(path2Outputs,decisionList$var_title[dec],".sim",sep=""),paste(path2Outputs,decisionList$var_title[dec],".sim",sep=""));
	changeVar("var_endDate",decisionList$var_endDate[dec],paste(path2Outputs,decisionList$var_title[dec],".sim",sep=""),paste(path2Outputs,decisionList$var_title[dec],".sim",sep=""));
	changeVar("var_path2metFile",decisionList$var_path2metFile[dec],paste(path2Outputs,decisionList$var_title[dec],".sim",sep=""),paste(path2Outputs,decisionList$var_title[dec],".sim",sep=""));
}

##
 # system evaluation given the variables
 ####################################################################
setwd(path2Outputs);
for (dec in 1:decisionList$itemNo){
	print(paste("start evaluation (Apsim) for decision: ","decision test ",dec,sep=""));
	writeLines(shell(paste("Apsim ",decisionList$var_title[dec],".sim",sep=""), intern=TRUE, wait=TRUE),paste(decisionList$var_title[dec],".sum",sep=""),sep="\n");
	print(paste("end evaluation (Apsim) for decision: ","decision test ",dec,sep=""));
}
setwd(path2Origin);

##
 # read outputs
 ####################################################################

