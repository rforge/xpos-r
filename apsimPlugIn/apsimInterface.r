##
 # FILE apsimInterface.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
 ####################################################################
 # APSIM setings
 ####################################################################
 # user has to update accordingly to his/her wishes
 # - apsim_userSettings
 # - apsim_readOutputs
 ####################################################################

##
 # USER SPECIFIC CONDITIONS SETTINGS FOR APSIM
 # inputs data
 ####################################################################
apsim_userSettings <- function()
{
	#
	#	TEMPLATE .SIM FILE
	#

	####	PATH of the folder that includes the template .sim file
	# N.B. the folder is also used as writing/readind folder for apsim .sim, .out, .sum working files
	# WARNING: enable apsim to be launched from this folder (edit your OS PATH variable)
	# use UNIX separator ("/" instead of windows "\"), finish the path with a separator
	# ex:	path2workDir <- "../../ApsimOptimization/";
##################################
	path2workDir <- "../../ApsimOptimization/";
##################################


	####	NAME of the actual template .sim file
	# ex:	simTemplate <- "wet-peanut_dry-maize_rotation-template220909.sim"
##################################
	simTemplate <- "wet-peanut_dry-maize_rotation-template220909.sim"
##################################

	#
	#	WEATHER DATA .MET FILE
	#

	####	PATH of the folder that includes teh .met file
	# use "\\\\" separator (to be improved, but until then ...), finish the path with a separator
	# ex:	path2MetFiles <- "C:\\\\Desktop\\\\MetFiles\\\\";
##################################
	path2MetFiles <- "C:\\\\Documents and Settings\\\\CRE256\\\\Desktop\\\\Katherine\\\\Met\\\\";
##################################
	
	####	NAME of the actual .met file
	# optimization is performed for one location (related to apsim settings), so that the weather data set is not a variable
	# however the uncertainty of the process is simulated by using one or an other "random" weather of this data set
	# ex:	metFileName <- "Tindal.met";
##################################
	metFileName <- "Tindal.met";
##################################
	var_metFile <- paste(path2MetFiles,metFileName,sep="");

	####	minimal no of year to simulate to collect the evaluation of one year only
	# e.g. period == means that the process will simulate apsim for 1950->1955 to evaluate year 1955
	# ex:	period <- 2;
##################################
	period <- 2;
##################################

	####	definition of min and max year that you want to use within the .met file
	# WARNING: according to the "period" above, the .met file has to include "period"-1 year(s) prior to your min year
	perDef <- array(c(
##################################
	1950,		# min year that you want to use as randomness
	2005,		# max year that you want to use as randomness
##################################
	1		# 1: discreet for APSIM
	),dim=3); 

	#
	#	SET VARIABLE FOR THE ALL OPTIMIZATION PROCESS
	#
	
	####	clock module day/month
	# keep the year as "var_startYear" or "var_endYear" to be replaced automatically
	# ex:	var_startDate <-	"15/09/var_startYear";
##################################
	var_startDate <-	"15/09/var_startYear";
	var_endDate <-	"31/12/var_endYear";
##################################

	####	write the variables that will last for the whole optimization
	file.copy(paste(path2workDir,simTemplate,sep=""),paste(path2workDir,"initFile",".sim",sep=""),overwrite=TRUE);
	changeVar(	"var_title",	"optimization",	paste(path2workDir,"initFile",".sim",sep=""),paste(path2workDir,"initFile",".sim",sep=""));
	changeVar(	"var_startDate",	var_startDate,	paste(path2workDir,"initFile",".sim",sep=""),paste(path2workDir,"initFile",".sim",sep=""));
	changeVar(	"var_endDate",	var_endDate,	paste(path2workDir,"initFile",".sim",sep=""),paste(path2workDir,"initFile",".sim",sep=""));
	changeVar(	"var_metFile",	var_metFile,	paste(path2workDir,"initFile",".sim",sep=""),paste(path2workDir,"initFile",".sim",sep=""));

	#
	#	SET VARIABLE DECISIONS TO OPTIMIZE
	#

	####	how many decisions define the decision space
	# ex:	varNo <- 6;
##################################
	varNo <- 12;
##################################

	####	set variable names that will appear in the template .sim file in order to be substituted during the process
	# N.B. according to the varNo above, you have to have as many names as variables
	# ex: "var_decision1"
	decNam <- array(c(
##################################
	"var_peanutIrrAmount",	#1
	"var_maizeIrrAmount",	#2
	"var_maizeFerAtSow",	#3
	"var_maizeFerAt21",	#4
	"var_maizeFerAt28",	#5
	"var_maizeFerAt35",	#6
	"var_maizeFerAt42",	#7
	"var_maizeFerAt49",	#8
	"var_maizeFerAt56",	#9
	"var_maizeFerAt63",	#10
	"var_maizeFerAt70",	#11
	"var_maizeFerAt77"	#12
##################################
	),dim=c(1,varNo));

	####	set decision space, i.e. the exploration boundaries
	# for each decision (ranked exactely as above), define the min, max, step boundaries to be explored
	# you gess min, max, step is the smallest range achievable, set the "step" accordingly to applicabililty finess:
	# can you make a difference between a fertilization amount of 20 and 21 kg? no, the step has to be larger than 1 then.
	# can you make a difference between a fertilization amount of 20 and 30 kg? yes, the step has to be lower or equal to 10 then.
	# can you make a difference between a fertilization amount of 20 and 25 kg? ...
	# N.B. according to the varNo above, you have to have as many (min,max,step) as variables
	decS <- matrix(c(
##################################
	20,	60,	10,	# min, max, minimal step of dec 1
	20,	60,	10,	# min, max, minimal step of dec 2
	20,	30,	3,	# min, max, minimal step of dec 3
	10,	20,	3,	# min, max, minimal step of dec 4
	10,	20,	3,	# min, max, minimal step of dec 5
	20,	30,	3,	# min, max, minimal step of dec 6
	20,	30,	3,	# min, max, minimal step of dec 7
	30,	40,	3,	# min, max, minimal step of dec 8
	30,	40,	3,	# min, max, minimal step of dec 9
	30,	40,	3,	# min, max, minimal step of dec 10
	20,	30,	3,	# min, max, minimal step of dec 11
	20,	30,	3	# min, max, minimal step of dec 12
##################################
	),3);

	####	check decS vs. varNo coherence
	if(dim(decS)[2]!=varNo){
		print("",quote=FALSE);
		print(	"##########################################",quote=FALSE);
		print(	"# incoherence in between decNo and decS",quote=FALSE);
		print(	"##########################################",quote=FALSE);
		stop();	
	}	

	#
	#	SET CRITERIA NO
	#

	####	how many criteria will be optimized
	# set here only the no of criteria,
	# the criteria are defined in the "apsim_readOutputs" function
	# ex:	criNo <- 7;
##################################
	criNo <- 7;
##################################
	criS <- array(NA,dim=c(1,criNo));
	# only if criNo == 2 (graphics purposes)
	if (criNo==2){
		criS <- array(c(	-1000,	-10000,	# min, max cri 1
					-4000,	-20000	# min, max cri 2
					),dim=c(1,2));
	}
	
	####	check criS vs. criNo coherence
	if(dim(criS)[2]!=criNo){
		print("",quote=FALSE);
		print(	"##########################################",quote=FALSE);
		print(	"# incoherence in between criNo and criS",quote=FALSE);
		print(	"##########################################",quote=FALSE);
		stop();	
	}

return(list("decNam"=decNam,"decS"=decS,"criS"=criS,"path2out"=path2workDir,"perDef"=perDef,"period"=period));
}

##
 # USER SPECIFIC CONDITIONS SETTINGS FOR APSIM
 # outputs data
 ####################################################################
 # how to read properly outputs from .out file
 # !! remember objectives are minimized
 ####################################################################
apsim_readOutputs <- function(path2Outputs, fileName, criNo)
{
	#
	#	THIS FUNCTION HAS TO BE ADAPTED TO YOUR CONFIGURATION
	#
	#	current version is made for:
	#	- rotation of peanut/maize
	#	- report frequency at harvest
	#

	####	column definition
	# mainly in order to ease the reading of the code
	# tip: take the .sim file and follow the variable list
##################################
	col_year <- 1;
	col_ddmmyy <- 2;
	# crop type : maize
	col_maizeBiomass <- 4;
	col_maizeYield <- 5;
	# crop type : peanut
	col_peanutBiomass <- 7;
	col_peanutYield <- 8;
	col_cumDrainage <- 9;
	col_cumRunoff <- 10;
	col_cumDenit <- 11;
	col_cumNLeached <- 12;
	col_cumNRunoff <- 13;
	col_irrigTot <- 14;
	col_fertiliser <- 15;
	col_rain <- 16;
##################################
	
	####	which ones are the criteria to optimize (both maximize and minimize)
	criteria <- array(c(
##################################
	col_peanutYield,		## crit 1
	col_maizeYield,		## crit 2
	col_cumDenit,		## crit 3
	col_cumNLeached,		## crit 4
	col_cumNRunoff,		## crit 5
	col_cumDrainage,		## crit 6
	col_cumRunoff		## crit 7
##################################
	),dim=criNo);

	#### read the .out apsim file
	temp <- read.table(paste(path2Outputs,fileName,".out",sep=""),skip=4,sep=",");
	colNo <- dim(temp)[2];

	#### make 2 lines (maize + peanut) one
	# this is required for rotations (of 2 crops)
	lin_temp <- 1; lin_res <- 1;	res1 <- temp[lin_temp,];
	lin_temp <- lin_temp +1;	res2 <- temp[lin_temp,];
	response <- array(NA,dim=c(1,colNo));
	for(col in 1:colNo){
		if (col==2) next;
		if(col==1 || col==3 || col==6){
			if(res1[1,col]!=res2[1,col]){
				print("apsim_readOutputs incorrect settings (\"apsimInterface.r\")");	stop();
			}; next;
		}; response[1,col] <- res1[1,col]+res2[1,col];
	}
	repeat{
		lin_temp <- lin_temp +1;	if (lin_temp>dim(temp)[1]) break;
		lin_res <- lin_res +1;		res1 <- temp[lin_temp,];
		lin_temp <- lin_temp +1;	res2 <- temp[lin_temp,];
		res <- array(NA,dim=c(1,colNo));
		for(col in 1:colNo){
			if (col==2) next;
			if(col==1 || col==3 || col==6){
				if(res1[1,col]!=res2[1,col]){
					print("apsim_readOutputs incorrect settings (\"apsimInterface.r\")");	stop();
				}; next;
			}; res[1,col] <- res1[1,col]+res2[1,col];
		}; response <- rbind(response,res);
	}

	####	is optimization a maximization or minimization?
	# every value is going to be minimize, so you need to negate the one you want to maximize
	for (col in c(
##################################
	col_maizeBiomass,		# to maximize
	col_maizeYield,		# to maximize
	col_peanutBiomass,	# to maximize
	col_peanutYield		# to maximize
##################################
		)){	response[,col] <- -response[,col];
	}

	####	only the last year is extracted
	criteria_vect <- array(NA,dim=c(1,criNo));
	for (c in 1:criNo){
		criteria_vect[1,c] <- response[dim(response)[1],criteria[c]]
	}

return(criteria_vect);
}

######################################################################
 # IT MIGHT BE INTERESTING TO HAVE A LOOK FURTHER DOWN
 # BUT YOU DO NOT HAVE TO MODIFY BELOW HERE
 #####################################################################

##
 # system evaluation given the variables
 ####################################################################
apsim_simulate <- function(path2Outputs,simFileName)
{
	path2Origin <- getwd();
	setwd(path2Outputs);

#	print("start apsim evaluation");
	writeLines(shell(paste("Apsim ",simFileName,".sim",sep=""), intern=TRUE, wait=TRUE, mustWork=TRUE),paste(simFileName,".sum",sep=""),sep="\n");
#	print("end apsim evaluation");

	setwd(path2Origin);
}

##
 # SIMULATE APSIM FROM REGION LIST
 ####################################################################
simulateApsim <- function(apsimSpec,dec,per,newDec,criNo)
{
	decNam <- apsimSpec$decNam;
	path2apsimOutputs <- apsimSpec$path2out;
	perDef <- apsimSpec$perDef;
	period <- apsimSpec$period;
	varNo <- dim(dec);

	# per in [0,1] has to be translate into a year
	year <- perDef[1]+per*(perDef[2]-perDef[1]);
	if (perDef[3]==1){
		year <- round(year);
	}

	# to reduce rw operations
	## change decision variables in .sim
	if(newDec==1){
		file.copy(paste(path2apsimOutputs,"initFile.sim",sep=""),paste(path2apsimOutputs,"noYearFile.sim",sep=""),overwrite=TRUE);
		for (v in 1:varNo){
			changeVar(decNam[1,v],dec[v],paste(path2apsimOutputs,"noYearFile.sim",sep=""),paste(path2apsimOutputs,"noYearFile.sim",sep=""));
		}
	}

	## change pertubation variables in .sim
	# i.e. add the year ('period' years simulated for 1 year output)
	# startYear and endYear define the length of the time period simulated (has to be included in the met file)
	file.copy(paste(path2apsimOutputs,"noYearFile.sim",sep=""),paste(path2apsimOutputs,"fileToSimulate.sim",sep=""),overwrite=TRUE);
	changeVar(	"var_startYear",	year-period,	paste(path2apsimOutputs,"fileToSimulate.sim",sep=""),paste(path2apsimOutputs,"fileToSimulate.sim",sep=""));
	changeVar(	"var_endYear",	year,			paste(path2apsimOutputs,"fileToSimulate.sim",sep=""),paste(path2apsimOutputs,"fileToSimulate.sim",sep=""));

	## run simulation
	apsim_simulate(path2apsimOutputs,"fileToSimulate");
	
	## read outputs from .out files
	# take out only the last year results
return(apsim_readOutputs(path2apsimOutputs,"optimization",criNo));
}
