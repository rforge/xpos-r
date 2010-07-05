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
	#	RUNNING MACHINE
	#
	####	To make the simulation run quicker on multiplle processor PCs
	# specify the number of processors you're allowed to use
	# until now: multi processors are used only for perturbation simulation of the same decision
##################################
	proNo <- 2;
##################################

	####	PATH of the folder used as writing/readind folder for apsim .sim, .out, .sum working files
	# WARNING: enable apsim to be launched from this folder (edit your OS PATH variable)
	# use UNIX separator ("/" instead of windows "\"), finish the path with a separator
	# ex:	path2workDir <- "../../ApsimOptimization/";
##################################
	path2workingDir <- "../../ApsimOptInBergRiver/WorkingDir/";
##################################

	#
	#	TEMPLATE .SIM FILE
	#

	####	PATH of the folder that includes the template .sim file
	# N.B. the folder is also used as writing/readind folder for apsim .sim, .out, .sum working files
	# WARNING: enable apsim to be launched from this folder (edit your OS PATH variable)
	# use UNIX separator ("/" instead of windows "\"), finish the path with a separator
	# ex:	path2workDir <- "../../ApsimOptimization/";
##################################
	path2template <- "../../ApsimOptInBergRiver/Templates/";
##################################


	####	NAME of the actual template .sim file
	# ex:	simTemplate <- "wet-peanut_dry-maize_rotation-template220909.sim"
##################################
	simTemplate <- "wheatAtStephenFarm_base_020710.sim"
##################################

	#
	#	WEATHER DATA .MET FILE
	#

	####	PATH of the folder that includes teh .met file
	# use "\\\\" separator (to be improved, but until then ...), finish the path with a separator
	# ex:	path2MetFiles <- "C:\\\\Desktop\\\\MetFiles\\\\";
##################################
	path2MetFiles <- "../../ApsimOptInBergRiver/Met/";
##################################
	
	####	NAME of the actual .met file
	# optimization is performed for one location (related to apsim settings), so that the weather data set is not a variable
	# however the uncertainty of the process is simulated by using one or an other "random" weather of this data set
	# ex:	metFileName <- "stephenFarmNCEP.met";
##################################
	metFileName <- "stephenFarmNCEP.met";
##################################
	var_metFile <- paste(path2MetFiles,metFileName,sep="");

	####	minimal no of year to simulate to collect the evaluation of one year only
	# e.g. period == means that the process will simulate apsim for 1950->1955 to evaluate year 1955
	# ex:	period <- 2;
##################################
	period <- 3;
##################################

	####	definition of min and max year that you want to use within the .met file
	# WARNING: according to the "period" above, the .met file has to include "period"-1 year(s) prior to your min year
	perDef <- array(c(
##################################
	1982,		# min year that you want to use as randomness
	2008,		# max year that you want to use as randomness
##################################
	1		# 1: discreet for APSIM
	),dim=3); 

	#
	#	SET VARIABLE FOR THE ALL OPTIMIZATION PROCESS
	#
	
	####	clock module day/month
	# keep the year as "var_startYear" or "var_endYear" to be replaced automatically
	# ex:	var_startDate <-	"15/09/var_startYear";
	# N.B.
	# if your start and end Dates are in the same year, period = 1 will make 2 year simulation
	# if your start and end Dates are in 2 != years, period = 1 will make 1 year simulation
##################################
	var_startDate <-	"1/1/var_startYear";
	var_endDate <-	"31/12/var_endYear";
##################################

	####	remove everything in the working directory
	while(file.exists(paste(path2workingDir,"initFile.sim",sep=""))){
		try(file.remove(paste(path2workingDir,"initFile.sim",sep="")),silent=TRUE);
	}

	####	write the variables that will last for the whole optimization
	file.copy(paste(path2template,simTemplate,sep=""),paste(path2workingDir,"initFile",".sim",sep=""),overwrite=TRUE);
	changeVar(	"var_startDate",	var_startDate,	paste(path2workingDir,"initFile",".sim",sep=""),paste(path2workingDir,"initFile",".sim",sep=""));
	changeVar(	"var_endDate",	var_endDate,	paste(path2workingDir,"initFile",".sim",sep=""),paste(path2workingDir,"initFile",".sim",sep=""));
	changeVar(	"var_metFile",	var_metFile,	paste(path2workingDir,"initFile",".sim",sep=""),paste(path2workingDir,"initFile",".sim",sep=""));

	#
	#	SET VARIABLE DECISIONS TO OPTIMIZE
	#

	####	how many decisions define the decision space
	# ex:	varNo <- 6;
##################################
	varNo <- 3;
##################################

	####	how many crops per Year
	# ex:	cropPerYear <- 2;
##################################
	cropPerYear <- 1;
##################################

	####	set variable names that will appear in the template .sim file in order to be substituted during the process
	# N.B. according to the varNo above, you have to have as many names as variables
	# ex: "var_decision1"
	decNam <- array(c(
##################################
	"var_sowingDate",			#1
	"var_wheatFertAmount",		#2
	"var_wheatIrrAmount"		#3
##################################
	),dim=c(1,varNo));

	####	set decision space, i.e. the exploration boundaries
##################################
#	if text needed (work with one only for now)
	textDec1<-c("10-apr","17-apr","24-apr",
			"01-may","08-may","15-may","22-may","29-may",
			"05-jun","12-jun");
#     otherwise
#	textDec1 <- NULL;
##################################
	# for each decision (ranked exactely as above), define the min, max, step boundaries to be explored
	# you gess min, max, step is the smallest range achievable, set the "step" accordingly to applicabililty finess:
	# can you make a difference between a fertilization amount of 20 and 21 kg? no, the step has to be larger than 1 then.
	# can you make a difference between a fertilization amount of 20 and 30 kg? yes, the step has to be lower or equal to 10 then.
	# can you make a difference between a fertilization amount of 20 and 25 kg? ...
	# N.B. according to the varNo above, you have to have as many (min,max,step) as variables
	decS <- matrix(c(
##################################
	0,	length(textDec1),	1,	# start at 0 for text because I'm gonna ceil it
#	0,	60,	6,	# min, max, minimal step of dec 1
	0,	300,	30,	# min, max, minimal step of dec 2
	0,	60,	6	# min, max, minimal step of dec 3
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
	criNo <- 3;
##################################
	criS <- array(NA,dim=c(1,criNo));
	# only if criNo == 2 (graphics purposes)
	if (criNo==2){
		criS <- array(c(	0,		-6500,	# min, max cri 1
					0,		600		# min, max cri 2
					),dim=c(2,2));
	}
	
	####	check criS vs. criNo coherence
	if(dim(criS)[2]!=criNo){
		print("",quote=FALSE);
		print(	"##########################################",quote=FALSE);
		print(	"# incoherence in between criNo and criS",quote=FALSE);
		print(	"##########################################",quote=FALSE);
		stop();	
	}

return(list("decNam"=decNam,"decS"=decS,"criS"=criS,"path2out"=path2workingDir,"perDef"=perDef,"period"=period,"proNo"=proNo,"textDec1"=textDec1,"cropPerYear"=cropPerYear));
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
	#	- winter wheat in swartland
	#	- report frequency at harvest
	#

	####	column definition
	# mainly in order to ease the reading of the code
	# tip: take the .sim file and follow the variable list
##################################
	col_year <- 1;
	col_ddmmyy <- 2;
	# crop type : wheat
	col_wheatBiomass <- 4;
	col_wheatYield <- 5;
	col_irrigTot <- 6;
	col_fertiliser <- 7;
##################################
#	col_sumNlosses <- 17; # sum of 11,12,13
	
	####	which ones are the criteria to optimize (both maximize and minimize)
	criteria <- array(c(
##################################
	col_wheatYield,		## crit 1
	col_irrigTot,		## crit 2
	col_fertiliser		## crit 3
##################################
	),dim=criNo);

	#### read the .out apsim file
	temp <- read.table(paste(path2Outputs,fileName,".out",sep=""),skip=4,sep=",");
	colNo <- dim(temp)[2]+1;

	#### opt.A : from 1 line only
	# no rotations
	response <- array(NA,dim=c(1,colNo));
	response <- temp[dim(temp)[1],];

#	#### opt.B init : make 2 lines (maize + peanut) one
#	# this is required for rotations (of 2 crops)
#	lin_temp <- 1; lin_res <- 1;	res1 <- temp[lin_temp,];
#	lin_temp <- lin_temp +1;	res2 <- temp[lin_temp,];
#	response <- array(NA,dim=c(1,colNo));
#	for(col in 1:(colNo-1)){
#		if (col==2) next;
#		if(col==1 || col==3 || col==6){
#			if(res1[1,col]!=res2[1,col]){
#				print("apsim_readOutputs incorrect settings (\"apsimInterface.r\")");	stop();
#			}; next;
#		};
#		response[1,col] <- res1[1,col]+res2[1,col];
#	}
#
#	#### opt.B init : make 2 lines (maize + peanut) one
#	# this is required for rotations (of 2 crops)
#	response[1,colNo] <- sum(response[1,11:13]);
#	repeat{
#		lin_temp <- lin_temp +1;	if (lin_temp>dim(temp)[1]) break;
#		lin_res <- lin_res +1;		res1 <- temp[lin_temp,];
#		lin_temp <- lin_temp +1;	res2 <- temp[lin_temp,];
#		res <- array(NA,dim=c(1,colNo));
#		for(col in 1:(colNo-1)){
#			if (col==2) next;
#			if(col==1 || col==3 || col==6){
#				if(res1[1,col]!=res2[1,col]){
#					print("apsim_readOutputs incorrect settings (\"apsimInterface.r\")");	stop();
#				}; next;
#			}; res[1,col] <- res1[1,col]+res2[1,col];
#		};
#		## sum some results into another
#		# this is required total N losses for example
#		res[1,colNo] <- sum(res[1,11:13]);
#		response <- rbind(response,res);
#	}

	####	is optimization a maximization or minimization?
	# every value is going to be minimize, so you need to negate the one you want to maximize
	for (col in c(
##################################
	col_wheatBiomass,		# to maximize
	col_wheatYield		# to maximize
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
apsim_simulate <- function(path2Outputs,simFileName,wait)
{
	path2Origin <- getwd();
	setwd(path2Outputs);

#print("new apsim evaluation");
	shell(paste("Apsim ",simFileName,".sim"," > ",simFileName,".sum",sep="")
				,intern=FALSE, wait=wait, mustWork=TRUE, invisible=FALSE
			)

	setwd(path2Origin);
}

##
 # SIMULATE APSIM FROM REGION LIST
 ####################################################################
simulateApsim <- function(apsimSpec,dec,per,criNo)
{
	decNam <- apsimSpec$decNam;
	path2apsimOutputs <- apsimSpec$path2out;
	perDef <- apsimSpec$perDef;
	period <- apsimSpec$period;
	proNo <- apsimSpec$proNo;
	textDec1 <- apsimSpec$textDec1;
	cropPerYear <- apsimSpec$cropPerYear;
	varNo <- dim(dec);
	perNo <- length(per);
	temp <- create_naDecEva(perNo,criNo);
	year <- array(NA,dim=perNo);

#print("remove file");
#
# I know: it should work without the 'while', even with an 'unlink' ..
# but it does not !!
# so here I am
# 
options(warn=-1);	# disable warnings
	for (p in 1:perNo){
		while(file.exists(paste(path2apsimOutputs,"optimization_",p,".out",sep=""))){
			try(file.remove(paste(path2apsimOutputs,"optimization_",p,".out",sep="")),silent=TRUE);
		}
		while(file.exists(paste(path2apsimOutputs,"fileToSimulate_",p,".sim",sep=""))){
			try(file.remove(paste(path2apsimOutputs,"fileToSimulate_",p,".sim",sep="")),silent=TRUE);
		}
		while(file.exists(paste(path2apsimOutputs,"fileToSimulate_",p,".sum",sep=""))){
			try(file.remove(paste(path2apsimOutputs,"fileToSimulate_",p,".sum",sep="")),silent=TRUE);
		}
	}
options(warn=0);	# enable warnings

### CREATE SIM FILES
#print("create sim file");
	for (p in 1:perNo){
		# per in [0,1] has to be translate into a year
		year[p] <- perDef[1]+per[p]*(perDef[2]-perDef[1]);
		if (perDef[3]==1){
			year[p] <- round(year[p]);
		}
	
		# to reduce rw operations
		## change decision variables in .sim
		if(p==1){
			file.copy(paste(path2apsimOutputs,"initFile.sim",sep=""),paste(path2apsimOutputs,"noYearFile.sim",sep=""),overwrite=TRUE);
			for (v in 1:varNo){
				if(v==1 && !is.null(textDec1)){
					changeVar(decNam[1,v],textDec1[ceiling(dec[v])],paste(path2apsimOutputs,"noYearFile.sim",sep=""),paste(path2apsimOutputs,"noYearFile.sim",sep=""));
					next;
				}
				changeVar(decNam[1,v],dec[v],paste(path2apsimOutputs,"noYearFile.sim",sep=""),paste(path2apsimOutputs,"noYearFile.sim",sep=""));
			}
		}

		## change pertubation variables in .sim
		# i.e. add the year ('period' years simulated for 1 year output)
		# startYear and endYear define the length of the time period simulated (has to be included in the met file)
		file.copy(paste(path2apsimOutputs,"noYearFile.sim",sep=""),paste(path2apsimOutputs,"fileToSimulate_",p,".sim",sep=""),overwrite=TRUE);
		changeVar(	"var_startYear",	year[p]-period,				paste(path2apsimOutputs,"fileToSimulate_",p,".sim",sep=""),paste(path2apsimOutputs,"fileToSimulate_",p,".sim",sep=""));
		changeVar(	"var_endYear",	year[p],					paste(path2apsimOutputs,"fileToSimulate_",p,".sim",sep=""),paste(path2apsimOutputs,"fileToSimulate_",p,".sim",sep=""));
		changeVar(	"var_title",	paste("optimization_",p,sep=""),	paste(path2apsimOutputs,"fileToSimulate_",p,".sim",sep=""),paste(path2apsimOutputs,"fileToSimulate_",p,".sim",sep=""));
	}

### CHECK ON SIM FILES CREATION
#print("   :     wait sim files creation");# potential infinite loop
	enterLoop <- Sys.time();
	repeat{
		simFileCreated <-  array(FALSE,dim=perNo);
		for (p in 1:perNo){
			if(file.exists(paste(path2apsimOutputs,"fileToSimulate_",p,".sim",sep=""))){
				simFileCreated[p] <- TRUE;
			}
		}
		if(sum(simFileCreated)==perNo){
			break;
		}else{
			# I do not know why, but some does not get created ...
			if(a<-difftime(Sys.time(),enterLoop,units="mins") > (perNo*period)){
				print("failing to create sim files");
				browser();
			}
		}
	}

### RUN SIM FILES
#print("run file");
	for (p in 1:perNo){
		## run simulation
		ifelse(p%%proNo==0,sequencial<-TRUE,sequencial<-FALSE);	# one simulation happens in the R windows
		#sequencial<-FALSE;							# none happens in the R windows, but get over my time limits regularly
		apsim_simulate(path2apsimOutputs,paste("fileToSimulate_",p,sep=""),sequencial);
	}
	
### CHECK ON OUT FILES CREATION
#print("   :     wait out files creation");# potential infinite loop
	enterLoop <- Sys.time();
	repeat{
		outFileCreated <-  array(FALSE,dim=perNo);
		for (p in 1:perNo){
			if(file.exists(paste(path2apsimOutputs,"optimization_",p,".out",sep=""))){
				outFileCreated[p] <- TRUE;
			}
		}
		if(sum(outFileCreated)==perNo){
			break;
		}else{
			# because crop might fail to germinate and never creat harvest report
			if(a<-difftime(Sys.time(),enterLoop,units="mins") > (perNo*period)){
				print("failing to create output files");
				browser();
			}
		}
	}

### CHECK ON OUT FILES DATA STORAGE
#print("   :     wait files are not empty");	# potential infinite loop
	enterLoop <- Sys.time();
	repeat{
		fileEmpty <-  array(TRUE,dim=perNo);
		for (p in 1:perNo){
			if(file.info(paste(path2apsimOutputs,"optimization_",p,".out",sep=""))[,"size"]>0){
				fileEmpty[p] <- FALSE;
			}
		}
		if(sum(fileEmpty)==0){
			break;
		}else{
			# because crop might fail to germinate and never creat harvest report
			if(a<-difftime(Sys.time(),enterLoop,units="mins") > (perNo*period)){
				print("failing to write output files");
				browser();
			}
		}
	}

### CHECK ON OUT FILES COMPLETION
#print("   :     wait files completion");	# potential infinite loop
	enterLoop <- Sys.time();
	repeat{
		fileCompleted <-  array(FALSE,dim=perNo);
		for (p in 1:perNo){
			lastYear <- read.table(paste(path2apsimOutputs,"optimization_",p,".out",sep=""),skip=4,sep=",");
# if start and end date are in different years
#			if( dim(lastYear)[1]==(cropPerYear*period)){
# otherwise
			if( dim(lastYear)[1]==(cropPerYear*period+1)){
				fileCompleted[p] <- TRUE;
			}
		}
		if(sum(fileCompleted)==perNo){
			break;
		}else{
			# because crop might fail to germinate and never creat harvest report
			if(a<-difftime(Sys.time(),enterLoop,units="mins") > (perNo*period)){
				print("failing to fill in output files");
				browser();
			}
		}
	}

### READ OUTPUTS
#print("read files");
	for (p in 1:perNo){
		## read outputs from .out files
		# take out only the last year results
		temp[p,1:criNo] <- apsim_readOutputs(path2apsimOutputs,paste("optimization_",p,sep=""),criNo);
	}

#print("get out of simulateApsim");
return(temp);
}


############################################################################################
### only for reminder of katherine's simulations
############################################################################################

## apsim_userSettings <- function()
##################################
#	proNo <- 2;
##################################
#	path2workingDir <- "../../ApsimOptiInKatherine/WorkingDir/";
##################################
#	path2template <- "../../ApsimOptiInKatherine/Templates/";
##################################
#	simTemplate <- "katherine_wet-peanut_dry-maize_rotation_CC2100.sim"
##################################
#	path2MetFiles <- "../../ApsimOptiInKatherine/Met/";
##################################
#	metFileName <- "Tindal.met";
#	period <- 3;
##################################
#	1950,		# min year that you want to use as randomness
#	2005,		# max year that you want to use as randomness
##################################
#	var_startDate <-	"1/12/var_startYear";
#	var_endDate <-	"30/11/var_endYear";
##################################
#	varNo <- 3;
##################################
#	"var_PeanutIrrAmount",		#1
#	"var_MaizeIrrAmount",		#2
#	"var_MaizeFertAmount"		#3
##################################
#	0,	60,	6,	# min, max, minimal step of dec 1
#	0,	60,	6,	# min, max, minimal step of dec 2
#	0,	300,	30	# min, max, minimal step of dec 3
##################################
#	criNo <- 3;
##################################

## apsim_readOutputs <- function(path2Outputs, fileName, criNo)
##################################
#	col_year <- 1;
#	col_ddmmyy <- 2;
#	# crop type : maize
#	col_maizeBiomass <- 4;
#	col_maizeYield <- 5;
#	# crop type : peanut
#	col_peanutBiomass <- 7;
#	col_peanutYield <- 8;
#	col_cumDrainage <- 9;
#	col_cumRunoff <- 10;
#	col_cumDenit <- 11;
#	col_cumNLeached <- 12;
#	col_cumNRunoff <- 13;
#	col_irrigTot <- 14;
#	col_fertiliser <- 15;
#	col_rain <- 16;
##################################
#	col_peanutYield,		## crit 1
#	col_maizeYield,		## crit 2
#	col_sumNlosses		## crit 3
##################################
#	col_maizeBiomass,		# to maximize
#	col_maizeYield,		# to maximize
#	col_peanutBiomass,	# to maximize
#	col_peanutYield		# to maximize
##################################
