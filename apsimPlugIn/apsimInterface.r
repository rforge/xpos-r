##
 # FILE apsimInterface.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
 ####################################################################
 # read, extract, replace, create sim files
 # according to APSIM
 ####################################################################
 # user has to update accordingly to his/her wishes
 # - apsim_init
 # - apsim_readOutputs
 ####################################################################


##
 # USER SPECIFIC CONDITIONS SETTINGS FOR APSIM
 # this could be done by reading an input file or asking the user
 ####################################################################
apsim_userSettings <- function()
{
	path2Templates <- "../../Templates/";
	path2Outputs <- "../../Outputs/";
	path2MetFiles <- "C:\\\\Documents and Settings\\\\CRE256\\\\Desktop\\\\Katherine\\\\Met\\\\";
	simTemplate <- "wet-peanut_dry-maize_rotation-template140909.sim"

	######
	# met file are not part of decisions
	# but years of a met file will be the perturbations
	######
	var_metFile <- paste(path2MetFiles,
		"Tindal.met",sep="");
	#	"KatherineAERO.met",sep="");
	#	"Berrimah.met",sep="");
	period <- 1; 
	# at this date we simulate 'period' years
	# (e.g. period <- 5 means we simulate 1950->1955)
	# to find out the response of year final year ONLY
	# min year shall thus be at least min of met file +period+1
	perDef <- array(c(
		1950,		# min perturbation parameter
		2005,		# max perturbation parameter
		1		# 1: discreet, 0: continuous
		),dim=3); 

	####### set for all simulations
	## clock module
		var_startDate <-	"15/09/var_startYear";
		var_endDate <-	"31/12/var_endYear";

	# actually write it
	file.copy(paste(path2Templates,simTemplate,sep=""),paste(path2Outputs,"initFile",".sim",sep=""),overwrite=TRUE);
	changeVar(	"var_title",	"simulation",	paste(path2Outputs,"initFile",".sim",sep=""),paste(path2Outputs,"initFile",".sim",sep=""));
	changeVar(	"var_startDate",	var_startDate,	paste(path2Outputs,"initFile",".sim",sep=""),paste(path2Outputs,"initFile",".sim",sep=""));
	changeVar(	"var_endDate",	var_endDate,	paste(path2Outputs,"initFile",".sim",sep=""),paste(path2Outputs,"initFile",".sim",sep=""));
	changeVar(	"var_metFile",	var_metFile,	paste(path2Outputs,"initFile",".sim",sep=""),paste(path2Outputs,"initFile",".sim",sep=""));

	####### decision to optimize
	varNo <- 8;
	# irrigation module
	decNam <- array(c(	"var_peanutIrrAmount",	#1
					"var_maizeIrrAmount",	#2
	# fertilization module
					"var_maizeFerAtSow",	#3
					"var_maizeFerAt21",	#4
					"var_maizeFerAt35",	#5
					"var_maizeFerAt49",	#6
					"var_maizeFerAt63",	#7
					"var_maizeFerAt77"	#8
			),dim=c(1,varNo));

	####### decision space definition
	decS <- matrix(c(	10,	60,	10,	# min, max, minimal step of dec 1
				10,	60,	10,	# min, max, minimal step of dec 2
				15,	35,	5,	# min, max, minimal step of dec 3
				20,	40,	5,	# min, max, minimal step of dec 4
				45,	65,	5,	# min, max, minimal step of dec 5
				65,	85,	5,	# min, max, minimal step of dec 6
				60,	80,	5,	# min, max, minimal step of dec 7
				10,	30,	5	# min, max, minimal step of dec 8
			),3);

	####### check coherenc
	if(dim(decS)[2]!=varNo){
		print("",quote=FALSE);
		print(	"##########################################",quote=FALSE);
		print(	"# incoherence in between decNo and decS",quote=FALSE);
		print(	"##########################################",quote=FALSE);
		stop();	
	}	

	####### criteria space definition
	criNo <- 2;
	# minimization ...
	criS <- matrix(c(	-1000,	-6000,	# min, max cri 1
				-4000,	-14000	# min, max cri 2
			),2);
	
	####### check coherenc
	if(dim(criS)[2]!=criNo){
		print("",quote=FALSE);
		print(	"##########################################",quote=FALSE);
		print(	"# incoherence in between criNo and criS",quote=FALSE);
		print(	"##########################################",quote=FALSE);
		stop();	
	}

return(list("decNam"=decNam,"decS"=decS,"criS"=criS,"path2out"=path2Outputs,"perDef"=perDef,"period"=period));
}

##
 # system evaluation given the variables
 ####################################################################
apsim_simulate <- function(path2Outputs,simFileName)
{
	path2Origin <- getwd();
	setwd(path2Outputs);

#	print("start apsim evaluation");
	writeLines(shell(paste("Apsim ",simFileName,".sim",sep=""), intern=TRUE, wait=TRUE),paste(simFileName,".sum",sep=""),sep="\n");
#	print("end apsim evaluation");

	setwd(path2Origin);
}

##
 # read outputs from .out file
 # NEEDS TO BE UPDATE ACCORDINGLY TO USER NEEDS
 # !! remember objectives are minimized
 ####################################################################
apsim_readOutputs <- function(path2Outputs, fileName)
{
	##### file column definitions
	col_year <- 1;
	col_ddmmyy <- 2;
	# crop type : maize
	col_maizeBiomass <- 4;
	col_maizeYield <- 5;		## crit 2
	# crop type : peanut
	col_peanutBiomass <- 7;
	col_peanutYield <- 8;		## crit 1
	col_cumDrainage <- 9;
	col_cumRunoff <- 10;
	col_cumDenit <- 11;
	col_cumNLeached <- 12;
	col_cumNRunoff <- 13;
	col_irrigTot <- 14;
	col_fertiliser <- 15;
	col_rain <- 16;

	temp <- read.table(paste(path2Outputs,fileName,".out",sep=""),skip=4,sep=",");
	
	lin_temp <- 1; lin_res <- 1;
	response <- array(c(temp[lin_temp,col_year],temp[lin_temp,col_peanutYield],NULL),dim=c(1,3));
	lin_temp <- lin_temp +1;
	ifelse(is.na(temp[lin_temp,col_maizeYield]),
		response[lin_res,3] <- 0,
		response[lin_res,3] <- temp[lin_temp,col_maizeYield]
	);
	repeat{
		lin_temp <- lin_temp +1;
		lin_res <- lin_res +1;
		if (lin_temp>dim(temp)[1]) break;

		response <- rbind(response,array(c(temp[lin_temp,col_year],temp[lin_temp,col_peanutYield],NULL),dim=c(1,3)));
		lin_temp <- lin_temp +1;
		ifelse(is.na(temp[lin_temp,col_maizeYield]),
			response[lin_res,3] <- 0,
			response[lin_res,3] <- temp[lin_temp,col_maizeYield]
		);
	}

return(-response);	# - to fit minimization requirements
}

##
 # SIMULATE APSIM FROM REGION LIST
 ####################################################################
simulateApsim <- function(apsimSpec,dec,per,newDec)
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
	temp <- apsim_readOutputs(path2apsimOutputs,"simulation");

return(temp[dim(temp)[1],2:dim(temp)[2]]);
}
