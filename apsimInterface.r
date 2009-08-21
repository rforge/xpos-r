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
	varNo <- 2;
	path2Templates <- "../Templates/";
	path2Outputs <- "../Outputs/";
	path2MetFiles <- "C:\\\\Documents and Settings\\\\CRE256\\\\Desktop\\\\Katherine\\\\Met\\\\";
	simTemplate <- "wet-peanut_dry-maize_rotation-template.sim"

	######
	# met file are not part of decisions
	# but years of a met file will be the perturbations
	######
	var_metFile <- paste(path2MetFiles,
		"Tindal.met",sep="");
	#	"KatherineAERO.met",sep="");
	#	"Berrimah.met",sep="");
	period <- 1;
	# at this date we simulate 'period' years (e.g. 1950->1955)
	# to find out the response of year 1955
	# min year shall thus be at least min of met file +period+1
	perDef <- array(c(
		1950,		# min perturbation parameter
		2005,		# max perturbation parameter
		1		# 1: discreet, 0: continuous
		),dim=3); 

	####### set for all simulations
	## clock module
		var_startDate <-	"1/11/var_startYear";
		var_endDate <-	"31/10/var_endYear";
	## irrigation module
		var_ASWdepth <-	1000;
	# 	var_frASW <- ;
	# 	var_irrEff <- ;

	# actually write it
	file.copy(paste(path2Templates,simTemplate,sep=""),paste(path2Outputs,"initFile",".sim",sep=""),overwrite=TRUE);
	changeVar(	"var_title",	"simulation",		paste(path2Outputs,"initFile",".sim",sep=""),paste(path2Outputs,"initFile",".sim",sep=""));
	changeVar(	"var_startDate",	var_startDate,	paste(path2Outputs,"initFile",".sim",sep=""),paste(path2Outputs,"initFile",".sim",sep=""));
	changeVar(	"var_endDate",	var_endDate,	paste(path2Outputs,"initFile",".sim",sep=""),paste(path2Outputs,"initFile",".sim",sep=""));
	changeVar(	"var_ASWdepth",	var_ASWdepth,	paste(path2Outputs,"initFile",".sim",sep=""),paste(path2Outputs,"initFile",".sim",sep=""));
	changeVar(	"var_metFile",	var_metFile,	paste(path2Outputs,"initFile",".sim",sep=""),paste(path2Outputs,"initFile",".sim",sep=""));

	####### decision to optimize
	# irrigation module
	decNam <- array(c(	"var_frASW",	
					"var_irrEff"
		),dim=c(1,varNo));

	####### decision space definition
	decS <- matrix(c(	0.5,	1,	0.1,	# min, max, minimal step of dec 1
				0.5,	1,	0.1	# min, max, minimal step of dec 2
			),3);

	####### check coherenc
	if(dim(decS)[2]!=varNo){
		print("",quote=FALSE);
		print(	"##########################################",quote=FALSE);
		print(	"# incoherence in between the procedure and APSIM initialisation",quote=FALSE);
		print(	"##########################################",quote=FALSE);
		stop();	
	}	

return(list("decNam"=decNam,"decS"=decS,"path2out"=path2Outputs,"perDef"=perDef,"period"=period));
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
	col_year <- 1;
	col_peanutYield <- 5;
	col_maizeYield <- 8;

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
	# i.e. add the year (5 years simulated for 1 year output)
	# startYear and endYear define the length of the time period simulated (has to be included in the met file)
	file.copy(paste(path2apsimOutputs,"noYearFile.sim",sep=""),paste(path2apsimOutputs,"fileToSimulate.sim",sep=""),overwrite=TRUE);
	changeVar(	"var_startYear",	year-period,	paste(path2apsimOutputs,"fileToSimulate.sim",sep=""),paste(path2apsimOutputs,"fileToSimulate.sim",sep=""));
	changeVar(	"var_endYear",	year,		paste(path2apsimOutputs,"fileToSimulate.sim",sep=""),paste(path2apsimOutputs,"fileToSimulate.sim",sep=""));

	## run simulation
	apsim_simulate(path2apsimOutputs,"fileToSimulate");
	
	## read outputs from .out files
	# take out only the last year results
	temp <- apsim_readOutputs(path2apsimOutputs,"simulation");
	
return(temp[dim(temp)[1],2:dim(temp)[2]]);
}
