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
	simTemplate <- "wet-peanut_dry-maize_rotation-template220909.sim"

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
	varNo <- 12;
	# irrigation module
	decNam <- array(c(	"var_peanutIrrAmount",	#1
					"var_maizeIrrAmount",	#2
	# fertilization module
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
			),dim=c(1,varNo));

	####### decision space definition
	decS <- matrix(c(	20,	60,	10,	# min, max, minimal step of dec 1
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
	## define thus  criteria in "apsim_readOutputs"
	## minimization ...
	if (criNo==2){
		criS <- matrix(c(	-1000,	-10000,	# min, max cri 1
					-4000,	-20000	# min, max cri 2
					),2);
	}else{criS <- NULL;}
	
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
apsim_readOutputs <- function(path2Outputs, fileName, criNo)
{
	##### file column definitions
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
	## which ones are the criteria
	criteria <- array(c(8,5),dim=criNo);

	## read .out apsim file
	temp <- read.table(paste(path2Outputs,fileName,".out",sep=""),skip=4,sep=",");
	colNo <- dim(temp)[2];

	## make 2 lines (maize + peanut) one
	lin_temp <- 1;
	lin_res <- 1;

	res1 <- temp[lin_temp,];
	lin_temp <- lin_temp +1;
	res2 <- temp[lin_temp,];
	response <- array(NA,dim=c(1,colNo));

	for(col in 1:colNo){
		if (col==2) next;
		if(col==1 || col==3 || col==6){
			if(res1[1,col]!=res2[1,col]){
				print("apsim_readOutputs incorrect settings (\"apsimInterface.r\")");
				stop();
			}
			next;
		}
		response[1,col] <- res1[1,col]+res2[1,col];
	}

	repeat{
		lin_temp <- lin_temp +1;
		lin_res <- lin_res +1;
		if (lin_temp>dim(temp)[1]) break;

		res1 <- temp[lin_temp,];
		lin_temp <- lin_temp +1;
		res2 <- temp[lin_temp,];
		res <- array(NA,dim=c(1,colNo));

		for(col in 1:colNo){
			if (col==2) next;
			if(col==1 || col==3 || col==6){
				if(res1[1,col]!=res2[1,col]){
					print("apsim_readOutputs incorrect settings (\"apsimInterface.r\")");
					stop();
				}
				next;
			}
			res[1,col] <- res1[1,col]+res2[1,col];
		}
		response <- rbind(response,res);
	}

	##### signs for optimization
	# every value is going to be minimize (negate maximization)
	for (col in c(col_maizeBiomass,col_maizeYield,col_peanutBiomass,col_peanutYield)){
		response[,col] <- -response[,col];
	}

	criteria_vect <- array(NA,dim=c(1,criNo));
	for (c in 1:criNo){
		criteria_vect[1,c] <- response[dim(response)[1],criteria[c]]
	}

return(criteria_vect);
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
return(apsim_readOutputs(path2apsimOutputs,"simulation",criNo));
}
