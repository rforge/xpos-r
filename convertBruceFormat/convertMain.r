##
 # FILE convertMain.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/convertBruceFormat
 ###############################################################################
 #
 # main algorithm, used for any fromat
 # - convertOne
 #	intends to convert one data set (one GCM - one period) for one model
 # - convert
 #	is here to process convertOne
 #	through lots of data sets and so far APSIM and AQUACROP crop models
 #
 ###############################################################################

## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ###
##									      ##
##									      ##
##              USER JOB: FILL IN AND ADAPT ALL init_* FUNCTIONS	      ##
##									      ##
##									      ##
## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ###

loop_on_paths <- function()
{
	for (it in 1:10){
		convert("ap",allGCM=T,iteration=it);
	}
}

##
 # INITIALISE YOUR DATA PATHS AND NAMES
 ###############################################################################
 # separated folders with "/" even for windows
 # finish all paths and folder name with "/"
 ###############################################################################
init_paths <- function(it)
{
	# in which folder to read the data
	input <- "/home/crespo/Desktop/Link2wine_shared/12_AgMIP/2012-07_observedSentinels/Public";
#	it<-1;
	# in which folder to write out the data
	if (is.null(it)){
		output <- "/home/crespo/Desktop/Link2wine_shared/12_AgMIP/2012-07_observedSentinels/InLandIsF";
	}else{
		output <- paste("/home/crespo/Desktop/11_START/ApsimMetFiles/rep",it,"/",sep="");
	}

	# what are the name of the data folders
#	folder <- list	(	"tmn"=	paste("tmin-",it,"/",sep=""),	# folder name for minimal temperatures
#				"tmx"=	paste("tmax-",it,"/",sep=""),	# folder name for maximal temperatures
#				"ppt"=	paste("ppt-",it,"/",sep="")	# folder name for precipitation
#			);	
	folder <- list	(	"tmn"=	"tmn/",	# folder name for minimal temperatures
				"tmx"=	"tmx/",	# folder name for maximal temperatures
				"ppt"=	"ppt/"	# folder name for precipitation
			);	

return(list("input"=input,"output"=output,"folder"=folder));
}

## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ###
##									      ##
##	           PLAY FREELY WITH WHATEVER IS ABOVE ...		      ##
##       BUT								      ##
##	           PLAY CAREFULLY WITH WHATEVER IS BELOW		      ##
##									      ##
## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ###

##
 # CONVERSION ROUTINE FOR MULTIPLE GCMs
 ###############################################################################
 # > model
 #	"br" for bruce format like
 # 	"ap" for APSIM
 #	"aq" for AQUACROP
 #	"cs" for CSAG (basically you want to compute ETo)
 #	"ds" for DSSAT
 #	"ag" for AgMIP
 #	"all" for both
 # > allGCM
 #	FALSE (default) plays only with stat list (defined in init_stat)
 #	TRUE plays also with GCMs list (defined in init_GCMs) 
 # > requires
 # 	init_paths and init_stat anyway,
 #	+ init_GCMs if allGCM is TRUE
 ###############################################################################
convert <- function(model,allSRES=TRUE,allGCM=TRUE,allPara=TRUE,allStat=TRUE,inLand=FALSE,seeSteps=FALSE,fillIn=TRUE,iteration=NULL)
{
### crop models
	if(model=="all") model <- 1;
	if(model=="ap") model <- 2;
	if(model=="aq") model <- 3;
	if(model=="cs") model <- 4;
	if(model=="ds") model <- 5;
	if(model=="ag") model <- 6;
	if (!is.numeric(model)){
		print("### ERROR: unknown target model");
		print("### targetModel available so far: 'ag' (AgMIP), 'cs' (CSAG), 'ap' (APSIM), 'aq' (AQUACROP), 'cs' (CSAG), 'ds' (DSSAT) or 'all'");
		stop();
	}

### initialisation
	path <- init_paths(iteration);
	staNames <- NULL;

	## SRES
	if(allSRES){
		sres <- list.files(path$input);
		for (es in 1:length(sres)){
			tmp <- list("t"=NULL)
			staNames <- c(staNames,tmp);
			names(staNames)[es]<-sres[es];
		}
	}else{	
		print("make a folder with ONLY the SRES you want to deal with !!");
		browser();
	}

	## GCMs
	if(allGCM){
		for (es in 1:length(staNames)){
			gcms <- list.files(paste(path$input,names(staNames)[es],sep="/"));
			for (gc in 1:length(gcms)){
				tmp <- list("t"=NULL)
				staNames[[es]] <- c(staNames[[es]],tmp);
				names(staNames[[es]])[gc] <- gcms[gc];
			}
		}
	}else{
		print("make a folder with ONLY the GCMs you want to deal with !!");
		browser();
	}

	## parameters
	if(allPara){
		for (es in 1:length(staNames)){
			for (gc in 1:length(staNames[[es]])){
				para <- list.files(paste(path$input,names(staNames)[es],names(staNames[[es]])[gc],sep="/"));
				for(pa in 1:length(para)){
					tmp <- list("t"=NULL)
					staNames[[es]][[gc]] <- c(staNames[[es]][[gc]],tmp);
					names(staNames[[es]][[gc]])[pa] <- para[pa];
				}
			}
		}
	}else{
		print("make a folder with ONLY the parameters you want to deal with !!");
		browser();
	}

	## staNames
	if(allStat){
		for (es in 1:length(staNames)){
			for (gc in 1:length(staNames[[es]])){
				for(pa in 1:length(para)){
					stat <- list.files(paste(path$input,names(staNames)[es],names(staNames[[es]])[gc],names(staNames[[es]][[gc]])[pa],sep="/"));
					staNames[[es]][[gc]][[pa]] <- stat;
				}
			}
		}
	}else{
		print("make a folder with ONLY the stations you want to deal with !!");
		browser();
	}

## given last structure, just go through all staNames of 1 param of all GCMs of all SRES included in staNames
### multiple GCMs routine
		for (es in 1:length(staNames)){
			print(paste(" > SRES : ",names(staNames)[es],sep=""),quote=FALSE);
			for (gc in 1:length(staNames[[es]])){
				print(paste(" > > GCMs : ",names(staNames[[es]])[gc],sep=""),quote=FALSE);
				for(st in 1:length(staNames[[es]][[gc]][[1]])){
					print(paste(" > > > station : ",staNames[[es]][[gc]][[1]][st],sep=""),quote=FALSE);
#					if (is.null(stat[[s]]$arid) || (is.numeric(stat[[s]]$arid) && (stat[[s]]$arid < 1 || stat[[s]]$arid > 5)) || (is.character(stat[[s]]$arid)&& stat[[s]]$arid!='A')){
#						print("# WARNING: wrong arid parameter -> assuming automatic condition",quote=FALSE);
#						stat[[s]]$arid <- 'A';
#					}
					pathToStation <-	list(	"input"=paste(path$input,names(staNames)[es],names(staNames[[es]])[gc],"",sep="/"),
									"output"=paste(path$output,names(staNames)[es],names(staNames[[es]])[gc],"",sep="/"),
									"folder"=	list(	"tmn"=path$folder$tmn,
												"tmx"=path$folder$tmx,
												"ppt"=path$folder$ppt
											),
									"file"=		list(	"temp"=staNames[[es]][[gc]][[1]][st],
												"prec"=staNames[[es]][[gc]][[1]][st]
											),
									"inland"=inLand,
									"arid"='A'
								);
					switch(model,
						{	# all
							convertOne("ap",pathToStation,seeSteps,fillIn);
							convertOne("aq",pathToStation,seeSteps,fillIn);
							convertOne("cs",pathToStation,seeSteps,fillIn);
							convertOne("ds",pathToStation,seeSteps,fillIn);
							convertOne("ag",pathToStation,seeSteps,fillIn);
						},{	# apsim only
							convertOne("ap",pathToStation,seeSteps,fillIn);
						},{	# aquacrop only
							convertOne("aq",pathToStation,seeSteps,fillIn);
						},{	# csag like only
							convertOne("cs",pathToStation,seeSteps,fillIn);
						},{	# dssat only
							convertOne("ds",pathToStation,seeSteps,fillIn);
						},{	# AgMIP only
							convertOne("ag",pathToStation,seeSteps,fillIn);
						}
					);

				}
			}
		}
print(" ... process completed ...");
}

##
 # CONVERT MAIN FUNCTION
 ###############################################################################
 # is the function to be called for any conversion
 # leave pathToStation NULL for one station
 # ##############################################################################
convertOne <- function(targetModel,pathToStation=NULL,seeSteps,fillIn)
{
### sources
	source("checkFunctions.r");
	source("bruceFormat.r");
	source("metTransformations.r");
	source("agriParameters.r");
	source("agriParameters_loops.r");

	if(is.null(pathToStation)){
		print("### ERROR: no station specified !!",quote=FALSE);
		stop();
	}

#?? pathToStation$arid
### target models
	if(targetModel=="ap") targetModel <- 1;
	if(targetModel=="aq") targetModel <- 2;
	if(targetModel=="cs") targetModel <- 3;
	if(targetModel=="ds") targetModel <- 4;
	if(targetModel=="ag") targetModel <- 5;
	if (!is.numeric(targetModel)){
		print("### ERROR: unknown target model",quote=FALSE);
		print("### targetModel available so far: 'ds' (DSSAT), 'cs' (CSAG), 'ap' (APSIM), 'aq' (AQUACROP) or 'all'",quote=FALSE);
		stop();
	}

## because so far, no crop model deal with missing data
 # 1: I am only interseted in the time period covered by tmn, tmx and ppt
	if(seeSteps)	print("... check unconsistencies ...",quote=FALSE);
	fileHead <- checkData(pathToStation);
	if(seeSteps)	print("... import data ...",quote=FALSE);
	data <- importData(pathToStation,fileHead);
## because so far, no crop model deal with missing data
 # 2: if there is any missing data (that I can spot) I will warn the user
	if(seeSteps)	print("... looking for missing data ...",quote=FALSE);
	data <- checkMissing(data);
	checkTmpRain(data);
## because so far, no crop model deal with missing data
 # 3: I need to make sure years are made of real number of days
	switch(fileHead$period$type+1,
		{	# 0 is for real
		},{	# 1 is for 365
			data <- transform_type1(data,fileHead,fillIn);
		},{	# 2 is for 360
			data <- transform_type2(data,fileHead,fillIn);
		}
	);

	if(seeSteps)	print("... create years and julian days ...",quote=FALSE);
	data <- createYearJulianDays(data,fileHead);

## then starts the crop model related operations
	switch(targetModel,
		{	#################### APSIM #
			source('convertToApsim.r');
			if(seeSteps)	print("... compute radiation ...",quote=FALSE);
			data <- compute_radn(data,fileHead$station,pathToStation$inland);
			if(seeSteps)	print("... compute tav and amp ...",quote=FALSE);
			data <- compute_tavNamp(data);
			if(seeSteps)	print("... format and write data into .met file ...",quote=FALSE);
			formatToMetFile(data,fileHead,pathToStation);
		},	# APSIM ####################
		{	################# AQUACROP #
			# aquacrop deals with day, 10-days and monthly records
			# so far we deal only with day records
			source('convertToAquacrop.r');
			if(seeSteps)	print("... compute ETo ...",quote=FALSE);
			if(is.numeric(pathToStation$arid)){
				data <- compute_ETo(data,fileHead,pathToStation$inland,pathToStation$arid);
			}else{
				data <- compute_ETo(data,fileHead,pathToStation$inland,3);
				counter <- 0;					# infinite loop check
				while(data$doItAgain!=0){
					counter <- counter+1;
					if( counter > 2){
						print("ERROR: infinite loop in convertOne function, my mistake",quote=FALSE);
						stop();
					}
					data <- compute_ETo(data,fileHead,pathToStation$inland,data$arid+data$doItAgain);
				}
			}
			if(seeSteps)	print("... format and write data into .TMP, .PLU and .ETo files ...",quote=FALSE);
			formatToTMPFile(data,fileHead,pathToStation);
			formatToPLUFile(data,fileHead,pathToStation);
			formatToEToFile(data,fileHead,pathToStation);
		},	# AQUACROP ####################
		{	######################## CSAG #
			if(seeSteps)	print("... compute ETo ...",quote=FALSE);
			if(is.numeric(pathToStation$arid)){
				data <- compute_ETo(data,fileHead,pathToStation$inland,pathToStation$arid);
			}else{
				data <- compute_ETo(data,fileHead,pathToStation$inland,3);
				counter <- 0;					# infinite loop check
				while(data$doItAgain!=0){
					counter <- counter+1;
					if( counter > 2){
						print("ERROR: infinite loop in convertOne function, my mistake",quote=FALSE);
						stop();
					}
					data <- compute_ETo(data,fileHead,pathToStation$inland,data$arid+data$doItAgain);
				}
			}
			if(seeSteps)	print("... remove added data ...",quote=FALSE);
			data <- removeAddedDays(data,fileHead);
			if(seeSteps)	print("... format and write data into .rdn and .eto files ...",quote=FALSE);
			formatToEToCSAG(data,fileHead,pathToStation);
			formatToRdnCSAG(data,fileHead,pathToStation);
			# formatToTmnCSAG(data,fileHead,pathToStation);
		},	# CSAG ########################
		{	####################### DSSAT #
			source('convertToDssat.r');
			if(seeSteps)	print("... compute radiation ...",quote=FALSE);
			data <- compute_radn(data,fileHead$station,pathToStation$inland);
			if(seeSteps)	print("... compute tav and amp ...",quote=FALSE);
			data <- compute_tavNamp(data);
			if(seeSteps)	print("... format and write data into .WTH file ...",quote=FALSE);
			formatToWTHfile(data,fileHead,pathToStation);
		}	# DSSAT #########################
	);

if(seeSteps)	print("... conversion completed ...",quote=FALSE);
}


###############################################################################
#
#	I AM NOT SURE I NEED THOSE ANYMORE ... ?
#
###############################################################################


##
 # STATION NAMES - from one to multiple stat
 ###############################################################################
 # temp and prec names are separated because they are different in some cases
 ###############################################################################
init_stat <- function(all=T,input)
{
if (all){
	stat <- list.files()
}else{
	stat <- list(#	list(	"temp"="templateName1.txt",		# name for temp data file
#				"prec"="templateName2.txt",		# name for prec data file (could be the same)
#				"arid"= 'A',				# default='A' - humidity conditions from 1 (humid) to 6 (hyper-arid)
#				"inLand"=TRUE),				# is the station in land (TRUE) or on the coast (FALSE)
#			list(	"temp"="0331520.1.txt",
#				"prec"="0331520.1.txt",
#				"arid"= 'A',
#				"inLand"=TRUE),
			list(	"temp"="quin5410.txt",
				"prec"="quin5410.txt",
				"arid"= 'A',
				"inLand"=TRUE),
			list(	"temp"="0261516.1.txt",
				"prec"="0261516.1.txt",
				"arid"= 'A',
				"inLand"=TRUE),
			list(	"temp"="0293597.2.txt",
				"prec"="0293597.2.txt",
				"arid"= 'A',
				"inLand"=TRUE),
			list(	"temp"="0327101.1.txt",
				"prec"="0327101.1.txt",
				"arid"= 'A',
				"inLand"=TRUE),
			list(	"temp"="0331520.1.txt",
				"prec"="0331520.1.txt",
				"arid"= 'A',
				"inLand"=TRUE),
			list(	"temp"="0331585.1.txt",
				"prec"="0331585.2.txt",
				"arid"= 'A',
				"inLand"=TRUE),
			list(	"temp"="0399894.1.txt",
				"prec"="0399894.1.txt",
				"arid"= 'A',
				"inLand"=TRUE),
			list(	"temp"="0400730.1.txt",
				"prec"="0400730.1.txt",
				"arid"= 'A',
				"inLand"=TRUE)
		);
}
return(stat);
}

##
 # GC MODELS NAMES
 ###############################################################################
 # you need it only for multiple GCMs routine conversion
 ###############################################################################
init_GCMs <- function()
{
#GCMs <- list(#	"obs"=	list(	"con"=	"FltStnData/"),
#		"ncep"=	list(	"con"=	"ncep2.2009/"),
#		"cccm"=	list(	"con"=	"cccma_cgcm3_1/",
#				"futA"=	"cccma_cgcm3_1-fa/",
#				"futB"=	"cccma_cgcm3_1-fb/"),
#		"cnrm"=	list(	"con"=	"cnrm_cm3/",
#				"futA"=	"cnrm_cm3-fa/",
#				"futB"=	"cnrm_cm3-fb/"),
#		"csiro35"=list(	"con"=	"csiro_mk3_5/",
#				"futA"=	"csiro_mk3_5-fa/",
#				"futB"=	"csiro_mk3_5-fb/"),
#		"gfdl0"=list(	"con"=	"gfdl_cm2_0/",
#				"futA"=	"gfdl_cm2_0-fa/",
#				"futB"=	"gfdl_cm2_0-fb/"),
#		"gfdl1"=list(	"con"=	"gfdl_cm2_1/",
#				"futA"=	"gfdl_cm2_1-fa/",
#				"futB"=	"gfdl_cm2_1-fb/"),
#		"giss"=	list(	"con"=	"giss_model_e_r/",
#				"futA"=	"giss_model_e_r-fa/",
#				"futB"=	"giss_model_e_r-fb/"),
#		"ipsl"=	list(	"con"=	"ipsl_cm4/",
#				"futA"=	"ipsl_cm4-fa/",
#				"futB"=	"ipsl_cm4-fb/"),
#		"echam"=list(	"con"=	"mpi_echam5/",
#				"futA"=	"mpi_echam5-fa/",
#				"futB"=	"mpi_echam5-fb/"),
#		"mri"=list(	"con"=	"mri_cgcm2_3_2a/",
#				"futA"=	"mri_cgcm2_3_2a-fa/",
#				"futB"=	"mri_cgcm2_3_2a-fb/"),
#		"echo"=	list(	"con"=	"miub_echo_g/",
#				"futA"=	"miub_echo_g-fa/",
#				"futB"=	"miub_echo_g-fb/")
#		"caafx"=list(	"a"=	"caafa/",
#				"b"=	"caafb/",
#				"c"=	"caafc/",
#				"d"=	"caafd/",
#				"e"=	"caafe/",
#				"f"=	"caaff/",
#				"g"=	"caafg/",
#				"h"=	"caafh/",
#				"i"=	"caafi/",
#				"j"=	"caafj/")
#	);
#return(GCMs);
}
