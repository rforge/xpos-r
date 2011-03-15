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

##
 # INITIALISE YOUR DATA PATHS AND NAMES
 ###############################################################################
 # separated folders with "/" even for windows
 # finish all paths and folder name with "/"
 ###############################################################################
init_paths <- function()
{
	# in which folder to read the data
	input <- "//media/E815-F933/AllData/A2.Senario.2.3/run.nigeria/FltStnData/";

	# in which folder to write out the data
	output <- "/media/E815-F933/AllData/A2.Senario.2.3/APSIMformat/FltStnData/"

	# what are the name of the data folders
	folder <- list	(	"tmn"=	"tmin/",	# folder name for minimal temperatures
				"tmx"=	"tmax/",	# folder name for maximal temperatures
				"ppt"=	"ppt/"		# folder name for precipitation
			);	

return(list("input"=input,"output"=output,"folder"=folder));
}

##
 # STATION NAMES - from one to multiple stations
 ###############################################################################
 # temp and prec names are separated because they are different in some cases
 ###############################################################################
init_stations <- function()
{
stations <- list(#	list(	"temp"="templateName1.txt",		# name for temp data file
#				"prec"="templateName2.txt",		# name for prec data file (could be the same)
#				"arid"= 'A',				# default='A' - humidity conditions from 1 (humid) to 6 (hyper-arid)
#				"inLand"=TRUE),				# is the station in land (TRUE) or on the coast (FALSE)
#			list(	"temp"="0725756AW.txt",
#				"prec"="0725756AW.txt",
#				"arid"= 'A',
#				"inLand"=TRUE),
#			list(	"temp"="CHOKWE.txt",
#				"prec"="CHOKWE.txt",
#				"arid"= 'A',
#				"inLand"=TRUE),
#			list(	"temp"="SUSSUNDENGA.txt",
#				"prec"="SUSSUNDENGA.txt",
#				"arid"= 'A',
#				"inLand"=TRUE),
#			list(	"temp"="XAI-XAI.txt",
#				"prec"="XAI-XAI.txt",
#				"arid"= 'A',
#				"inLand"=FALSE)
			list(	"temp"="65001.txt",
				"prec"="65001.txt",
				"arid"= 'A',
				"inLand"=FALSE)
		);

return(stations);
}

##
 # GC MODELS NAMES
 ###############################################################################
 # you need it only for multiple GCMs routine conversion
 ###############################################################################
init_GCMs <- function()
{
GCMs <- list(	"obs"=	list(	"con"=	"FltStnData/"),
		"ncep"=	list(	"con"=	"ncep2.2009/"),
		"cccm"=	list(	"con"=	"cccma_cgcm3_1/",
				"futA"=	"cccma_cgcm3_1-fa/",
				"futB"=	"cccma_cgcm3_1-fb/"),
		"cnrm"=	list(	"con"=	"cnrm_cm3/",
				"futA"=	"cnrm_cm3-fa/",
				"futB"=	"cnrm_cm3-fb/"),
		"csiro35"=list(	"con"=	"csiro_mk3_5/",
				"futA"=	"csiro_mk3_5-fa/",
				"futB"=	"csiro_mk3_5-fb/"),
		"gfdl0"=list(	"con"=	"gfdl_cm2_0/",
				"futA"=	"gfdl_cm2_0-fa/",
				"futB"=	"gfdl_cm2_0-fb/"),
		"gfdl1"=list(	"con"=	"gfdl_cm2_1/",
				"futA"=	"gfdl_cm2_1-fa/",
				"futB"=	"gfdl_cm2_1-fb/"),
		"giss"=	list(	"con"=	"giss_model_e_r/",
				"futA"=	"giss_model_e_r-fa/",
				"futB"=	"giss_model_e_r-fb/"),
		"ipsl"=	list(	"con"=	"ipsl_cm4/",
				"futA"=	"ipsl_cm4-fa/",
				"futB"=	"ipsl_cm4-fb/"),
		"echam"=list(	"con"=	"mpi_echam5/",
				"futA"=	"mpi_echam5-fa/",
				"futB"=	"mpi_echam5-fb/"),
		"mri"=list(	"con"=	"mri_cgcm2_3_2a/",
				"futA"=	"mri_cgcm2_3_2a-fa/",
				"futB"=	"mri_cgcm2_3_2a-fb/"),
		"echo"=	list(	"con"=	"miub_echo_g/",
				"futA"=	"miub_echo_g-fa/",
				"futB"=	"miub_echo_g-fb/")
	);
return(GCMs);
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
 # 	"ap" for APSIM
 #	"aq" for AQUACROP
 #	"all" for both
 # > manyGCMs
 #	FALSE (default) plays only with stations list (defined in init_stations)
 #	TRUE plays also with GCMs list (defined in init_GCMs) 
 # > requires
 # 	init_paths and init_stations anyway,
 #	+ init_GCMs if manyGCMs is TRUE
 ###############################################################################
convert <- function(model,manyGCMs=FALSE)
{
### crop models
	if(model=="all") model <- 1;
	if(model=="ap") model <- 2;
	if(model=="aq") model <- 3;
	if (!is.numeric(model)){
		print("### ERROR: unknown target model");
		print("### targetModel available so far: 'ap' (APSIM), 'aq' (AQUACROP) or 'all' (both)");
		stop();
	}

### initialisation
	path <- init_paths();
	if (manyGCMs)	gcms <- init_GCMs();
	stations <- init_stations();

### multiple GCMs routine
	if(manyGCMs){
		for (g in 1:length(gcms)){
			for (p in 1:length(gcms[[g]])){
				print(paste(" > GCM : ",gcms[[g]][p],sep=""),quote=FALSE);
				for (s in 1:length(stations)){
					if (is.null(stations[[s]]$arid) || (is.numeric(stations[[s]]$arid) && (stations[[s]]$arid < 1 || stations[[s]]$arid > 5)) || (is.character(stations[[s]]$arid)&& stations[[s]]$arid!='A')){
						print("# WARNING: wrong arid parameter -> assuming automatic condition",quote=FALSE);
						stations[[s]]$arid <- 'A';
					}
					pathToStation <-	list(	"input"=paste(path$input,gcms[[g]][[p]],sep=""),
									"output"=paste(path$output,gcms[[g]][[p]],sep=""),
									"folder"=	list(	"tmn"=path$folder$tmn,
												"tmx"=path$folder$tmx,
												"ppt"=path$folder$ppt
											),
									"file"=		list(	"temp"=stations[[s]]$temp,
												"prec"=stations[[s]]$prec
											),
									"inland"=stations[[s]]$inLand,
									"arid"=stations[[s]]$arid
								);
					print(paste(" ---------->  station: ",stations[[s]]$temp,sep=""),quote=FALSE);
					switch(model,
						{	# all
							convertOne("ap",pathToStation,FALSE);
							convertOne("aq",pathToStation,FALSE);
						},{	# apsim only
							convertOne("ap",pathToStation,FALSE);
						},{	# aquacrop only
							convertOne("aq",pathToStation,FALSE);
						}
					);
				}
			}	
		}
	}else{
### single GCM routine
		for (s in 1:length(stations)){
			if (is.null(stations[[s]]$arid) || (is.numeric(stations[[s]]$arid) && (stations[[s]]$arid < 1 || stations[[s]]$arid > 5)) || (is.character(stations[[s]]$arid)&& stations[[s]]$arid!='A')){
				print("# WARNING: wrong arid parameter -> assuming automatic condition",quote=FALSE);
				stations[[s]]$arid <- 'A';
			}
			pathToStation <-	list(	"input"=path$input,
							"output"=path$output,
							"folder"=	list(	"tmn"=path$folder$tmn,
										"tmx"=path$folder$tmx,
										"ppt"=path$folder$ppt
									),
							"file"=		list(	"temp"=stations[[s]]$temp,
										"prec"=stations[[s]]$prec
									),
							"inland"=stations[[s]]$inLand,
							"arid"=stations[[s]]$arid
						);
			print(paste(" ---------->  station: ",stations[[s]]$temp,sep=""),quote=FALSE);
			switch(model,
				{	# all
					convertOne("ap",pathToStation,FALSE);
					convertOne("aq",pathToStation,FALSE);
				},{	# apsim only
					convertOne("ap",pathToStation,FALSE);
				},{	# aquacrop only
					convertOne("aq",pathToStation,FALSE);
				}
			);
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
convertOne <- function(targetModel,pathToStation=NULL,seeSteps=FALSE)
{
### sources
	source("checkFunctions.r");
	source("bruceFormat.r");
	source("agriParameters.r");
	source("metTransformations.r");

	if(is.null(pathToStation)){
		print("### ERROR: no station specified !!",quote=FALSE);
		stop();
	}
pathToStation$arid
### target models
	if(targetModel=="ap") targetModel <- 1;
	if(targetModel=="aq") targetModel <- 2;
	if (!is.numeric(targetModel)){
		print("### ERROR: unknown target model",quote=FALSE);
		print("### targetModel available so far: 'APSIM' or 'AQUACROP'",quote=FALSE);
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
			data <- transform_type1(data,fileHead);
		},{	# 2 is for 360
			data <- transform_type2(data,fileHead);
		}
	);

	if(seeSteps)	print("... create years and julian days ...",quote=FALSE);
	data <- createYearJulianDays(data,fileHead);

## then starts the crop model related operations
	switch(targetModel,
		{	                            #################### APSIM #
			source('convertToApsim.r');
			if(seeSteps)	print("... compute radiation ...",quote=FALSE);
			data <- compute_radn(data,fileHead$station,pathToStation$inland);
			if(seeSteps)	print("... compute tav and amp ...",quote=FALSE);
			data <- compute_tavNamp(data);
			if(seeSteps)	print("... format and write data into .met file ...",quote=FALSE);
			formatToMetFile(data,fileHead,pathToStation);
		},	# APSIM ####################
		{	                            ################# AQUACROP #
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
		}	# AQUACROP #################
	);

if(seeSteps)	print("... conversion completed ...",quote=FALSE);
}

	# in which folder to read the data
	input <- "C:/Documents and Settings/crespo/Desktop/Apsim/WesternCape/StephenFarm_GCMs/";

	# in which folder to write out the data
	output <- "C:/Documents and Settings/crespo/Desktop/Apsim/WesternCape/ApsimMetFiles/CombinedCongress/"

	# what are the name of the data folders
	folder <- list	(	"tmn"=	"tmn/",	# folder name for minimal temperatures
				"tmx"=	"tmx/",	# folder name for maximal temperatures
				"ppt"=	"ppt/"		# folder name for precipitation
			);	

return(list("input"=input,"output"=output,"folder"=folder));
}

