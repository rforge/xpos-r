##
 # FILE convertAll.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/convertBruceFormat
 ###############################################################################
 #
 # main algorithm, used for any fromat
 # which intend to convert 1 data set for 1 time period only
 #
 ###############################################################################
 # if you are running the script in linux for linux use of the files, that's fine
 # if you are running the script in windows for windows use of the files, that's fine
 # BUT if you are running the script in linux for windows use of the files
 # you'll face some formating problem
 # we thought about fixing that, but if you intend to use it on windows,
 # I guess you can afford to run the script on windows ...
 ###############################################################################

##
 # INITIALISE YOUR DATA PATHS AND NAMES
 ###############################################################################
 # so far you write them hard in the code
 # separated with "/" even for windows
 # finish all paths and folder name with "/"
 ###############################################################################
readPaths <- function()
{
	# in which folder to read the data
	input <- "/home/csag/crespo/Desktop/ConversionTestData/Inputs/";

	# in which folder to write out the data
	output <- "/home/csag/crespo/Desktop/ConversionTestData/Test/"

	# what is the name of the max temp data folder
	folder <- list	(	"tmn"=	"ex_tmn/",	# folder name for minimal temperatures
				"tmx"=	"ex_tmx/",	# folder name for maximal temperatures
				"ppt"=	"ex_ppt/"		# folder name for precipitation
			);	

	# what are the names of temp and prec data files
	file <- list	(	"temp"=	"Buhera.txt",	# file name of both temperature files
				"prec"=	"Buhera.txt"		# file name of precepitation file (which is sometimes different)
			);	

	# for radn computation only
	inland <- TRUE;		# TRUE (station is in land), FALSE (coastal)

# concatanate everything
	paths <- list("input"=input,"output"=output,"folder"=folder,"file"=file,"inland"=inland);

return(paths);
}

##
 # CONVERT MAIN FUNCTION
 ###############################################################################
 # is the function to be called for any conversion
 ###############################################################################
convert <- function(targetModel)
{
### sources
	source("checkFunctions.r");
	source("bruceFormat.r");
	source("agriParameters.r");
	source("metTransformations.r");

### target models
	if(targetModel=="APSIM" || targetModel=="AP" || targetModel=="apsim" || targetModel=="ap") targetModel <- 1;
	if(targetModel=="AQUACROP" || targetModel=="AQ" || targetModel=="aquacrop" || targetModel=="aq") targetModel <- 2;
	if (!is.numeric(targetModel)){
		print("### ERROR: unknown target model");
		print("### targetModel available so far: 'APSIM' or 'AQUACROP'");
		stop();
	}

###
	print("... read paths ...");
	path_list <- readPaths();

## because so far, no crop model deal with missing data
 # 1: I am only interseted in the time period covered by tmn, tmx and ppt

###
	print("... check unconsistencies ...");
	fileHead <- checkData(path_list);

###
	print("... import data ...");
	data <- importData(path_list,fileHead);

## because so far, no crop model deal with missing data
 # 2: if there is any missing data (that I can spot) I will warn the user

###
	print("... looking for missing data ...");
	checkMissing(data);

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

## then starts the crop model related operations
	switch(targetModel,
		{	                            #################### APSIM #
			source('convertToApsim.r');
			print("... create years and julian days ...");
			data <- createYearJulianDays(data,fileHead);
			print("... compute radiation ...");
			data <- compute_radn(data,fileHead$station,path_list$inland);
			print("... compute tav and amp ...");
			data <- compute_tavNamp(data);
			print("... format and write data into .met file ...");
			formatToMetFile(data,fileHead,path_list);
		},	# APSIM ####################
		{	                            ################# AQUACROP #
			# aquacrop deals with day, 10-days and monthly records
			# so far we deal only with day records
			source('convertToAquacrop.r');
			print("... create years and julian days ...");
			data <- createYearJulianDays(data,fileHead);
			print("... compute ETo ...");
			data <- compute_ETo(data,fileHead$station,path_list$inland);
			print("... format and write data into .TMP, .PLU and .ETo files ...");
			formatToTMPFile(data,fileHead,path_list);
			formatToPLUFile(data,fileHead,path_list);
			formatToEToFile(data,fileHead,path_list);
		}	# AQUACROP #################
	);

print("... conversion completed ...");
}

# SO FAR
# you have to change input/output paths
# folder names
# file names
# inland variable
# -------------------------> in the convertAll.r file
# SEE UP, the following is not automatised yet

## 
 # GC MODELS NAMES (MODEL 1 IS NCEP)
 ###############################################################################
init_gcmNames <- function()
{
gcmNames <- list(	"obs"=	list(	"con"=	"obs",
						"futA"=	NULL,
						"futB"=	NULL),
			"ncep"=	list(	"con"=	"ncep2.1",
						"futA"=	NULL,
						"futB"=	NULL),
			"cccm"=	list(	"con"=	"cccma_cgcm3_1",
						"futA"=	"cccma_cgcm3_1-fa",
						"futB"=	"cccma_cgcm3_1-fb"),
			"cnrm"=	list(	"con"=	"cnrm_cm3",
						"futA"=	"cnrm_cm3-fa",
						"futB"=	"cnrm_cm3-fb"),
			"csiro35"=	list(	"con"=	"csiro_mk3_5",
						"futA"=	"csiro_mk3_5-fa",
						"futB"=	"csiro_mk3_5-fb"),
			"gfdl"=	list(	"con"=	"gfdl_cm2_0",
						"futA"=	"gfdl_cm2_0-fa",
						"futB"=	"gfdl_cm2_0-fb"),
			"giss"=	list(	"con"=	"giss_model_e_r",
						"futA"=	"giss_model_e_r-fa",
						"futB"=	"giss_model_e_r-fb"),
			"ipsl"=	list(	"con"=	"ipsl_cm4",
						"futA"=	"ipsl_cm4-fa",
						"futB"=	"ipsl_cm4-fb"),
			"echo"=	list(	"con"=	"miub_echo_g",
						"futA"=	"miub_echo_g-fa",
						"futB"=	"miub_echo_g-fb"),
			"echam"=	list(	"con"=	"mpi_echam5",
						"futA"=	"mpi_echam5-fa",
						"futB"=	"mpi_echam5-fb"),
			"cgcm"=	list(	"con"=	"mri_cgcm2_3_2a",
						"futA"=	"mri_cgcm2_3_2a-fa",
						"futB"=	"mri_cgcm2_3_2a-fb")
	);
return(gcmNames);
}


 ###############################################################################
 ###############################################################################
 # WAITING FOR MORE ...
 ###############################################################################
 ###############################################################################

#path <- init_paths();
#gcms <- init_gcmNames();

# main loop
#for (g in 1:length(gcms)){
#	for (p in 1:3){
#print(gcms[[g]][[p]]);
#		path2data <- path;
#		path2data$input <- paste(path$input,gcms[[g]][[p]],"/",sep="");
#		path2data$output <- paste(path$output,gcms[[g]][[p]],"/",sep="");
#		convert("APSIM"); # convert takes "APSIM" and soon "AQUACROP"
#		if(g==1 || g==2) break;	# obs and ncep
#	}
#}

