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
