##
 # TO BE REMOVED ....
 ###############################################################################
 # functions are made to convert:
 # - 1 station
 # - during 1 continuous period of time 
 # -> i.e. 3 files: tmin,tmax,ppt according to Bruce's downscaled format
 #########################################################################

##
 # MAIN FUNCTION CALL
 #########################################################################
 # if you are running the script in linux for linux use of the files, that's fine
 # if you are running the script in windows for windows use of the files, that's fine
 # BUT if you are running the script in linux for windows use of the files
 # you'll face some formating problem
 # we thought about fixing that, but if you intend to use it on windows,
 # I guess you can afford to run the script on windows ...
 #########################################################################
 #
 # NONE OF THESE FUNCTIONS DEAL WITH MISSING VALUES
 # 
 #########################################################################

convert <- function (cropModel)
{

source('convertFunctions.r');
source('rwfileOp.r');

if(cropModel=="APSIM" || cropModel=="apsim" || cropModel=="ap" || cropModel=="AP"){
## APSIM format
 # please use R path separator (even on windows), the same as Linux
 # i.e. "/" (and not "\" as windows)
 #########################################################################
apsim_convert_OneStation4OnePeriod(	path=list("input"=	"/home/csag/crespo/Desktop/AquaCrop/",			# where to read the input data
						"output"=	"/home/csag/crespo/Desktop/AquaCrop/ApsimFormat/",	# where to write the output data
						"data"=	list(	"tmin"=	"tmin/",	# folder name for minimal temperatures
								"tmax"=	"tmax/",	# folder name for maximal temperatures
								"ppt"=	"ppt/")),	# folder name for precipitation
					"0725756AW.txt",	# station Name for temperture files
					"0725756AW.txt",	# station Name for precipitation files
					"0725756AW",		# my output file name (no extension)
					inland=TRUE		# inland {TRUE,FALSE}
					);
}

if(cropModel=="AQUACROP" || cropModel=="aquacrop" || cropModel=="aq" || cropModel=="AQ"){
## AQUACROP format
 # please use R path separator (even on windows), the same as Linux
 # i.e. "/" (and not "\" as windows)
 #########################################################################
aquacrop_convert_OneStation4OnePeriod(	path=list(	"input"=	"/home/csag/crespo/Desktop/AquaCrop/",			# where to read the input data
						"output"=	"/home/csag/crespo/Desktop/AquaCrop/AquaCropFormat/",	# where to write the output data
						"data"=	list(	"tmin"=	"tmin/",	# folder name for minimal temperatures
								"tmax"=	"tmax/",	# folder name for maximal temperatures
								"ppt"=	"ppt/")),	# folder name for precipitation
					"XAI-XAI.txt",	# station Name for temperture files
					"XAI-XAI.txt",	# station Name for precipitation files
					"XAI-XAI",	# my output file name (no extension)
					inland=FALSE	# inland {TRUE,FALSE}
					);
}
}

##########################################################################
## FAQ
##########################################################################
#
# ? where are the produced file ?
# whatever you wrote as "output"
# 
# ? can I run the process for more than one station/period ?
# see convertMainLoop.r file
#



