##
 # FILE convertMain.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
 # convert bruce's downscaled format into apsim's met file
 #########################################################################
 # functions are made to convert:
 # - 1 station
 # - during 1 continuous period of time 
 # -> i.e. 3 files: tmin,tmax,ppt according to Bruce's dowscaled format
 #########################################################################

##
 # REQUIRED SOURCES
 #########################################################################
source('convertFunctions.r');
source('rwfileOp.r');

## REQUIRED PATH TO DATA
 #########################################################################
 # set paths to data
 # - path to the downscaled forlders tmin, tmax, ppt		## 1 ##
 # - path to where to save the produced .met files		## 2 ##
 # - folder names of those including tmin, tmax and ppt	## 3 ##
 # or read it from "convertFunctions.r" with
 # > path <- init_paths();
 #########################################################################
 # please use R path separator (even on windows), the same as Linux
 # i.e. "/" (and not "\" as windows)
 #########################################################################
path <- 	list(	"input"=			"Z:/Philadelphia/cccma_cgcm3_1/",
			"output"=			"C:/Documents and Settings/crespo/Desktop/ApsimInBergRiver/",
			"data"=list("tmin"=	"tmn/",
					"tmax"=	"tmx/",
					"ppt"=	"ppt/"
			)
		);

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
convert_OneStation4OnePeriod(	path,
				"0021130_A.txt",	# station Name for temperture files
				"00211300.txt",	# station Name for precipitation files
				"test_new",	# my output file name (no extension)
				inland=TRUE		# inland {TRUE,FALSE}
				);


##########################################################################
## FAQ
##########################################################################
#
# ? where are the produced file ?
# wherever you set into data$outputs in init_data() (convertD2A.r)
# 
# ? can I run the process for more than one station/period ?
# No current fct run that, but feel free to use available functions
# to produce your own scripts, you'll find every kind of loop typing
# "?Control" in a R terminal, and already some hand-made facilitating functions
# such as list of station ("init_stationNames"), list of gcm ("init_gcmNames")
# and others in "convertFunctions"
#
# ?
#
#
#
#
#
#
#


