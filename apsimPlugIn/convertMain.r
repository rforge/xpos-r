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
path <- list("input"=			"../../Test/GCM2/",	## 1 ##
		"output"=		"../../Test/Outputs/",## 2 ##
		"data"=list(	"tmin"=	"tmn/",				## 3 ##
				"tmax"=	"tmx/",				## 3 ##
				"ppt"=	"ppt/"				## 3 ##
			)
		);

##
 # MAIN FUNCTION CALL
 #########################################################################
convert_OneStation4OnePeriod(path,"0010425AW.txt");

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


