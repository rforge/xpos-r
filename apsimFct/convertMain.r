##
 # FILE convertMain.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
 # convert bruce's downscaled format into apsim's met file
 #########################################################################
 # this is not working by itself,
 # but explaining how to use the functions in convertD2A.r
 #########################################################################

##
 # - I - THIS IS REQUIRED
 #########################################################################
 # required sources
 #########################################################################
source('convertD2A.r');
source('rwfileOp.r');

##
 # personal settings
 #########################################################################
 # functions are made to convert:
 # - 1 station
 # - during 1 continuous period of time 
 # -> i.e. 3 files: tmin,tmax,ppt at Bruce's dowscaled format
 #########################################################################

## - II - THIS IS REQUIRED
 #########################################################################
 # go to convertD2A.r and set "init_data" to your preferences
 # i.e.
 # - path to the downscaled forlders tmin, tmax. ppt
 # - path to where to save the produced .met files
 # - folder names of those includeing tmin, tmax and ppt
 # then you can read this data with
 #########################################################################
data <- init_data();

## - III - THIS IS NOT REQUIRED
 # but can help
 # "init_stationName" allows to list many stations
 # it produces a vector of dim = station No
 #########################################################################
stationNames <- init_stationNames();
stationNo <- dim(stationNames);

## - IV - THIS IS NOT REQUIRED
 # but can help
 # "init_gcmNames" allows to various gcm and period or time
 # it produces a list of GCMs, for wich you specify a folder name per period
 #########################################################################
gcmNames <- init_gcmNames();

## - V - THIS IS REQUIRED
 # produce one station (full file name)
 #########################################################################
station <- stationNames[1];

################################################################################
## when you've done everything above,
## MAIN FUNCTION CALL
################################################################################
convert_OneStation4OnePeriod(data,station);

################################################################################
## FAQ
################################################################################
#
# ? where are the produced file ?
# wherever you set into data$outputs in init_data() (convertD2A.r)
# 
# ? can I run the process for more than one station/period ?
# No current fct run that, but feel free to use available functions
# to produce your own scripts, you find every kind of loop typing
# "?Control" in a R terminal
#
# ? 

