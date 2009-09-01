##
 # FILE convertDscl2Apsim
 # AUTHOR olivier crespo
 #########################################################################

source('convertD2A.r');
source('rwfileOp.r');

## read user settings
data <- init_data();
stationNames <- init_stationName();
gcmNames <- init_gcmNames();

## loop initialization
station <- stationNames[1];

convert_OneStation4OnePeriod(data,station);
