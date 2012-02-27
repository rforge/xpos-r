##
 # FILE call_xPos-a.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
 # collection of command line to call xPos-a
 ####################################################################

#xPos <- function(
#			mod,		## model to be simulated for evaluation: Deb test functions{1,2,3,4}, apsim{10}, apsimInKatherine{11}, apsimInUFS{12}
#			partNo,		## No of divided parts per region {2,3}
#			decNo,		## decision number per region {0:automatic, 1...n }
#			perNo,		## perturbation param number (no of random occurences to test uncertainty)
#			simLimit,		## simulation number limit
#			timLimit,		## time limit in sec
#			seeItThrough=NULL,## for graphics {"g","d"}
#			log=FALSE,  	## if TRUE, then create a log file with basic info
#			seed=NULL)		## if needed (integer)

source('main.r');
## CALL EXAMPLE
#xPos(mod,partNo,decNo,perNo,simLimit,timLimit,seeItThrough=NULL,seed=NULL)
xPos(11,2,0,8,1000000,1000000,log=FALSE,seed=NULL)
#xPos(11,2,2,4,100,300,log=FALSE,seed=NULL)
# 604800 secs = 1 week
# 302400 secs = 3.5 days
# 216000 secs = 2.5 days
# 172800 secs = 2 days
# 86400 secs = 24 hours
# 43200 secs = 12 hours
# 18000 secs = 5 hours
# 10800 secs = 3 hours
# 900 secs = 15 min
