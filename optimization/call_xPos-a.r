##
 # FILE call_xPos-a.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
 # collection of command line to call xPos-a
 ####################################################################

#xPos <- function(
#			mod,			## model to be simulated for evaluation: Deb test functions{1,2,3,4}, apsim{10}
#			partNo,		## No of divided parts per region {2,3}
#			decNo,		## decision number per region {0:automatic, 1...n }
#			perNo,		## perturbation param number (no of random occurences to test uncertainty)
#			simLimit,		## simulation number limit
#			timLimit,		## time limit in sec
#			seeItThrough=NULL,## for graphics {"g","d"}
#			seed=NULL)		## if needed (integer)

source('main.r');
## CALL EXAMPLE
#xPos(mod,partNo,decNo,perNo,simLimit,timLimit,seeItThrough=NULL,seed=NULL)
xPos(10,2,0,6,1000000,216000)
#xPos(10,2,2,2,10,10)
#xPos(3,2,0,6,10000,30,"d")
# 302400 secs = 3.5 days
# 216000 secs = 2.5 days
# 172800 secs = 2 days
# 86400 secs = 24 hours
# 43200 secs = 12 hours
# 18000 secs = 5 hours
