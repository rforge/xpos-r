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
#			simLimit,	## simulation number limit
#			timLimit,	## time limit in sec
#		  seeItThrough=NULL,	## for graphics {"g","d"}
#			seed=NULL)	## if needed (integer)

source('main.r');
## CALL EXAMPLE
#xPos(mod,partNo,decNo,perNo,simLimit,timLimit,seeItThrough=NULL,seed=NULL)
xPos(1,2,2,1,1000,600,"d")
# 43200 secs = 12 hours
# 18000 secs = 5 hours
