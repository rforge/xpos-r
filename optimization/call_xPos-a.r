##
 # FILE call_xPos-a.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
 # collection of command line to call xPos-a
 ####################################################################

source('main.r');

## CALL EXAMPLE
#xPos(mod,partNo,decNo,perNo,simLimit,timLimit,seeItThrough=NULL,seed=NULL)
xPos(10,2,0,6,100000,1500)
# 43200 secs = 12 hours
