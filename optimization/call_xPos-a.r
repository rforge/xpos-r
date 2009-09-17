##
 # FILE call_xPos-a.r
 # AUTHOR olivier crespo
 # DATE july 2009 - july 2009, 10
 # collection of command line to call xPos-a
 ####################################################################

source('main.r');

## CALL EXAMPLE
#xPos(mod,partNo,decNo,perNo,simLimit,timLimit,seeItThrough=NULL,seed=NULL)
xPos(1,2,0,1,1000000,3600,"d")