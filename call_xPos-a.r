##
 # FILE call_xPos-a.r
 # AUTHOR olivier crespo
 # DATE july 2009 - july 2009, 10
 # collection of command line to call xPos-a
 ####################################################################

source('main.r');

## CALL EXAMPLE
#xPos(mod,partNo,decNo,perNo,simLimit,timLimit,seeItThrough=NULL,seed=NULL)
xPos(10,2,2,1,100,600,"d",1)