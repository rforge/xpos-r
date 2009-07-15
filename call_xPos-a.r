##
 # FILE call_xPos-a.r
 # AUTHOR olivier crespo
 # DATE july 2009 - july 2009, 10
 # collection of command line to call xPos-a
 ####################################################################

source('main.r');

# decision space
decS <- matrix(c(0,1,0.2,0,1,0.1),3);


## CALL EXAMPLE
## seed <- xPos(1,decS,50,10,10000,10,"y")