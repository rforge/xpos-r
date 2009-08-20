##
 # FILE exitFct.r
 # AUTHOR olivier crespo
 # DATE july 2009 - july 2009, 11
 # various check in order to help locate the error
 ####################################################################

##
 # INPUT ARGUMENT ERROR
 ####################################################################

##
 # MODEL USED VALIDITY
 ####################################################################
is.modelValid <- function(model)
{
	valid <- FALSE;
	switch(model,
		valid <- TRUE,	# 1: Mathematical model based on Deb multicfrontal test fct
		valid <- TRUE,	# 2: Mathematical model based on Deb discoutinuity test fct
		valid <- TRUE,	# 3: Mathematical model based on Deb nonConvexity test fct
		valid <- TRUE,	# 4: Mathematical model based on Deb convexNonConvexity test fct
		,,,,,
		valid <- TRUE	# 10: Apsim
	);
return(valid);
}

##
 # partNo validation
 ####################################################################
is.partNoValid <- function(partNo)
{
	valid <- FALSE;
	switch(partNo,
		{},			# 1: no use
		valid <- TRUE,	# into 2
		valid <- TRUE,	# into 3
		{}			# nothing else exists yet
	);
return(valid);
}

##
 # DECISION SPACE BASICS VALIDITY
 ####################################################################
 # decision space is expected to be a 2 dimensional matrix
 # 3 lines: min bound, max bound, minimal discernable step
 # for as many column as decision variables
 ####################################################################
is.decSpaceValid <- function(space)
{
	valid <- TRUE;
	dimNo <- dim(space)[2];
	line <- dim(space)[1];

	if (line!=3){
		valid <- FALSE;
		return(valid);}
	for (d in 1:dimNo){
		if (space[1,d]>space[2,d]) valid <- FALSE;			# min > max
		if (space[3,d]<0) valid <- FALSE;					# step > 0
		if (space[3,d]>(space[2,d]-space[1,d])) valid <- FALSE; 	# step <= max-min
	}

return(valid);	
}


##
 # CHECK INPUT PARAMETERS VALIDITY
 ####################################################################
checkInputs <- function(mod,partNo,decNo,perNo,simLimit,timLimit,seeItThrough="n",seed=NULL)
{
	stopProcess <- FALSE;

	# input parameter

	# model used for simulation
	if (!is.modelValid(mod)) {stopProcess <- 1;}

	# partNo used for division
	if (!is.partNoValid(partNo)) {stopProcess <- 2;}

	# decision number per region
	if (decNo<1) {stopProcess <- 3;}

	# perturbation parameter number
	if (perNo<1) {stopProcess <- 4;}
	
	# simulation number limit
	if (simLimit<decNo*perNo) {stopProcess <- 5;}
	
	# time limit
	if (timLimit<1) {stopProcess <- 6;}

	# see it through
	# to do
	
	# seed
	# do no know yet

	if(stopProcess){
		print("",quote=FALSE);
		print(	"##########################################",quote=FALSE);
		print(	"# input list expected is as follow",quote=FALSE);
		print(args(xPos),quote=FALSE);
		switch(stopProcess,
			print("mod (simulation model): interger from 1 to 4"),
			print("division partNo is either 2 or 3"),
			print("decNo (decision number per evaluated region): integer > 0"),
			print("perNo (perturbation parameter number): integer > 0"),
			print("simLimit (simulation number upper limit): interger greater than >= decNo * perNo (i.e. 1 region evaluation)"),
			print("timLimit (time limit in sec): interger > 0")
		);
		print(	"##########################################",quote=FALSE);
		stop();	
	}

}