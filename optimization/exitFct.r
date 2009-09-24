##
 # FILE exitFct.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
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
		{},{},{},{},{},
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
	decNo <- dim(space)[2];
	line <- dim(space)[1];

	if (line!=3){
		valid <- FALSE;
		return(valid);
	}
	for (d in 1:decNo){
		if (space[1,d]>space[2,d]) valid <- FALSE;			# min > max
		if (space[3,d]<0) valid <- FALSE;					# step > 0
		if (space[3,d]>(space[2,d]-space[1,d])) valid <- FALSE; 	# step <= max-min
	}

return(valid);	
}


##
 # CHECK INPUT PARAMETERS VALIDITY
 ####################################################################
checkInputs <- function(mod,partNo,decNo,perNo,simLimit,timLimit,seeItThrough,seed=NULL)
{
	stopProcess <- FALSE;

	# input parameter

	# model used for simulation
	if (!is.modelValid(mod)) {stopProcess <- 1;}

	# partNo used for division
	if (!is.partNoValid(partNo)) {stopProcess <- 2;}

	# decision number per region
	if (decNo<0) {stopProcess <- 3;}

	# perturbation parameter number
	if (perNo<1) {stopProcess <- 4;}
	
	# simulation number limit
	if (simLimit<decNo*perNo) {stopProcess <- 5;}
	
	# time limit
	if (timLimit<1) {stopProcess <- 6;}

	# see it through
	if (seeItThrough!="g" && seeItThrough!="d" && !is.null(seeItThrough) ) {stopProcess <- 7;}
	
	# seed
	# do no know yet

	if(stopProcess){
		print("",quote=FALSE);
		print(	"##########################################",quote=FALSE);
		#print(	"# input list expected is as follow",quote=FALSE);
		#print(args(xPos),quote=FALSE);
		switch(stopProcess,
			print("# mod (simulation model): interger from 1 to 4 or 10(Apsim)",quote=FALSE),
			print("# division partNo is either 2 or 3",quote=FALSE),
			print("# decNo (decision number per evaluated region): integer >= 0 (0 for auto)",quote=FALSE),
			print("# perNo (perturbation parameter number): integer > 0",quote=FALSE),
			print("# simLimit (simulation number upper limit): interger greater than >= decNo * perNo (i.e. 1 region evaluation)",quote=FALSE),
			print("# timLimit (time limit in sec): interger > 0",quote=FALSE),
			print("# seeItThrough value invalid: NULL(no graph) or \"g\"(graph) or \"d\"(debug)",quote=FALSE)
		);
		print(	"##########################################",quote=FALSE);
		stop();	
	}

}

##
 # LAST ACTION: WRITE THE LIST OF THE BEST REGIONS FOUND
 ####################################################################
write.bestList <- function(besList,apsimSpec,fullSimNo,fullTime)
{
	if(!is.null(apsimSpec)){
	
		#
		#	WRITE DECISION SPACE RESULTS
		#
		
		#### file name
		decFile <- paste(apsimSpec$path2out,"bestRegions",format(Sys.time(),"_%d-%m-%Y_%H-%M-%S"),".dec",sep="");
		#### file head
		write(paste("## BEST REGIONS ACHIEVED (decision space perspective)",
				paste("## ",date(),sep=""),
				paste("## no of regions : ",besList$itemNo,sep=""),
				paste("## multicriteria rank : ",sum(besList$regEva[[1]]$selCri[1,]),sep=""),
				"##################################################",
				sep="\n"
			),decFile,append=FALSE
		);

		## decision initial input (decS)
		## equally optimal region achieved (besList$regEva[[r]]$regDef)

		## format template
		r = "n"
		write(paste("\n## REGION ",r,sep=","),decFile,append=TRUE);
		write(paste("decision space definition",sep=","),decFile,append=TRUE);
		write(paste("minDec1","minDec2","minDec...\n",
				"maxDec1","maxDec2","maxDec...\n",
				"stepDec1","stepDec2","stepDec...\n",
				sep=","),decFile,append=TRUE);
		write(paste("for all decision simulated in this region",sep=","),decFile,append=TRUE);
		write(paste("decision vector",sep=","),decFile,append=TRUE);
		write(paste("dec1","dec2","dec...",sep=","),decFile,append=TRUE);
		write(paste("evaluation vector",sep=","),decFile,append=TRUE);
		write(paste("crit1","crit2","crit...",sep=","),decFile,append=TRUE);
			
		for(r in 1:besList$itemNo){
			write(paste("\n## REGION ",r,sep=","),decFile,append=TRUE);
			write(paste("besList$regEva[[",r,"]]$regDef",sep=","),decFile,append=TRUE);
			write.table(besList$regEva[[r]]$regDef,decFile,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",",quote=FALSE);
			for (d in 1:besList$regEva[[r]]$itemNo){
				write(paste("besList$regEva[[",r,"]]$decDef[[",d,"]]",sep=","),decFile,append=TRUE);
				write.table(t(besList$regEva[[r]]$decDef[[d]]),decFile,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",",quote=FALSE);
				write(paste("besList$regEva[[",r,"]]$decEva[[",d,"]]",sep=","),decFile,append=TRUE);
				write.table(besList$regEva[[r]]$decEva[[d]],decFile,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",",quote=FALSE);
			}
		}
	}		
}