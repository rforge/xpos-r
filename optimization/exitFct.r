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
	#
	#	WRITE DECISION SPACE RESULTS
	#

	#### file name
	decFile <- paste(apsimSpec$path2out,"bestRegions",format(Sys.time(),"_%d-%m-%Y_%H-%M-%S"),".dec",sep="");
	#### file head
	write(paste("## BEST REGIONS ACHIEVED (decision space perspective)",
			paste("## ",date(),sep=""),
			paste("## resolution took : ",format(fullTime)," (",fullSimNo," simulations)",sep=""),
			paste("## no of equally optimal regions : ",besList$itemNo,sep=""),
			paste("## multicriteria rank : ",sum(besList$regEva[[1]]$selCri[1,]),sep=""),
			"##################################################\n",
			sep="\n"
		),decFile,append=FALSE
	);

	## initial input (decS)
	write("initial decision region definition",decFile,append=TRUE);
	write.table(apsimSpec$decS,decFile,col.names=FALSE,row.names=FALSE,append=TRUE,sep="\t",quote=FALSE);
	write("\n",decFile,append=TRUE);

	## equally optimal region achieved (besList$regEva[[r]]$regDef)
	for(r in 1:besList$itemNo){
		write(paste("besList$regEva[[",r,"]]$regDef",sep=""),decFile,append=TRUE);
		write.table(besList$regEva[[r]]$regDef,decFile,col.names=FALSE,row.names=FALSE,append=TRUE,sep="\t",quote=FALSE);
		write("\n",decFile,append=TRUE);
	}

	#
	#	WRITE DECISION VECTOR AND EVALUATIONS
	#

	#### file name
	criFile <- paste(apsimSpec$path2out,"bestRegions",format(Sys.time(),"_%d-%m-%Y_%H-%M-%S"),".cri",sep="");
	#### file head
	write(paste("## CRITERIA ACHIEAVMENT OF BEST REGION'S DECISIONS",
			paste("## ",date(),sep=""),
			paste("## resolution took : ",format(fullTime)," (",fullSimNo," simulations)",sep=""),
			paste("## no of equally optimal regions : ",besList$itemNo,sep=""),
			paste("## multicriteria rank : ",sum(besList$regEva[[1]]$selCri[1,]),sep=""),
			"##################################################\n",
			sep="\n"
		),criFile,append=FALSE
	);

	## initial input (decS)
	varNo <- dim(apsimSpec$decS)[2];
	perNo <- dim(besList$regEva[[1]])[1];
	criNo <- dim(besList$regEva[[1]])[2];
	write("format",criFile,append=TRUE);
	cat(paste("regN:\t",sep=""),file=criFile,append=TRUE,fill=FALSE);
	write.table(array("dec",dim=varNo),criFile,col.names=FALSE,row.names=FALSE,append=TRUE,sep="\t",quote=FALSE,eol="\t");
	cat("->\t",file=criFile,append=TRUE,fill=FALSE);
	write.table(array("cri",dim=c(perNo,criNo)),criFile,col.names=FALSE,row.names=FALSE,append=TRUE,sep="\t",quote=FALSE,eol="\t");

	for (r in 1:besList$itemNo){
		write("\n",criFile,append=TRUE);
		for (d in 1:besList$regEva[[r]]$itemNo){
			cat(paste("reg",r,":\t",sep=""),file=criFile,append=TRUE,fill=FALSE);
			write.table(besList$regEva[[r]]$decDef[d],criFile,col.names=FALSE,row.names=FALSE,append=TRUE,sep="\t",quote=FALSE,eol="\t");
			cat("->\t",file=criFile,append=TRUE,fill=FALSE);
			write.table(besList$regEva[[r]]$decEva,criFile,col.names=FALSE,row.names=FALSE,append=TRUE,sep="\t",quote=FALSE,eol="\t");
		}
	}
}		
