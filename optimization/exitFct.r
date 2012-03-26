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
		valid <- TRUE,	# 10: Apsim
		valid <- TRUE,	# 11: ApsimInKatherine
		valid <- TRUE,	# 12: ApsimInUFS
		valid <- TRUE	# 13: ApsimInWC
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
 # CSV format for excel
 ####################################################################
 # 	  		  DECISIONS				||	OUTCOMES
 # 	    Dec 1		|	   Dec 2 ...	||	
 # minReg maxReg simDec	| minReg maxReg simDec	|| Cri1 Cri2 ...
 ####################################################################
write.bestList <- function(besList,apsimSpec,fullSimNo,fullTime,totalNoOfSimu)
{
	#
	#	WRITE all in a *.csv
	#
	#### file name
	csvFile <- paste(apsimSpec$path2out,"resultsAchievedThe",format(Sys.time(),"_%d-%m-%Y_%H-%M-%S"),".csv",sep="");
	#### file head
	write(	paste(	paste("file name",csvFile,sep=","),
					paste("creation date",date(),sep=","),
					paste("time spent",format(fullTime),sep=","),
					paste("simulation No",fullSimNo,sep=","),
					paste("decomp No",totalNoOfSimu,sep=","),
					paste("optimal No",besList$itemNo,sep=","),
					paste("mc rank",sum(besList$regEva[[1]]$selCri[1,]),sep=","),
					paste("per No",dim(besList$regEva[[1]]$decEva[[1]])[1],sep=","),
					"",
					paste("initialy","explored",sep=","),
					sep="\n")
			,csvFile,append=FALSE
	);
	#### col names
	decNo <- dim(apsimSpec$decS)[2];
	criNo <- dim(apsimSpec$criS)[2];
	colNames <- array("",dim=c(2,(3*decNo+5*criNo)));
	col <- 1;
	for(d in 1:decNo){
		colNames[1,col] <- apsimSpec$decS[1,d]; 	
		colNames[1,col+1] <- apsimSpec$decS[2,d]; 	
		colNames[2,col:(col+2)] <- paste("decision",d,sep="");
		col <- col+3;
	}
	for(c in 1:criNo){
		colNames[2,col:(col+4)] <- paste("criterion",c,sep="");
		col <- col+5;
	}
	write.table(colNames,csvFile,col.names=F,row.names=F,quote=F,sep=",",append=TRUE);
	colNames <- array(NA,dim=c(1,(3*decNo+5*criNo)));
	col <- 1;
	for(d in 1:decNo){
		colNames[1,col] <- "min"
		colNames[1,col+1] <- "max"
		colNames[1,col+2] <- "sim"
		col <- col+3;
	}
	for(c in 1:criNo){
		colNames[1,col] <- "min"
		colNames[1,col+1] <- "max"
		colNames[1,col+2] <- "mean"
		colNames[1,col+3] <- "median"
		colNames[1,col+4] <- "stdev"
		col <- col+5;
	}
	write.table(colNames,csvFile,col.names=F,row.names=F,quote=F,sep=",",append=TRUE);

	#### equally optimal region achieved (besList$regEva[[r]]$regDef)
	for(r in 1:besList$itemNo){
		for(s in 1:besList$regEva[[r]]$itemNo){
			oneDec <- array(NA,dim=c(1,(3*decNo+5*criNo)));
			for (d in 1:decNo){
				# reg definition
				oneDec[1,(d-1)*3+1] <- besList$regEva[[r]]$regDef[1,d];
				oneDec[1,(d-1)*3+2] <- besList$regEva[[r]]$regDef[2,d];
				# dec definition
				oneDec[1,(d-1)*3+3] <- besList$regEva[[r]]$decDef[[s]][d];
			}
			for (c in 1:criNo){
				oneDec[1,(decNo*3+(c-1)*5+1)] <- min(besList$regEva[[r]]$decEva[[s]][,c]);
				oneDec[1,(decNo*3+(c-1)*5+2)] <- max(besList$regEva[[r]]$decEva[[s]][,c]);
				oneDec[1,(decNo*3+(c-1)*5+3)] <- mean(besList$regEva[[r]]$decEva[[s]][,c]);
				oneDec[1,(decNo*3+(c-1)*5+4)] <- median(besList$regEva[[r]]$decEva[[s]][,c]);
				oneDec[1,(decNo*3+(c-1)*5+5)] <- sd(besList$regEva[[r]]$decEva[[s]][,c]);
			}
			write.table(oneDec,csvFile,col.names=F,row.names=F,quote=F,sep=",",append=TRUE);
		}
	}

browser();


#### original version
	#
	#	WRITE DECISION SPACE RESULTS
	#

	#### file name
	decFile <- paste(apsimSpec$path2out,"bestRegions",format(Sys.time(),"_%d-%m-%Y_%H-%M-%S"),".dec",sep="");
	#### file head
	write(paste("## BEST REGIONS ACHIEVED (decision space perspective)",
			paste("## ",date(),sep=""),
			paste("## resolution took : ",format(fullTime)," (",fullSimNo," simulations)",sep=""),
			paste("## no of equally optimal regions : ",besList$itemNo," out of ",totalNoOfSimu," explored",sep=""),
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
			paste("## no of equally optimal regions : ",besList$itemNo," out of ",totalNoOfSimu," explored",sep=""),
			paste("## multicriteria rank : ",sum(besList$regEva[[1]]$selCri[1,]),sep=""),
			"##################################################",
			sep="\n"
		),criFile,append=FALSE
	);

	## initial input (decS)
	varNo <- dim(apsimSpec$decS)[2];
	perNo <- dim(besList$regEva[[1]]$decEva[[1]])[1];
	criNo <- dim(besList$regEva[[1]]$decEva[[1]])[2];
	write("format",criFile,append=TRUE);
	cat(paste("regN:\t",sep=""),file=criFile,append=TRUE,fill=FALSE);
	write.table(array("dec",dim=varNo),criFile,col.names=FALSE,row.names=FALSE,append=TRUE,sep="\t",quote=FALSE,eol="\t");
	cat("->\t",file=criFile,append=TRUE,fill=FALSE);
	write.table(array("cri",dim=criNo),criFile,col.names=FALSE,row.names=FALSE,append=TRUE,sep="\t",quote=FALSE,eol="\t");

	for (r in 1:besList$itemNo){
		write("\n",criFile,append=TRUE);
		for (d in 1:besList$regEva[[r]]$itemNo){
			for (p in 1:perNo){
				cat(paste("\nreg",r,":\t",sep=""),file=criFile,append=TRUE,fill=FALSE);
				write.table(besList$regEva[[r]]$decDef[d],criFile,col.names=FALSE,row.names=FALSE,append=TRUE,sep="\t",quote=FALSE,eol="\t");
				cat("->\t",file=criFile,append=TRUE,fill=FALSE);
				write.table(besList$regEva[[r]]$decEva[[d]][p,],criFile,col.names=FALSE,row.names=FALSE,append=TRUE,sep="\t",quote=FALSE,eol="\t");
			}
		}
	}
}		
