##
 # FILE apsimInterface.r
 # AUTHOR olivier crespo
 # DATE june 2009 - july 2009, 21
 # read, extract, replace, create sim files
 # according to APSIM
 ####################################################################

##
 # decision initialisation
 ####################################################################
apsim_initDecisions <- function()
{
	decisionList <- list("itemNo"=0,"var_title"=NULL,"var_startDate"=NULL,"var_endDate"=NULL,"var_path2metFile"=NULL);

	## decision 1
	title <- "decisionTest1";
	startDate <- "1/1/1940";
	endDate <- "1/1/1945";
	path2metFile <- "C:\\\\Documents and Settings\\\\CRE256\\\\Desktop\\\\Simu\\\\Tindal.met";

	decisionList$itemNo <- decisionList$itemNo +1;
	decisionList$var_title <- c(decisionList$var_title,title);
	decisionList$var_startDate <- c(decisionList$var_startDate,startDate);
	decisionList$var_endDate <- c(decisionList$var_endDate,endDate);
	decisionList$var_path2metFile <- c(decisionList$var_path2metFile,path2metFile);

	## decision 2	
	title <- "decisionTest2";
	startDate <- "1/1/1950";
	endDate <- "1/1/1955";
	path2metFile <- "C:\\\\Documents and Settings\\\\CRE256\\\\Desktop\\\\Simu\\\\KatherineAERO.met";

	decisionList$itemNo <- decisionList$itemNo +1;
	decisionList$var_title <- c(decisionList$var_title,title);
	decisionList$var_startDate <- c(decisionList$var_startDate,startDate);
	decisionList$var_endDate <- c(decisionList$var_endDate,endDate);
	decisionList$var_path2metFile <- c(decisionList$var_path2metFile,path2metFile);

	## decision 3
	title <- "decisionTest3";
	startDate <- "1/1/1960";
	endDate <- "1/1/1965";
	path2metFile <- "C:\\\\Documents and Settings\\\\CRE256\\\\Desktop\\\\Simu\\\\Berrimah.met";

	decisionList$itemNo <- decisionList$itemNo +1;
	decisionList$var_title <- c(decisionList$var_title,title);
	decisionList$var_startDate <- c(decisionList$var_startDate,startDate);
	decisionList$var_endDate <- c(decisionList$var_endDate,endDate);
	decisionList$var_path2metFile <- c(decisionList$var_path2metFile,path2metFile);

return(decisionList);
}

##
 # change variables
 ####################################################################
apsim_changeVar <- function (decisionList)
{
	for (dec in 1:decisionList$itemNo){
		file.copy(paste(path2Templates,simTemplate,sep=""),paste(path2Outputs,decisionList$var_title[dec],".sim",sep=""),overwrite=TRUE);
		changeVar("var_title",decisionList$var_title[dec],paste(path2Outputs,decisionList$var_title[dec],".sim",sep=""),paste(path2Outputs,decisionList$var_title[dec],".sim",sep=""));
		changeVar("var_startDate",decisionList$var_startDate[dec],paste(path2Outputs,decisionList$var_title[dec],".sim",sep=""),paste(path2Outputs,decisionList$var_title[dec],".sim",sep=""));
		changeVar("var_endDate",decisionList$var_endDate[dec],paste(path2Outputs,decisionList$var_title[dec],".sim",sep=""),paste(path2Outputs,decisionList$var_title[dec],".sim",sep=""));
		changeVar("var_path2metFile",decisionList$var_path2metFile[dec],paste(path2Outputs,decisionList$var_title[dec],".sim",sep=""),paste(path2Outputs,decisionList$var_title[dec],".sim",sep=""));
	}

}

##
 # system evaluation given the variables
 ####################################################################
apsim_simulate <- function(decisionList)
{
	setwd(path2Outputs);
	for (dec in 1:decisionList$itemNo){
		print(paste("start evaluation (Apsim) for decision: ","decision test ",dec,sep=""));
		writeLines(shell(paste("Apsim ",decisionList$var_title[dec],".sim",sep=""), intern=TRUE, wait=TRUE),paste(decisionList$var_title[dec],".sum",sep=""),sep="\n");
		print(paste("end evaluation (Apsim) for decision: ","decision test ",dec,sep=""));
	}
	setwd(path2Origin);
}