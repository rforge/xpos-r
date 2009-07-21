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
