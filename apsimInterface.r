##
 # FILE apsimInterface.r
 # AUTHOR olivier crespo
 # DATE june 2009 - july 2009
 # read, extract, replace, create sim files
 # according to APSIM
 ####################################################################

##
 # newFile with newVar instead of oldVar in oldFile
 ####################################################################
changeVar <- function(oldVar,newVar,oldFile,newFile)
{	
	tempFile <- readLines(oldFile,n=-1,warn=FALSE);
	tempFile <- sub(oldVar, newVar, tempFile);
	writeLines(tempFile,newFile,sep="\n");
}

##
 # list variable names to look for
 ####################################################################
init_variables <- function(varNo)
{
	variableList <- array("empty",dim=c(varNo,3));

	# var_id, name in .sim, component name="xxx" in .sim
	variableList[1,1] <- "var_title";		variableList[1,2] <- "title";		variableList[1,3] <- "";
	variableList[2,1] <- "var_startDate";	variableList[2,2] <- "start_date"; 	variableList[2,3] <- "clock";
	variableList[3,1] <- "var_endDate";		variableList[3,2] <- "end_date"; 	variableList[3,3] <- "clock";
	variableList[4,1] <- "var_path2metFile";	variableList[4,2] <- "filename"; 	variableList[4,3] <- "met";

	return(variableList);
}