##
 # FILE fileOp.r
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
