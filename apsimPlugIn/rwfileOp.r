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
##
 # collect index related to a location border set
 ####################################################################
 # according to "shapefiles" package
 ####################################################################
shpIndex.inLonLat <- function(lonMin,lonMax,latMin,latMax,shapefile)
{	
	indices <- NULL;
	for(i in 1:length(shapefile$shp$shp)){
		if(shapefile$shp$shp[[i]]$box[1]>lonMin && shapefile$shp$shp[[i]]$box[3]<lonMax){
		if(shapefile$shp$shp[[i]]$box[2]>latMin && shapefile$shp$shp[[i]]$box[4]<latMax){
			indices <- c(indices,i);
		}}
	}
	
return(as.array(indices));
}
##
 # print values for one location index
 ####################################################################
 # according to "shapefiles" package
 ####################################################################
dbfValues.atIndex <- function(ind,shapefile)
{	
	values <- NULL;
	for(i in 1:length(shapefile$dbf$dbf)){
		values <- c(values,shapefile$dbf$dbf[[i]][ind]);
	}
	
return(as.array(values));
}


