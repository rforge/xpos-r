##
 # FILE mapsTools.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/ClimateDataTools/AgMIP
 #####################################################################
 # Here we assume we have a data map stored as a matrix
 # where left to right columns are west to east longitudes
 # and top to bottom rows are north to south latitudes
 # I'LL CALL THIS MATRIX THE top-north left-west matrix
 #####################################################################

# transform lat,lon coordinates into the closest suitable matrix indices
####################################################################
# INPUTS	mLat top-north left-west Latitudes matrix
# 		mLon top-north left-west Longitudes matrix
#		coo = c(latitude,longitude)
# RETURNS	index = c(latitude,longitude) in a top-north left-west matrix
####################################################################
map_latLon2matIndex <- function(coo,mLat,mLon)
{
# did not test it, watch out and swaped lat lon in input adn output
	index<-c(NA,NA)
	dLat <- mLat[,1]-coo[1]
	dLon <- mLon[1,]-coo[2]
	pLat <- length(dLat[dLat>0])+1
	pLon <- length(dLon[dLon<0])+1
	ifelse(abs(dLat[pLat-1])<abs(dLat[pLat]),pLat<-pLat-1,pLat<-pLat)
	ifelse(abs(dLon[pLon-1])<abs(dLon[pLon]),pLon<-pLon-1,pLon<-pLon)
	index[1] <- pLat
	index[2] <- pLon
return(index)
}
poi_latLon2pointIndex <- function(coo,mLat,mLon)
{
	coo<-map_latLon2matIndex(coo,mLat,mLon);
	coo[1] <- dim(mLat)[1]-coo[1];
	tmp<-coo[1]; coo[1]<-coo[2]; coo[2]<-tmp;
return(t(coo));
}

# draw land,countries,river borders
# assumes plot has been created already
# 1: continent borders only
# 2: continent, countries only
# 3: continent, countries and rivers
####################################################################
# adapt and check with transform above
####################################################################
map_borders <- function(opt=2,plotData,sLat,sLon)
{
	# read the data
	path2data <- "~/User-A/Help/R/AfricaDataMap/"
	land_borders <- read.table(paste(path2data,"my-africa-cil.txt",sep=""));
	if(opt>1)	country_borders <- read.table(paste(path2data,"my-africa-bdy.txt",sep=""))
	if(opt>2)	river_borders <- read.table(paste(path2data,"my-africa-riv.txt",sep=""));

	# coutry_borders is 5 decimals long, so ...
	tLat <- round(sLat[,1],digits=5)
	tLon <- round(sLon[1,],digits=5)
	# re-index it
print("land borders ...")
	land_index <- array(NA,dim=dim(land_borders))
	for (p in 1:dim(land_index)[1]){
print(paste(p,dim(land_index)[1],sep="/"));
		if(any(is.na(land_borders[p,])))	next;
		if(land_borders[p,2]>tLat[1] || land_borders[p,2]<tLat[length(tLat)])	next;
		if(land_borders[p,1]<tLon[1] || land_borders[p,1]>tLon[length(tLon)])	next;
		land_index[p,] <- poi_latLon2pointIndex(c(land_borders[p,2],land_borders[p,1]),sLat,sLon)
	}

	country_index <- NULL;
	if(opt>1){	print("country borders ...")
		country_index <- array(NA,dim=dim(country_borders))
		for (p in 1:dim(country_index)[1]){
print(paste(p,dim(land_index)[1],sep="/"));
			if(any(is.na(country_borders[p,])))	next;
			if(country_borders[p,2]>tLat[1] || country_borders[p,2]<tLat[length(tLat)])	next;
			if(country_borders[p,1]<tLon[1] || country_borders[p,1]>tLon[length(tLon)])	next;
			country_index[p,] <- poi_latLon2pointIndex(c(country_borders[p,2],country_borders[p,1]),sLat,sLon)
		}
	}

	river_index <- NULL;
	if(opt>2){	print("river borders ...")
		river_index <- array(NA,dim=dim(river_borders))
		for (p in 1:dim(river_index)[1]){
print(paste(p,dim(land_index)[1],sep="/"));
			if(any(is.na(river_borders[p,])))	next;
			if(river_borders[p,2]>tLat[1] || river_borders[p,2]<tLat[length(tLat)])	next;
			if(river_borders[p,1]<tLon[1] || river_borders[p,1]>tLon[length(tLon)])	next;
			river_index[p,] <- poi_latLon2pointIndex(c(river_borders[p,2],river_borders[p,1]),sLat,sLon)
		}
	}

return(list("land"=land_index,"country"=country_index,"river"=river_index))
}


