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
	dLat <- mLat-coo[1]
	dLon <- mLon-coo[2]
	pLat <- length(dLat[dLat>0])+1
	pLon <- length(dLon[dLon<0])+1
	ifelse(abs(dLat[pLat-1])<abs(dLat[pLat]),pLat<-pLat-1,pLat<-pLat)
	ifelse(abs(dLon[pLon-1])<abs(dLon[pLon]),pLon<-pLon-1,pLon<-pLon)
	index[1] <- pLat
	index[2] <- pLon

return(index)
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
		if(any(is.na(land_borders[p,])))	next;
		if(land_borders[p,2]>tLat[1] || land_borders[p,2]<tLat[length(tLat)])	next;
		if(land_borders[p,1]<tLon[1] || land_borders[p,1]>tLon[length(tLon)])	next;
		land_index[p,] <- transform_coor2mat(land_borders[p,])
	}

	if(opt>1){	print("country borders ...")
		country_index <- array(NA,dim=dim(country_borders))
		for (p in 1:dim(country_index)[1]){
			if(any(is.na(country_borders[p,])))	next;
			if(country_borders[p,2]>tLat[1] || country_borders[p,2]<tLat[length(tLat)])	next;
			if(country_borders[p,1]<tLon[1] || country_borders[p,1]>tLon[length(tLon)])	next;
			dLat <- tLat-country_borders[p,2]
			dLon <- tLon-country_borders[p,1]
			pLat <- length(dLat[dLat>0])+1	# > because southern, i.e matrix starts with positive values
			pLon <- length(dLon[dLon<0])+1	# > because eastern, i.e matrix starts with negative values
			ifelse(abs(dLat[pLat-1])<abs(dLat[pLat]),pLat<-pLat-1,pLat<-pLat)
			ifelse(abs(dLon[pLon-1])<abs(dLon[pLon]),pLon<-pLon-1,pLon<-pLon)
			country_index[p,2] <- length(tLat)-pLat+1
			country_index[p,1] <- pLon	
		}
	}
	if(opt>2){	print("river borders ...")
		river_index <- array(NA,dim=dim(river_borders))
		for (p in 1:dim(river_index)[1]){
			if(any(is.na(river_borders[p,])))	next;
			if(river_borders[p,2]>tLat[1] || river_borders[p,2]<tLat[length(tLat)])	next;
			if(river_borders[p,1]<tLon[1] || river_borders[p,1]>tLon[length(tLon)])	next;
			dLat <- tLat-river_borders[p,2]
			dLon <- tLon-river_borders[p,1]
			pLat <- length(dLat[dLat>0])+1	# > because southern, i.e matrix starts with positive values
			pLon <- length(dLon[dLon<0])+1	# > because eastern, i.e matrix starts with negative values
			ifelse(abs(dLat[pLat-1])<abs(dLat[pLat]),pLat<-pLat-1,pLat<-pLat)
			ifelse(abs(dLon[pLon-1])<abs(dLon[pLon]),pLon<-pLon-1,pLon<-pLon)
			river_index[p,2] <- length(tLat)-pLat+1
			river_index[p,1] <- pLon	
		}
	}

return(list("land"=land_index,"country"=country_index,"river"=river_index))
}


