##
 # FILE matPlot.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/ClimateDataTools/AgMIP
 #####################################################################
 # Here we assume we have a data map stored as a matrix
 # where left to right columns are west to east longitudes
 # and top to bottom rows are north to south latitudes
 # I'LL CALL THIS MATRIX THE top-north left-west matrix
 #####################################################################

# WorldClim Data was last in
 path2lat <- "/local/users-a/crespo/wine_shared/12_AgMIP/2012-09_SAAworkshop/AgMIP-AccraClimateFromAlex/WorldClim/wdc_lat.rData"
 path2lon <- "/local/users-a/crespo/wine_shared/12_AgMIP/2012-09_SAAworkshop/AgMIP-AccraClimateFromAlex/WorldClim/wdc_lon.rData"
 path2alt <- "/local/users-a/crespo/wine_shared/12_AgMIP/2012-09_SAAworkshop/AgMIP-AccraClimateFromAlex/WorldClim/wdc_alt.rData"
 path2ppt <- "/local/users-a/crespo/wine_shared/12_AgMIP/2012-09_SAAworkshop/AgMIP-AccraClimateFromAlex/WorldClim/wdc_ppt.rData"
 path2tme <- "/local/users-a/crespo/wine_shared/12_AgMIP/2012-09_SAAworkshop/AgMIP-AccraClimateFromAlex/WorldClim/wdc_tme.rData"
# get wdc
# it's big (especially ppt and tme), cut it fast

####### DEFINE SUBSET
# corTL<-c(40,-30);corBR<-c(-35,52)			# 137.158.102.43:/exports/terra/data/staging/local/users-aAfrica
# corTL<-c(0,10);corBR<-c(-35,52)			# southern Africa
# corTL<-c(-22,15.9);corBR<-c(-35,34)			# South Africa
# corTL<-c(-28.83,25.66667);corBR<-c(-29.50,27.00)	# fast-track Bloemfontein

## some points
# c(-29.10901,26.18730)	# UFS
# c(-29.1,26.3)		# CSAG station 
# c(-29.12,26.18)	# MERRA station


# draw latitudes
# lat is a vector (can be a value)
# latMat is the latitude matrix (not rotated)
map_drawLat <- function(lat,latMat)
{
	latMat_R <- mat_rotate45(latMat)
	for (l in 1:length(lat)){
		c<-which(abs(latMat_R[1,]-(lat[l]))==min(abs(latMat_R[1,]-(lat[l]))))/dim(latMat_R)[2]
		abline(h=c,lt=2)
#		mtext(lat[l],side=4,line=0,at=c(dim(latMat)[1],c))
	}
rm(latMat_R,c)
}

# draw longitudes
# lon is a vector (can be a value)
# lonMat is the longitude matrix (not rotated)
map_drawLon <- function(lon,lonMat)
{
	lonMat_R <- mat_rotate45(lonMat)
	for (l in 1:length(lon)){
		r<-which(abs(lonMat_R[,1]-(lon[l]))==min(abs(lonMat_R[,1]-(lon[l]))))/dim(lonMat_R)[1]
		abline(v=r,lt=2)
#		mtext(lon[l],side=3,line=0,at=c(r,dim(lonMat)[2]))
	}
rm(lonMat_R,r)
}

# draw points
# poi is of dim=c(n,2) for lat and long
# latM and lonM are the latitude and longitude matrices (not rotated)
map_drawPoi <- function(poi,latM,lonM,lab=NULL)
{
	latMat_R <- mat_rotate45(latM)
	lonMat_R <- mat_rotate45(lonM)
	for (p in 1:length(poi)){
		c<-which(abs(latMat_R[1,]-(poi[p,1]))==min(abs(latMat_R[1,]-(poi[p,1]))))/dim(latMat_R)[2]
		r<-which(abs(lonMat_R[,1]-(poi[p,2]))==min(abs(lonMat_R[,1]-(poi[p,2]))))/dim(lonMat_R)[1]
		points(r,c)
		text(r,c+0.03,labels=lab)
	}
rm(latM_R,lonM_R,p,pos)
}


# draw land,countries,river borders
# assumes plot has been created already
# 1: continent borders only
# 2: continent, countries only
# 3: continent, countries and rivers
####################################################################
# adapt and check with transform above
####################################################################
map_borders <- function(opt=2,sLat,sLon)
{
	# read the data
	path2data <- "~/User-A/Help/R/AfricaDataMap/"
	land_borders <- read.table(paste(path2data,"my-africa-cil.txt",sep=""));
	if(opt>1)	country_borders <- read.table(paste(path2data,"my-africa-bdy.txt",sep=""))
	if(opt>2)	river_borders <- read.table(paste(path2data,"my-africa-riv.txt",sep=""));

#
# under construction
#
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


