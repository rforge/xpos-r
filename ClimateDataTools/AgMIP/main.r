####################################################################
# STARTED 2012-OCT-4
# olivier
####################################################################

### intialiation
graphics.off()
path2in <- "../../../../12_AgMIP/2012-09_SAAworkshop/AgMIP-Accra Climate from Alex/WorldClim/"
#path2in <- "~/Desktop/WorldClim/"


## read *.mat matlab formatted matrices
####################################################################
#library('R.matlab')
#file <- list.files(path2in)
#f<-5
#tmp <- readMat(paste(path2in,file[f],sep=""))
#wdc <- tmp[[1]]
#save(wdc,file=paste(path2in,"wdc_tme.rData",sep=""))
#rm(tmp)
#rm(wdc)

## 
####################################################################
rm(wdc,latAll,lonAll,datAll)
load(paste(path2in,"wdc_lat.rData",sep=""));latAll<-wdc;
load(paste(path2in,"wdc_lon.rData",sep=""));lonAll<-wdc;
load(paste(path2in,"wdc_alt.rData",sep=""));datAll<-wdc;
#load(paste(path2in,"wdc_ppt.rData",sep=""));datAll<-wdc;
#load(paste(path2in,"wdc_tme.rData",sep=""));datAll<-wdc;

# ploting issues
# turn the matrix
####################################################################
mat_rotate45 <- function(mat)
{	new<-array(NA,dim=dim(mat))
	for (i in 1:dim(mat)[1]){
		new[i,]<-mat[((dim(mat)[1])-i+1),]
	}
return<-(t(new))
}

# select subsection of a matrix
# given corTL : top left lat/lon corner
# given corBR : bottom right lat/lon corner
####################################################################
mat_selArea <- function(mDat,mLat,mLon,corTL=c(0,10),corBR=c(-35,52))
{	
	topRow <- length(mLat[mLat[,1]>=corTL[1],1])+1
	botRow <- length(mLat[mLat[,1]>=corBR[1],1])
	lefCol <- length(mLon[1,mLon[1,]<=corTL[2]])+1
	rigCol <- length(mLon[1,mLon[1,]<=corBR[2]])

	new_mDat <- mDat[topRow:botRow,lefCol:rigCol]
	new_mLat <- mLat[topRow:botRow,lefCol:rigCol]
	new_mLon <- mLon[topRow:botRow,lefCol:rigCol]
	
return(list("dat"=new_mDat,"lat"=new_mLat,"lon"=new_mLon))
}

# draw land,countries,river borders
# assumes plot has been created already
# 1: continent borders only
# 2: continent, countries only
# 3: continent, countries and rivers
####################################################################
pMat_borders <- function(opt=2,plotData,sLat,sLon)
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
		dLat <- tLat-land_borders[p,2]
		dLon <- tLon-land_borders[p,1]
		pLat <- length(dLat[dLat>0])+1	# > because southern, i.e matrix starts with positive values
		pLon <- length(dLon[dLon<0])+1	# > because eastern, i.e matrix starts with negative values
		ifelse(abs(dLat[pLat-1])<abs(dLat[pLat]),pLat<-pLat-1,pLat<-pLat)
		ifelse(abs(dLon[pLon-1])<abs(dLon[pLon]),pLon<-pLon-1,pLon<-pLon)
		land_index[p,2] <- length(tLat)-pLat+1
		land_index[p,1] <- pLon	
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

## main
####################################################################
corTL<-c(0,10)
corBR<-c(-35,52)

subMat<-mat_selArea(mDat=datAll,mLat=latAll,mLon=lonAll,corTL=corTL,corBR=corBR)
datSub <- subMat$dat
latSub <- subMat$lat
lonSub <- subMat$lon


plotData<-mat_rotate45(datSub)
x<-1:nrow(plotData)
y<-1:ncol(plotData)
filled.contour(x,y,plotData,asp=1,col=terrain.colors(12))
#contour(x,y,plotData, levels=c(0,500,1000,2000,6000), add=TRUE, col="brown")

# plot
opt<-2;
#plotBorders<-pMat_borders(opt=opt,plotData,latSub,lonSub)
lines(plotBorders$land,		type="l",lty=1,asp=1)
if(opt>1)	lines(plotBorders$country,	type="l",lty=2,asp=1);
if(opt>2)	lines(plotBorders$river,	type="l",lty=3,asp=1)




















