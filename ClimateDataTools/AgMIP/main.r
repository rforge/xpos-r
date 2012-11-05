##
 # FILE main.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/ClimateDataTools/AgMIP
 ####################################################################

graphics.off()
source('matrixTools.r')
source('mapsTools.r')

####### INIT
#path2in <- "/home/crespo/Desktop/Link to WinShared/12_AgMIP/2012-09_SAAworkshop/AgMIP-Accra Climate from Alex/WorldClim/"
path2in <- "/local/users-a/crespo/wine_shared/12_AgMIP/2012-09_SAAworkshop/AgMIP-Accra Climate from Alex/WorldClim/"
#path2in <- "~/Desktop/WorldClim/"

####### LOAD DATA
#mat_translate(path2in)
load(paste(path2in,"wdc_lat.rData",sep=""));latAll<-wdc;	# (lat,repeat)
load(paste(path2in,"wdc_lon.rData",sep=""));lonAll<-wdc;	# (repeat,lon)
load(paste(path2in,"wdc_alt.rData",sep=""));altAll<-wdc;	# altitude = (lat,lon)
load(paste(path2in,"wdc_ppt.rData",sep=""));pptAll<-wdc;	# total(?) precipitation = (lat,lon,month)
load(paste(path2in,"wdc_tme.rData",sep=""));tmeAll<-wdc;	# mean temperature = (lat,lon,month)

####### DEFINE SUBSET
#corTL<-c(40,-30);corBR<-c(-35,52)		# 137.158.102.43:/exports/terra/data/staging/local/users-aAfrica
corTL<-c(0,10);corBR<-c(-35,52)			# southern Africa
#corTL<-c(-22,15.9);corBR<-c(-35,34)		# South Africa
#corTL<-c(-28.80,25.00);corBR<-c(-29.50,27.00)	# fast-track Bloemfontein

####### REDUCE FULL MATRICES TO SUBmatrices
corners <- list('TL'=NULL,'BR'=NULL)
corners$TL <- map_latLon2matIndex(corTL,mLat=latAll,mLon=lonAll)
corners$BR <- map_latLon2matIndex(corBR,mLat=latAll,mLon=lonAll)
altSub <- altAll[corners$TL[1]:corners$BR[1],corners$TL[2]:corners$BR[2]]
latSub <- latAll[corners$TL[1]:corners$BR[1],corners$TL[2]:corners$BR[2]]
lonSub <- lonAll[corners$TL[1]:corners$BR[1],corners$TL[2]:corners$BR[2]]
pptSub <- pptAll[corners$TL[1]:corners$BR[1],corners$TL[2]:corners$BR[2],]
tmeSub <- tmeAll[corners$TL[1]:corners$BR[1],corners$TL[2]:corners$BR[2],]
rm(wdc,latAll,lonAll,altAll,pptAll,tmeAll)

####### PLOT param
is.temp<-T; # F is rainfall
rNo <- 4
cNo <- 3
colNo <- 100
####### PLOT in 4x3 matrix
altPlot<-mat_rotate45(altSub)
x<-1:nrow(altPlot)
y<-1:ncol(altPlot)
ifelse(is.temp,limits<-range(tmeSub,na.rm=T),limits<-range(pptSub,na.rm=T))
limits[1]<-10*floor(limits[1]/10)
limits[2]<-10*ceiling(limits[2]/10)
par(mfrow=c(rNo,cNo))

for (r in 1:rNo){
	for (c in 1:cNo){
		ifelse(	is.temp,
			datSub<-tmeSub[,,(r*3+c-3)],
			datSub<-pptSub[,,(r*3+c-3)]
		)
		datPlot<-mat_rotate45(datSub)

		par(mfg=c(r,c))
		par(mar=c(0,1,0,0))

#		filled.contour(x,y,plotData,asp=1,col=terrain.colors(12))
#		image(matrix(rep(1:100,length.out=1000),nrow=100),asp=1,col=rainbow(100,start=0.49,end=0.75))

		if (is.temp){
			image(x,y,datPlot,asp=1,col=rainbow(colNo,start=0.65,end=1),axes=F,breaks=seq(limits[1],limits[2],length.out=colNo+1))
			contour(x,y,datPlot, levels=seq(limits[1],limits[2],25), lwd=.2, add=TRUE, col="brown")
		}else{
			image(x,y,datPlot,asp=1,col=rainbow(colNo,start=0.49,end=0.75),axes=F,breaks=seq(limits[1],limits[2],length.out=colNo+1))
			contour(x,y,datPlot, levels=seq(limits[1],limits[2],100), lwd=.2, add=TRUE, col="brown")
		}

		# plot stations
		ufs <- poi_latLon2pointIndex(c(-29.10901,26.18730),latSub,lonSub) 
		points(ufs,pch="+")

		# plot
		opt<-2;
		if(r==1 && c==1){
			plotBorders<-map_borders(opt=opt,plotData,latSub,lonSub);
		}
		lines(plotBorders$land,		type="l",lty=1,asp=1);
		if(opt>1)	lines(plotBorders$country,	type="l",lty=2,asp=1);
		if(opt>2)	lines(plotBorders$river,	type="l",lty=3,asp=1);
	}
}



















