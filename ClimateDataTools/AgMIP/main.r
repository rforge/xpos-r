##
 # FILE main.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/ClimateDataTools/AgMIP
 ####################################################################

#graphics.off()
source('matrixTools.r')
source('mapsTools.r')
source('../plotFct.r')

####### INIT
#path2in <- "/home/crespo/Desktop/Link to WinShared/12_AgMIP/2012-09_SAAworkshop/AgMIP-Accra Climate from Alex/WorldClim/"
path2in <- "/local/users-a/crespo/wine_shared/12_AgMIP/2012-09_SAAworkshop/AgMIP-Accra Climate from Alex/WorldClim/"
#path2in <- "~/Desktop/WorldClim/"
pathToWorldClim <-"/home/crespo/Desktop/12_AgMIP/2012-10-01_fastTrack/AMIP/WorldClimData/"

####### LOAD DATA
#mat_translate(path2in)
load(paste(path2in,"wdc_lat.rData",sep=""));latAll<-wdc;	# (lat,repeat)
load(paste(path2in,"wdc_lon.rData",sep=""));lonAll<-wdc;	# (repeat,lon)
load(paste(path2in,"wdc_alt.rData",sep=""));altAll<-wdc;	# altitude = (lat,lon)
load(paste(path2in,"wdc_ppt.rData",sep=""));pptAll<-wdc;	# total(?) precipitation = (lat,lon,month)
load(paste(path2in,"wdc_tme.rData",sep=""));tmeAll<-wdc;	# mean temperature = (lat,lon,month)

####### DEFINE SUBSET
#corTL<-c(40,-30);corBR<-c(-35,52)		# 137.158.102.43:/exports/terra/data/staging/local/users-aAfrica
#corTL<-c(0,10);corBR<-c(-35,52)		# southern Africa
#corTL<-c(-22,15.9);corBR<-c(-35,34)		# South Africa
corTL<-c(-28.83,25.66667);corBR<-c(-29.50,27.00)	# fast-track Bloemfontein

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
is.temp<-F; # F is rainfall
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

skip<-T
if(!skip){
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
#			image(x,y,datPlot,asp=1,col=rainbow(colNo,start=0.65,end=1),axes=F,breaks=seq(limits[1],limits[2],length.out=colNo+1))
			image(x,y,datPlot,asp=1,col=rainbow(colNo,start=0.65,end=1),axes=F,breaks=seq(min(datPlot),max(datPlot),length.out=colNo+1))
			contour(x,y,datPlot, levels=seq(limits[1],limits[2],1), lwd=.5, add=TRUE, col="black")
		}else{
#			image(x,y,datPlot,asp=1,col=rainbow(colNo,start=0.49,end=0.75),axes=F,breaks=seq(limits[1],limits[2],length.out=colNo+1))
			image(x,y,datPlot,asp=1,col=rainbow(colNo,start=0.49,end=0.75),axes=F,breaks=seq(min(datPlot),max(datPlot),length.out=colNo+1))
			contour(x,y,datPlot, levels=seq(limits[1],limits[2],5), lwd=.5, add=TRUE, col="black")
		}

		# plot stations
		#UFS
#		points(poi_latLon2pointIndex(c(-29.10901,26.18730),latSub,lonSub),pch=1)
		#CSAG station
#		points(poi_latLon2pointIndex(c(-29.1,26.3),latSub,lonSub),pch=2)
		#MERRA station
		points(poi_latLon2pointIndex(c(-29.12,26.18),latSub,lonSub),pch=3)
		#Wiltrud stations
		points(poi_latLon2pointIndex(c(-29.30,25.88333),latSub,lonSub),pch=".")	#	13742 -29.30,25.88333 NA 	1990010112 1997073112 0 _KUILPUT_
		points(poi_latLon2pointIndex(c(-29.20,26.00),latSub,lonSub),pch=".")	#	13745 -29.20,26.00 NA 		1980010112 1986013112 0 _SALOPIA_
		points(poi_latLon2pointIndex(c(-29.43333,26.08333),latSub,lonSub),pch=".")#	13749 -29.43333,26.08333 NA 	1990010112 1999113012 0 _KAFFERRIVIER - POL_
		points(poi_latLon2pointIndex(c(-29.20,26.18333),latSub,lonSub),pch=".")	#	13762 -29.20,26.18333 NA 	1980010112 1986063012 0 _FERREIRA_
		points(poi_latLon2pointIndex(c(-29.08333,26.21667),latSub,lonSub),pch=".")#	13765 -29.08333,26.21667 NA 	1980010112 1981123112 0 _BLOEMFONTEIN - BAYSWATER_
		points(poi_latLon2pointIndex(c(-29.13333,26.21667),latSub,lonSub),pch=".")#	13770 -29.13333,26.21667 NA 	1990010112 1999123112 0 _BLOEMFONTEIN - KINGS PAR_
		points(poi_latLon2pointIndex(c(-29.15,26.21667),latSub,lonSub),pch=".")	#	13771 -29.15,26.21667 NA 	1990010112 1999013112 0 _BLOEMFONTEIN - HAMILTON_
		points(poi_latLon2pointIndex(c(-29.10,26.30),latSub,lonSub),pch=".")	#	13776 -29.10,26.30 NA 		1990010112 1992123112 0 _BLOEMFONTEIN JBM HERTZOG_
		points(poi_latLon2pointIndex(c(-29.10,26.30),latSub,lonSub),pch=".")	#	13777 -29.10,26.30 NA 		1990010112 1991022812 0 _J B M HERTZOG - WK_
		points(poi_latLon2pointIndex(c(-29.21667,26.30),latSub,lonSub),pch=".")	#	13781 -29.21667,26.30 NA 	1990010112 1999013112 0 _GROOTVLEI - TNK_
		points(poi_latLon2pointIndex(c(-29.03333,26.41667),latSub,lonSub),pch=".")#	13786 -29.03333,26.41667 NA 	1990010112 1999123112 0 _MAZELSPOORT DAM_
		points(poi_latLon2pointIndex(c(-29.21667,26.41667),latSub,lonSub),pch=".")#	13787 -29.21667,26.41667 NA 	1990010112 1999123112 0 _TUSSENVIER_
		points(poi_latLon2pointIndex(c(-29.08333,26.60),latSub,lonSub),pch=".")	#	13795 -29.08333,26.60 NA 	1990010112 1999123112 0 _TEVREDE_
		points(poi_latLon2pointIndex(c(-29.26667,26.60),latSub,lonSub),pch=".")	#	13796 -29.26667,26.60 NA 	1990010112 1993053112 0 _RUSTFONTEINDAM - IRR_
		points(poi_latLon2pointIndex(c(-29.11667,26.76667),latSub,lonSub),pch=".")#	13806 -29.11667,26.76667 NA 	1980010112 1987123112 0 _FELOANE_
		points(poi_latLon2pointIndex(c(-29.28333,26.81667),latSub,lonSub),pch=".")#	13810 -29.28333,26.81667 NA 	1980010112 1984103112 0 _BLYDSKAP_
		points(poi_latLon2pointIndex(c(-29.06667,26.90),latSub,lonSub),pch=".")	#	13817 -29.06667,26.90 NA 	1980010112 1984083112 0 _NORTH END_
		points(poi_latLon2pointIndex(c(-29.05,26.96667),latSub,lonSub),pch=".")	#	13827 -29.05,26.96667 NA 	1980010112 1984083112 0 _RAKHOI_
		points(poi_latLon2pointIndex(c(-28.88333,25.96667),latSub,lonSub),pch=".")#	14123 -28.88333,25.96667 NA 	1990010112 1999123112 0 _KRUGERSDRIFTDAM_
		points(poi_latLon2pointIndex(c(-28.95,26.33333),latSub,lonSub),pch=".")	#	14137 -28.95,26.33333 NA 	1990010112 1999123112 0 _GLEN COLLEGE - AGR_
		points(poi_latLon2pointIndex(c(-28.95,26.81667),latSub,lonSub),pch=".")	#	14159 -28.95,26.81667 NA 	1980010112 1982073112 0 _KGALALA_
		points(poi_latLon2pointIndex(c(-28.95,26.35),latSub,lonSub),pch=".")	#	19839 -28.95,26.35 NA 		1990010112 1999053112 0 _BLOEMFONTEIN;GLEN._
		points(poi_latLon2pointIndex(c(-29.43333,26.10),latSub,lonSub),pch=".")	#	20305 -29.43333,26.10 NA 	1990010112 1993063012 0 _KAFFERRIVIER.DEPOT._
		points(poi_latLon2pointIndex(c(-29.13333,25.83333),latSub,lonSub),pch=".")#	20318 -29.13333,25.83333 NA 	1990010112 1999093012 0 _DE BRUG.DEPOT._
		points(poi_latLon2pointIndex(c(-29.01667,26.30),latSub,lonSub),pch=".")	#	20386 -29.01667,26.30 NA 	1990010112 1999093012 0 _OOVAN TONDER:DEPOT._
		points(poi_latLon2pointIndex(c(-29.08333,27.00),latSub,lonSub),pch=".")	#	20390 -29.08333,27.00 NA 	1990010112 1992103112 0 _WEPENER.DEPOT._
		points(poi_latLon2pointIndex(c(-29.05,26.11667),latSub,lonSub),pch=".")	#	20579 -29.05,26.11667 NA 	1990010112 1999123112 0 _BAINSVLEI_
		points(poi_latLon2pointIndex(c(-29.10,26.30),latSub,lonSub),pch=".")	#	20601 -29.10,26.30 NA 		1990010112 1999093012 0 _BLOEMFONTEIN JBM HERTZOG_
		points(poi_latLon2pointIndex(c(-29.31667,25.90),latSub,lonSub),pch=".")	#	22426 -29.31667,25.90 NA 	1998060112 1999113012 0 _WILLIAMSTRIP_
		points(poi_latLon2pointIndex(c(-28.92956,26.32631),latSub,lonSub),pch=4)#	30144 -28.92956,26.32631 NA 	1999062312 2011033112 0 _BLOEMFONTEIN; GLEN_
		points(poi_latLon2pointIndex(c(-29.10,26.30),latSub,lonSub),pch=".")	#	30271 -29.10,26.30 NA 		2000010112 2004043012 0 _BLOEMFONTEIN WO_
		points(poi_latLon2pointIndex(c(-29.11667,26.18333),latSub,lonSub),pch=".")#	30272 -29.11667,26.18333 NA 	2000010112 2004013112 0 _BLOEM STAD_
		points(poi_latLon2pointIndex(c(-29.06543,26.93816),latSub,lonSub),pch=4)#	30419 -29.06543,26.93816 NA 	1999022612 2010122912 0 _THABA NCHU_
		points(poi_latLon2pointIndex(c(-29.02125,26.14578),latSub,lonSub),pch=4)#	30454 -29.02125,26.14578 NA 	2000100412 2011033112 0 _BLOEMFONTEIN - BAINSVLEI_
		points(poi_latLon2pointIndex(c(-29.1058,26.185),latSub,lonSub),pch=".")	#	30455 -29.1058,26.185 NA 	2000100412 2010062712 0 _BLOEMFONTEIN - UOFS_
		points(poi_latLon2pointIndex(c(-29.23489,26.20272),latSub,lonSub),pch=".")#	30736 -29.23489,26.20272 NA 	2006071812 2008071312 0 _BLOEMFONTEIN: PARADYS_
		points(poi_latLon2pointIndex(c(-29.45014,26.03777),latSub,lonSub),pch=".")#	30737 -29.45014,26.03777 NA 	2006072012 2011033112 0 _BLOEMFONTEIN: WEGSLUIT_
		

		# plot
#		opt<-2;
#		if(r==1 && c==1){
#			plotBorders<-map_borders(opt=opt,plotData,latSub,lonSub);
#		}
#		lines(plotBorders$land,		type="l",lty=1,asp=1);
#		if(opt>1)	lines(plotBorders$country,	type="l",lty=2,asp=1);
#		if(opt>2)	lines(plotBorders$river,	type="l",lty=3,asp=1);
	}
}

if (is.temp){
	copyDev2eps(file=paste(pathToWorldClim,"fastTrack_temp.eps",sep=""))
	copyDev2jpg(file=paste(pathToWorldClim,"fastTrack_temp.jpg",sep=""))
}else{
	copyDev2eps(file=paste(pathToWorldClim,"fastTrack_prec.eps",sep=""))
	copyDev2jpg(file=paste(pathToWorldClim,"fastTrack_prec.jpg",sep=""))
}
}#skip

datSub<-array(NA,dim=dim(latSub))

graphics.off()
if (is.temp){
	for(i in 1:dim(tmeSub)[1]){
		for(j in 1:dim(tmeSub)[2]){
			datSub[i,j] <- mean(tmeSub[i,j,])
		}
	}
	tmpPlot<-mat_rotate45(datSub)
	image(x,y,tmpPlot,asp=1,col=rainbow(colNo,start=0.65,end=1),axes=F,breaks=seq(min(datPlot),max(datPlot),length.out=colNo+1))
	contour(x,y,tmpPlot, levels=seq(limits[1],limits[2],2), lwd=.5, add=TRUE, col="black")
#	copyDev2eps(file=paste(pathToWorldClim,"fastTrack_annTemp.eps",sep=""))
#	copyDev2jpg(file=paste(pathToWorldClim,"fastTrack_annTemp.jpg",sep=""))
}else{
	for(i in 1:dim(pptSub)[1]){
		for(j in 1:dim(pptSub)[2]){
			datSub[i,j] <- mean(pptSub[i,j,])
		}
	}
	pptPlot<-mat_rotate45(datSub)
	image(x,y,pptPlot,asp=1,col=rainbow(colNo,start=0.49,end=0.75),axes=F,breaks=seq(min(datPlot),max(datPlot),length.out=colNo+1))
	contour(x,y,pptPlot, levels=seq(limits[1],limits[2],10), lwd=.5, add=TRUE, col="black")
#	copyDev2eps(file=paste(pathToWorldClim,"fastTrackMix_annPrec.eps",sep=""))
#	copyDev2jpg(file=paste(pathToWorldClim,"fastTrackMix_annPrec.jpg",sep=""))
}

browser()

# plot stations
		#UFS
#		points(poi_latLon2pointIndex(c(-29.10901,26.18730),latSub,lonSub),pch=1)
		#CSAG station
#		points(poi_latLon2pointIndex(c(-29.1,26.3),latSub,lonSub),pch=2)
		#MERRA station
		points(poi_latLon2pointIndex(c(-29.12,26.18),latSub,lonSub),pch=3)
		#Wiltrud stations
#		points(poi_latLon2pointIndex(c(-29.30,25.88333),latSub,lonSub),pch=".")	#	13742 -29.30,25.88333 NA 	1990010112 1997073112 0 _KUILPUT_
#		points(poi_latLon2pointIndex(c(-29.20,26.00),latSub,lonSub),pch=".")	#	13745 -29.20,26.00 NA 		1980010112 1986013112 0 _SALOPIA_
#		points(poi_latLon2pointIndex(c(-29.43333,26.08333),latSub,lonSub),pch=".")#	13749 -29.43333,26.08333 NA 	1990010112 1999113012 0 _KAFFERRIVIER - POL_
#		points(poi_latLon2pointIndex(c(-29.20,26.18333),latSub,lonSub),pch=".")	#	13762 -29.20,26.18333 NA 	1980010112 1986063012 0 _FERREIRA_
#		points(poi_latLon2pointIndex(c(-29.08333,26.21667),latSub,lonSub),pch=".")#	13765 -29.08333,26.21667 NA 	1980010112 1981123112 0 _BLOEMFONTEIN - BAYSWATER_
#		points(poi_latLon2pointIndex(c(-29.13333,26.21667),latSub,lonSub),pch=".")#	13770 -29.13333,26.21667 NA 	1990010112 1999123112 0 _BLOEMFONTEIN - KINGS PAR_
#		points(poi_latLon2pointIndex(c(-29.15,26.21667),latSub,lonSub),pch=".")	#	13771 -29.15,26.21667 NA 	1990010112 1999013112 0 _BLOEMFONTEIN - HAMILTON_
#		points(poi_latLon2pointIndex(c(-29.10,26.30),latSub,lonSub),pch=".")	#	13776 -29.10,26.30 NA 		1990010112 1992123112 0 _BLOEMFONTEIN JBM HERTZOG_
#		points(poi_latLon2pointIndex(c(-29.10,26.30),latSub,lonSub),pch=".")	#	13777 -29.10,26.30 NA 		1990010112 1991022812 0 _J B M HERTZOG - WK_
#		points(poi_latLon2pointIndex(c(-29.21667,26.30),latSub,lonSub),pch=".")	#	13781 -29.21667,26.30 NA 	1990010112 1999013112 0 _GROOTVLEI - TNK_
#		points(poi_latLon2pointIndex(c(-29.03333,26.41667),latSub,lonSub),pch=".")#	13786 -29.03333,26.41667 NA 	1990010112 1999123112 0 _MAZELSPOORT DAM_
#		points(poi_latLon2pointIndex(c(-29.21667,26.41667),latSub,lonSub),pch=".")#	13787 -29.21667,26.41667 NA 	1990010112 1999123112 0 _TUSSENVIER_
#		points(poi_latLon2pointIndex(c(-29.08333,26.60),latSub,lonSub),pch=".")	#	13795 -29.08333,26.60 NA 	1990010112 1999123112 0 _TEVREDE_
#		points(poi_latLon2pointIndex(c(-29.26667,26.60),latSub,lonSub),pch=".")	#	13796 -29.26667,26.60 NA 	1990010112 1993053112 0 _RUSTFONTEINDAM - IRR_
#		points(poi_latLon2pointIndex(c(-29.11667,26.76667),latSub,lonSub),pch=".")#	13806 -29.11667,26.76667 NA 	1980010112 1987123112 0 _FELOANE_
#		points(poi_latLon2pointIndex(c(-29.28333,26.81667),latSub,lonSub),pch=".")#	13810 -29.28333,26.81667 NA 	1980010112 1984103112 0 _BLYDSKAP_
#		points(poi_latLon2pointIndex(c(-29.06667,26.90),latSub,lonSub),pch=".")	#	13817 -29.06667,26.90 NA 	1980010112 1984083112 0 _NORTH END_
#		points(poi_latLon2pointIndex(c(-29.05,26.96667),latSub,lonSub),pch=".")	#	13827 -29.05,26.96667 NA 	1980010112 1984083112 0 _RAKHOI_
#		points(poi_latLon2pointIndex(c(-28.88333,25.96667),latSub,lonSub),pch=".")#	14123 -28.88333,25.96667 NA 	1990010112 1999123112 0 _KRUGERSDRIFTDAM_
#		points(poi_latLon2pointIndex(c(-28.95,26.33333),latSub,lonSub),pch=".")	#	14137 -28.95,26.33333 NA 	1990010112 1999123112 0 _GLEN COLLEGE - AGR_
#		points(poi_latLon2pointIndex(c(-28.95,26.81667),latSub,lonSub),pch=".")	#	14159 -28.95,26.81667 NA 	1980010112 1982073112 0 _KGALALA_
#		points(poi_latLon2pointIndex(c(-28.95,26.35),latSub,lonSub),pch=".")	#	19839 -28.95,26.35 NA 		1990010112 1999053112 0 _BLOEMFONTEIN;GLEN._
#		points(poi_latLon2pointIndex(c(-29.43333,26.10),latSub,lonSub),pch=".")	#	20305 -29.43333,26.10 NA 	1990010112 1993063012 0 _KAFFERRIVIER.DEPOT._
#		points(poi_latLon2pointIndex(c(-29.13333,25.83333),latSub,lonSub),pch=".")#	20318 -29.13333,25.83333 NA 	1990010112 1999093012 0 _DE BRUG.DEPOT._
#		points(poi_latLon2pointIndex(c(-29.01667,26.30),latSub,lonSub),pch=".")	#	20386 -29.01667,26.30 NA 	1990010112 1999093012 0 _OOVAN TONDER:DEPOT._
#		points(poi_latLon2pointIndex(c(-29.08333,27.00),latSub,lonSub),pch=".")	#	20390 -29.08333,27.00 NA 	1990010112 1992103112 0 _WEPENER.DEPOT._
#		points(poi_latLon2pointIndex(c(-29.05,26.11667),latSub,lonSub),pch=".")	#	20579 -29.05,26.11667 NA 	1990010112 1999123112 0 _BAINSVLEI_
#		points(poi_latLon2pointIndex(c(-29.10,26.30),latSub,lonSub),pch=".")	#	20601 -29.10,26.30 NA 		1990010112 1999093012 0 _BLOEMFONTEIN JBM HERTZOG_
#		points(poi_latLon2pointIndex(c(-29.31667,25.90),latSub,lonSub),pch=".")	#	22426 -29.31667,25.90 NA 	1998060112 1999113012 0 _WILLIAMSTRIP_
#		points(poi_latLon2pointIndex(c(-28.92956,26.32631),latSub,lonSub),pch=4)#	30144 -28.92956,26.32631 NA 	1999062312 2011033112 0 _BLOEMFONTEIN; GLEN_
#		points(poi_latLon2pointIndex(c(-29.10,26.30),latSub,lonSub),pch=".")	#	30271 -29.10,26.30 NA 		2000010112 2004043012 0 _BLOEMFONTEIN WO_
#		points(poi_latLon2pointIndex(c(-29.11667,26.18333),latSub,lonSub),pch=".")#	30272 -29.11667,26.18333 NA 	2000010112 2004013112 0 _BLOEM STAD_
#		points(poi_latLon2pointIndex(c(-29.06543,26.93816),latSub,lonSub),pch=4)#	30419 -29.06543,26.93816 NA 	1999022612 2010122912 0 _THABA NCHU_
#		points(poi_latLon2pointIndex(c(-29.02125,26.14578),latSub,lonSub),pch=4)#	30454 -29.02125,26.14578 NA 	2000100412 2011033112 0 _BLOEMFONTEIN - BAINSVLEI_
#		points(poi_latLon2pointIndex(c(-29.1058,26.185),latSub,lonSub),pch=".")	#	30455 -29.1058,26.185 NA 	2000100412 2010062712 0 _BLOEMFONTEIN - UOFS_
#		points(poi_latLon2pointIndex(c(-29.23489,26.20272),latSub,lonSub),pch=".")#	30736 -29.23489,26.20272 NA 	2006071812 2008071312 0 _BLOEMFONTEIN: PARADYS_
#		points(poi_latLon2pointIndex(c(-29.45014,26.03777),latSub,lonSub),pch=".")#	30737 -29.45014,26.03777 NA 	2006072012 2011033112 0 _BLOEMFONTEIN: WEGSLUIT_


if (is.temp){
	copyDev2eps(file=paste(pathToWorldClim,"fastTrack_annTemp.eps",sep=""))
	copyDev2jpg(file=paste(pathToWorldClim,"fastTrack_annTemp.jpg",sep=""))
}else{
	copyDev2eps(file=paste(pathToWorldClim,"fastTrack_annPrec.eps",sep=""))
	copyDev2jpg(file=paste(pathToWorldClim,"fastTrack_annPrec.jpg",sep=""))
}









