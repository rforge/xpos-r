##
 # FILE agmip_climPlot-oc.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
 ####################################################################
 # so far (OCT-2013)
 # stat indices are computed directly in the plot function
 # not supposed to stay like this
 ####################################################################
 # FYI
 #% @DATE    YYYY      MM      DD    SRAD    TMAX    TMIN    RAIN    WIND     DEWP    VPRS    RHUM  # Tave
 #%	1	2	3	4	5	6	7	8	9	10	11	12	13
 ####################################################################

ifelse(Sys.info()["sysname"]=="Linux",locSep<-"/",locSep<-"//")
graphics.off()

# default
area <- "";baseFolder<-"../data/Climate/Historical";futureFolder<-"../data/Climate/Simplescenario";graphInFolder <- "../data/Climate/Graphics.2"
fiveGCM_l <- c("E","I","K","O","R")
fiveGCM_n <- c(grep("E",LETTERS),grep("I",LETTERS),grep("K",LETTERS),grep("O",LETTERS),grep("R",LETTERS))
fiveColors<- c(26,33,43,50,142)

# MAL-CLIP
##area <- "Mzimba - Malawi";baseFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/MAL-CLIP/MAL-Baselines";futureFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/MAL-CLIP/MAL-Futures";graphInFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/MAL-CLIP/MAL-Graphics.2";baseP<-c(10,11,12,1,2,3,4)
# MOZ-area
##area <- "Sussundenga - Mozambique";baseFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/MOZ-CLIP/MOZ-Baselines";futureFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/MOZ-CLIP/MOZ-Futures";graphInFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/MOZ-CLIP/MOZ-Graphics.2";baseP<-c(10,11,12,1,2,3)
# RZA-CLIP
##area <- "Limpopo - South Africa";baseFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/RZA-CLIP/RZA-Baselines";futureFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/RZA-CLIP/RZA-Futures";graphInFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/RZA-CLIP/RZA-Graphics.2";baseP<-c(10,11,12,1,2,3)
# ZIM-CLIP
area <- "Nkayi - Zimbabwe";baseFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/ZIM-CLIP/ZIM-HS-Baselines";futureFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/ZIM-CLIP/ZIM-HS-Futures-v2.1";graphInFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/ZIM-CLIP/ZIM-HS-Graphics.2";baseP<-c(10,11,12,1,2,3)

# BOT-AMIIP
##area <- "N/S - Botswana";baseFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/BOT-AMIIP/BOT-Baselines";futureFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/BOT-AMIIP/BOT-Futures";graphInFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/BOT-AMIIP/BOT-Graphics.2";baseP<-c(10,11,12,1,2,3)
# LES-AMIIP
##area <- "Mohale's Hoek - Lesotho";baseFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/LES-AMIIP/LES-Baselines";futureFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/LES-AMIIP/LES-Futures";graphInFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/LES-AMIIP/LES-Graphics.2";baseP<-c(10,11,12,1,2,3)
# NAM-AMIIP-caprivi
##area <- "Caprivi - Namibia";baseFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/NAM-AMIIP/NAM-caprivi-Baselines";futureFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/NAM-AMIIP/NAM-caprivi-Futures";graphInFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/NAM-AMIIP/NAM-caprivi-Graphics.2";baseP<-c(10,11,12,1,2,3)
# NAM-AMIIP-rundu
##area <- "Rundu - Namibia";baseFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/NAM-AMIIP/NAM-rundu-Baselines";futureFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/NAM-AMIIP/NAM-rundu-Futures";graphInFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/NAM-AMIIP/NAM-rundu-Graphics.2";baseP<-c(10,11,12,1,2,3)
# RZA-AMIIP-HS
##area <- "Free State - South Africa";baseFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/RZA-AMIIP/RZA-HS-Baselines";futureFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/RZA-AMIIP/RZA-HS-Futures-v2.1";graphInFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/RZA-AMIIP/RZA-HS-Graphics.2";baseP<-c(10,11,12,1,2,3)
# RZA-AMIIP
##area <- "Free State - South Africa";baseFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/RZA-AMIIP/RZA-Baselines";futureFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/RZA-AMIIP/RZA-Futures";graphInFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/RZA-AMIIP/RZA-Graphics.2";baseP<-c(10,11,12,1,2,3)
# SWA-AMIIP
##area <- "Swaziland";baseFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/SWA-AMIIP/SWA-Baselines";futureFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/SWA-AMIIP/SWA-Futures";graphInFolder <- "~/Desktop/Wine-shared/Projects/2012-2014_AgMIP/SWA-AMIIP/SWA-Graphics.2";baseP<-c(10,11,12,1,2,3)


# time period and emmission
periodxRCP <- c(	"Control",		#A = observational time period
		"RCP2.6, Near-term",		#B = RCP2.6	2010-2039 (Near-term)
		"RCP4.5, Near-term",		#C = RCP4.5	2010-2039 (Near-term)
		"RCP6.0, Near-term",		#D = RCP6.0	2010-2039 (Near-term)
		"RCP8.5, Near-term",		#E = RCP8.5	2010-2039 (Near-term)
		"RCP2.6, Mid-Century",		#F = RCP2.6	2040-2069 (Mid-Century)
		"RCP4.5, Mid-Century",		#G = RCP4.5	2040-2069 (Mid-Century)
		"RCP6.0, Mid-Century",		#H = RCP6.0	2040-2069 (Mid-Century)
		"RCP8.5, Mid-Century",		#I = RCP8.5	2040-2069 (Mid-Century)
		"RCP2.6, End-of-Century",	#J = RCP2.6	2070-2099 (End-of-Century)
		"RCP4.5, End-of-Century",	#K = RCP4.5	2070-2099 (End-of-Century)
		"RCP6.0, End-of-Century",	#L = RCP6.0	2070-2099 (End-of-Century)
		"RCP8.5, End-of-Century")	#M = RCP8.5	2070-2099 (End-of-Century)

colYear <- 2 
colMonth <- 3
colTmax <- 6
colTmin <- 7
colRain <- 8
colTave <- 13

if(!file.exists(graphInFolder)){
	if(!dir.create(graphInFolder)){
		print(paste("","Could not create the output Graphics folder",sep=" >> "))
		browser()
	}
}

##
 # callPlot, to loop on files, time periods, for every plots
 # inputs
 # (above) : area, baseFolder, futureFolder
 # basePeriod : vector of months, e.g. OND is c(10,11,12)
 # GCMs : vector of GCM letter identifiers, e.g. c("C","K")
 # futurePeriod: vector of time period x RCP combination as specified in AgMIP (A..M)
 # outputs
 # currently: save figure in XXXXX_XX.AgMIP
 # 	standing for by order 	XX: country ID; XX: location ID;
 #				X: futperiod x RCP; _ to represent multiple GCMs
 #				XX: identical to baseFile	
 #
 # NB: ONLY AgMIP FILES IN BASEFOLDER	
callPlot <- function(basePeriod=baseP,GCMs=LETTERS[1:20],futurePeriod_l=LETTERS[1:13],biasMeth_l="A",periodRCP=periodxRCP,line5GCM=FALSE)
{
	# loop on every files in the baseFolder
	baseFile_l <- list.files(baseFolder)
	for(f in 1:length(baseFile_l)){
		baseInFile <- paste(baseFolder,baseFile_l[f],sep=locSep)
		fileNsplit0 <- strsplit(baseInFile,split=locSep)[[1]][length(strsplit(baseInFile,split=locSep)[[1]])]
		fileNsplit1 <- strsplit(fileNsplit0,split="\\.")[[1]][1]
		fileNsplit2 <- strsplit(fileNsplit1,split="")[[1]]
		location <- paste(area," (",paste(fileNsplit2[1],fileNsplit2[2],fileNsplit2[3],fileNsplit2[4],sep=""),")",sep="")
		biasMeth <- biasMeth_l[1]

		# trends
		plot_tmpTrends(baseInFile,figFile=paste(graphInFolder,fileNsplit1,sep=locSep),location)
#		plot_rainTrends(baseInFile,figFile=paste(graphInFolder,fileNsplit1,sep=locSep),location)

		# loop on every time period
		for (futurePeriod in futurePeriod_l){
			fileName <- paste(paste(fileNsplit2[1],fileNsplit2[2],fileNsplit2[3],fileNsplit2[4],futurePeriod,GCMs[runif(1,1,length(GCMs))],fileNsplit2[7],biasMeth,sep=""),"AgMIP",sep=".")
			if(!file.exists(paste(futureFolder,fileName,sep=locSep))) next
			print(paste("future period",futurePeriod,"file",paste(fileNsplit2[1],fileNsplit2[2],fileNsplit2[3],fileNsplit2[4],sep=""),sep=" >> "))
			# adapt figure title
			figTitle <- periodRCP[which(futurePeriod==LETTERS[1:26])]

 			figFile <- paste(graphInFolder,paste(fileNsplit2[1],fileNsplit2[2],fileNsplit2[3],fileNsplit2[4],futurePeriod,"_",fileNsplit2[7],biasMeth,sep=""),sep=locSep)
			# call plot functions
			plot_BPgcm(basePeriod,GCMs,futurePeriod,baseInFile,location,figFile,fileNsplit2,biasMeth,figTitle,line5GCM)
			plot_TPgcm(basePeriod,GCMs,futurePeriod,baseInFile,location,figFile,fileNsplit2,biasMeth,figTitle)
		}
	}
}

############################################################################
############################################################################
plot_tmpTrends <- function(baseInFile,figFile,location)
{
	yearNo <- 31
	graphics.off()

	# baslines
	baseData <- read.table(baseInFile, skip = 5, sep = '')

	xVec <- 1:(dim(baseData)[1])
	yMat <- matrix(c(baseData[,colTmin],baseData[,colTmax]),byrow=T,nrow=2)

	runWin <- length(xVec)/yearNo/2
	rm_tmin <- NULL
	rm_tmax <- NULL
	for(i in xVec){
		if(i<(runWin/2)||i>(length(xVec)-runWin/2)){
			rm_tmin <- c(rm_tmin,NA)
			rm_tmax <- c(rm_tmax,NA)
		}else{
			s <- i-runWin/2
			e <- i+runWin/2
			rm_tmin <- c(rm_tmin,mean(yMat[1,s:e]))
			rm_tmax <- c(rm_tmax,mean(yMat[2,s:e]))
		}
	}
	tr_tmin_a <- lm(rm_tmin~xVec)
	tr_tmax_a <- lm(rm_tmax~xVec)

	ymin<-floor(min(yMat,na.rm=TRUE))
	ymax<-ceiling(max(yMat,na.rm=TRUE))
#	ymin<-ymin-.1*(ymax-ymin)

	# stat test (vs daily data)
	s <- ceiling(.5*(dim(yMat)[2]-length(tr_tmin_a$fitted.values)))
	e <- dim(yMat)[2]-s
	ttD.tmin <- t.test(yMat[1,s:e],tr_tmin_a$fitted.values)
	ttD.tmax <- t.test(yMat[2,s:e],tr_tmax_a$fitted.values)
	ctD.tmin <- cor.test(yMat[1,s:e],tr_tmin_a$fitted.values,method="pearson",alternative="greater")
	ctD.tmax <- cor.test(yMat[2,s:e],tr_tmax_a$fitted.values,method="pearson",alternative="greater")
#print(ttD.tmin)
#print(ctD.tmin)
	# stat test (vs runing mean)
	ttR.tmin <- t.test(rm_tmin,tr_tmin_a$fitted.values)
	ttR.tmax <- t.test(rm_tmax,tr_tmax_a$fitted.values)
	ctR.tmin <- cor.test(rm_tmin[s:e],tr_tmin_a$fitted.values,method="pearson")
	ctR.tmax <- cor.test(rm_tmax[s:e],tr_tmax_a$fitted.values,method="pearson")
#print(ttR.tmin)
#print(ctR.tmin)
#browser()

	jpeg(filename=paste(figFile,"-BT.jpg",sep=""),width=480,height=480)

	figT <- paste("Baseline trends, ",location,"\r\n","tmax ",signif(tr_tmax_a$coefficients[2]*(length(xVec)-runWin)/yearNo,digits=2)," and tmin ",signif(tr_tmin_a$coefficients[2]*length(xVec)/yearNo,digits=2)," oC/year",sep="")
	plot(xVec,yMat[1,],type="l",ylim=c(ymin,ymax),pty="m",col="lightblue",xlab="1980-2010 (11323 days)",ylab="min and max temperatures (oC)",bty="n",main=figT)
	points(xVec,yMat[2,],type="l",col="lightcoral")

	lines(rm_tmin,lw=2,col="darkblue")
	lines(rm_tmax,lw=2,col="darkred")

	abline(h=mean(yMat[1,]),lw=.5,lt=1)
	abline(h=mean(yMat[2,]),lw=.5,lt=1)

	lines(s:e,tr_tmin_a$fitted.values,lw=2,lt=2)
	lines(s:e,tr_tmax_a$fitted.values,lw=2,lt=2)

#	legend("bottomright", ncol=2,
#		title="correlation",
#		legend=c(signif(ctD.tmin$p.value,4),signif(ctD.tmax$p.value,4),signif(ctR.tmin$p.value,4),signif(ctR.tmax$p.value,4)),
#		col=c("lightblue","lightcoral","darkblue","darkred"),
#		lty=c(1,1,1,1),
#		lwd=c(1,1,2,2)
#	)
#	legend("bottomleft", ncol=2,
#		title="p-value (t.test)",
#		legend=c(signif(ttD.tmin$p.value,4),signif(ttD.tmax$p.value,4),signif(ttR.tmin$p.value,4),signif(ttR.tmax$p.value,4)),
#		col=c("lightblue","lightcoral","darkblue","darkred"),
#		lty=c(1,1,1,1),
#		lwd=c(1,1,2,2)
#	)
	legend("bottom", ncol=3, bty="n",
		legend=c("daily tmin", "daily tmax", "runing ave. (6m)", "runing ave. (6m)", "horizontal", "trends" ),
		col=c("lightblue","lightcoral","darkblue","darkred","black","black"),
		lty=c(1,1,1,1,1,2),
		lwd=c(1,1,2,2,1,2)
	)

#savePlot(paste(figFile,"-BT.jpg",sep=""),"jpeg");graphics.off()
dev.off()
}

############################################################################
############################################################################
plot_rainTrends <- function(baseInFile,figFile,location)
{
source("/home/olivier/Desktop/Optimisation/xpos-r/ClimateDataTools/Climatology/climStat.r")
source("/home/olivier/Desktop/Optimisation/xpos-r/ClimateDataTools/ClimFormats/dataRead.r")
source("/home/olivier/Desktop/Optimisation/xpos-r/ClimateDataTools/Climatology/climTools.r")

	yearNo <- 31
	graphics.off()

	# baslines
	baseData <- read.table(baseInFile, skip = 5, sep = '')

	xVec <- 1:(dim(baseData)[1])
	yMat <- baseData[,colRain]

	myData<-read_AgMIPformat(baseInFile)
	winTot <- stat_windowTotals(myData$data,maxMV=5,winWidth=366)	

	## averages over 30 years
	clim <- NULL
	for(m in 1:12){
		for(d in 1:maxNo_days(1980,m)){ # what matters is that I want a leap year
			clim <- c(clim,mean(winTot$rain[which(winTot$mm==m&winTot$dd==d)],na.rm=T))
		}
	}
	
	anom <- NULL
	for(i in 1:length(xVec)){
		anom <- c(anom,winTot$rain[i]-clim[winTot$juld[i]])
	}

#	anom<-ifelse(anom<.2,NA,anom)
	plot(clim,type="l")
browser()
	ymin<-0
	ymax<-ceiling(max(yMat,rt_rain,winTot$rain,na.rm=TRUE))

#	plot(rt_rain,col="darkblue",ylim=c(ymin,ymax))
#	lines(yMat,col="lightblue")
#	lines(winTot$rain,lwd=2)
#	tr_rain_d <- lm(yMat~xVec)
#	tr_rain_a <- lm(rm_rain~xVec)
#	ymin<-ymin-.1*(ymax-ymin)

	# stat test (vs daily data)
	s <- ceiling(.5*(dim(yMat)[2]-length(tr_rain_a$fitted.values)))
	e <- dim(yMat)[2]-s

#	jpeg(filename=paste(figFile,"-BT.jpg",sep=""),width=480,height=480)

	figT <- paste("Baseline trends, ",location,"\r\n","rainfall ",signif(tr_rain_a$coefficients[2]*(length(xVec)-runWin)/yearNo,digits=2)," and tmin ",signif(tr_rain_a$coefficients[2]*length(xVec)/yearNo,digits=2)," oC/year",sep="")
	plot(xVec,yMat,type="l",ylim=c(ymin,ymax),pty="m",col="lightblue",xlab="1980-2010 (11323 days)",ylab="precipitation (mm)",bty="n",main=figT)
#	hist(xVec,yMat[1:100],type="l",col="lightcoral")

	lines(rm_rain,lw=2,col="darkblue")

	abline(h=mean(yMat[1,]),lw=.5,lt=1)
	abline(h=mean(yMat[2,]),lw=.5,lt=1)

	lines(s:e,tr_tmin_a$fitted.values,lw=2,lt=2)
	lines(s:e,tr_tmax_a$fitted.values,lw=2,lt=2)

#	legend("bottomright", ncol=2,
#		title="correlation",
#		legend=c(signif(ctD.tmin$p.value,4),signif(ctD.tmax$p.value,4),signif(ctR.tmin$p.value,4),signif(ctR.tmax$p.value,4)),
#		col=c("lightblue","lightcoral","darkblue","darkred"),
#		lty=c(1,1,1,1),
#		lwd=c(1,1,2,2)
#	)
#	legend("bottomleft", ncol=2,
#		title="p-value (t.test)",
#		legend=c(signif(ttD.tmin$p.value,4),signif(ttD.tmax$p.value,4),signif(ttR.tmin$p.value,4),signif(ttR.tmax$p.value,4)),
#		col=c("lightblue","lightcoral","darkblue","darkred"),
#		lty=c(1,1,1,1),
#		lwd=c(1,1,2,2)
#	)
	legend("bottom", ncol=3, bty="n",
		legend=c("daily tmin", "daily tmax", "runing ave. (6m)", "runing ave. (6m)", "horizontal", "trends" ),
		col=c("lightblue","lightcoral","darkblue","darkred","black","black"),
		lty=c(1,1,1,1,1,2),
		lwd=c(1,1,2,2,1,2)
	)

#savePlot(paste(figFile,"-BT.jpg",sep=""),"jpeg");graphics.off()
dev.off()
}

############################################################################
############################################################################
plot_BPgcm <- function(basePeriod,GCMs,futurePeriod,baseInFile,location,figFile,fileNsplit,biasMeth,figTitle,line5GCM)
{
	# baslines
	baseData <- read.table(baseInFile, skip = 5, sep = '')
	while(dim(baseData)[2]<12){	# simple scenario case
		baseData <- cbind(baseData,array(-99,dim=dim(baseData)[1]))
	}
	baseData[,colTave] <- rowMeans(baseData[,6:7])

	baseToPlot <- array(NA,dim=c(2,17))
	for (i in 1:12){
		baseToPlot[1,i] <- mean(baseData[baseData[,colMonth]==i,colTave])
		baseToPlot[2,i] <- mean(baseData[baseData[,colMonth]==i,colRain])
	}
	baseToPlot[,13] <- c(mean(baseData[,colTave]),mean(baseData[,colRain]))
	baseToPlot[,14] <- c(mean(baseData[baseData[,colMonth]==1|baseData[,colMonth]==2|baseData[,colMonth]==3,colTave]),mean(baseData[baseData[,colMonth]==1|baseData[,colMonth]==2|baseData[,colMonth]==3,colRain]))
	baseToPlot[,15] <- c(mean(baseData[baseData[,colMonth]==4|baseData[,colMonth]==5|baseData[,colMonth]==6,colTave]),mean(baseData[baseData[,colMonth]==4|baseData[,colMonth]==5|baseData[,colMonth]==6,colRain]))
	baseToPlot[,16] <- c(mean(baseData[baseData[,colMonth]==7|baseData[,colMonth]==8|baseData[,colMonth]==9,colTave]),mean(baseData[baseData[,colMonth]==7|baseData[,colMonth]==8|baseData[,colMonth]==9,colRain]))
	baseToPlot[,17] <- c(mean(baseData[baseData[,colMonth]==10|baseData[,colMonth]==11|baseData[,colMonth]==12,colTave]),mean(baseData[baseData[,colMonth]==10|baseData[,colMonth]==11|baseData[,colMonth]==12,colRain]))

	# futures
	futuToPlot <- array(NA,dim=c(4,17,length(GCMs)))
	for(i in GCMs){
		# digit 5 : future period x RCP AgMIP style ID; 6: GCM (i), 8: future bias (A)
		fileName <- paste(paste(fileNsplit[1],fileNsplit[2],fileNsplit[3],fileNsplit[4],futurePeriod,i,fileNsplit[7],biasMeth,sep=""),"AgMIP",sep=".")
#		if(!file.exists(paste(futureFolder,fileName,sep=locSep))) next
		climData <- read.table(paste(futureFolder,fileName,sep=locSep), skip = 5, sep = '')
		while(dim(climData)[2]<12){	# simple scenario case
			climData[dim(climData)[2]+1] <- array(-99,dim=dim(climData)[1])
		}
		climData[,colTave] <- rowMeans(climData[,6:7])
		for (j in 1:12){
			futuToPlot[1,j,which(GCMs==i)] <- mean(climData[climData[,colMonth]==j,colTave])
			futuToPlot[2,j,which(GCMs==i)] <- mean(climData[climData[,colMonth]==j,colRain])
		}
		futuToPlot[,13,which(GCMs==i)] <- c(mean(climData[,colTave]),mean(climData[,colRain]))
		futuToPlot[,14,which(GCMs==i)] <- c(mean(climData[climData[,colMonth]==1|climData[,colMonth]==2|climData[,colMonth]==3,colTave]),mean(climData[climData[,colMonth]==1|climData[,colMonth]==2|climData[,colMonth]==3,colRain]))
		futuToPlot[,15,which(GCMs==i)] <- c(mean(climData[climData[,colMonth]==4|climData[,colMonth]==5|climData[,colMonth]==6,colTave]),mean(climData[climData[,colMonth]==4|climData[,colMonth]==5|climData[,colMonth]==6,colRain]))
		futuToPlot[,16,which(GCMs==i)] <- c(mean(climData[climData[,colMonth]==7|climData[,colMonth]==8|climData[,colMonth]==9,colTave]),mean(climData[climData[,colMonth]==7|climData[,colMonth]==8|climData[,colMonth]==9,colRain]))
		futuToPlot[,17,which(GCMs==i)] <- c(mean(climData[climData[,colMonth]==10|climData[,colMonth]==11|climData[,colMonth]==12,colTave]),mean(climData[climData[,colMonth]==10|climData[,colMonth]==11|climData[,colMonth]==12,colRain]))
	}

	# plot
	jpeg(filename=paste(figFile,"-BP.jpg",sep=""),width=720,height=480)
	par(mfcol=c(2,1))
	
	for(i in 1:2){
		par(mfg=c(i,1))
		par(mar=c(1,4,2,1))
		par(oma=c(2,2,0,0))
		ymin<-floor(min(baseToPlot[i,],futuToPlot[i,,],na.rm=TRUE))
		ymax<-ceiling(max(baseToPlot[i,],futuToPlot[i,,],na.rm=TRUE))
		plot(1:12,baseToPlot[i,1:12],type="b",pch=8,xlim=c(1,18),ylim=c(ymin,ymax),xaxt="n",yaxt="n",xlab="",ylab=ifelse(i==1,"Average temperatures (oC)","Average precipitations (mm/day)"))
		points(14,baseToPlot[i,13],pch=8)
		lines(15:18,baseToPlot[i,14:17],type="b",pch=8)
		
		# add 5 GCM lines
		if (line5GCM){
			c<-1
			for(j in fiveGCM_n){
				lines(1:12,futuToPlot[i,1:12,j],col=fiveColors[c],lwd=.5)
				c<-c+1
			}
		}

		figT <- paste(figTitle,location,sep=", ")
		if(i==1)	bp <- boxplot(futuToPlot[i,1,],futuToPlot[i,2,],futuToPlot[i,3,],futuToPlot[i,4,],futuToPlot[i,5,],futuToPlot[i,6,],
			futuToPlot[i,7,],futuToPlot[i,8,],futuToPlot[i,9,],futuToPlot[i,10,],futuToPlot[i,11,],futuToPlot[i,12,],
			NA,futuToPlot[i,13,],futuToPlot[i,14,],futuToPlot[i,15,],futuToPlot[i,16,],futuToPlot[i,17,],
			axis=FALSE,add=TRUE,outline=FALSE,main=figT)
		if(i==2)	bp <- boxplot(futuToPlot[i,1,],futuToPlot[i,2,],futuToPlot[i,3,],futuToPlot[i,4,],futuToPlot[i,5,],futuToPlot[i,6,],
			futuToPlot[i,7,],futuToPlot[i,8,],futuToPlot[i,9,],futuToPlot[i,10,],futuToPlot[i,11,],futuToPlot[i,12,],
			NA,futuToPlot[i,13,],futuToPlot[i,14,],futuToPlot[i,15,],futuToPlot[i,16,],futuToPlot[i,17,],
			axis=FALSE,add=TRUE,outline=FALSE)
		abline(v=13)
		axis(1,label=c("J","F","M","A","M","J","J","A","S","O","N","D","","ann","JFM","AMJ","JAS","OND"),at=1:18,xlab=NULL)
	}		

#savePlot(paste(figFile,"-BP.jpg",sep=""),"jpeg"); graphics.off()
dev.off()
}

############################################################################
############################################################################
plot_TPgcm <- function(basePeriod,GCMs,futurePeriod,baseInFile,location,figFile,fileNsplit,biasMeth,figTitle)
{
	climData <- read.table(baseInFile, skip = 5, sep = '')
	while(dim(climData)[2]<12){	# simple scenario case
		climData <- cbind(climData,array(-99,dim=dim(climData)[1]))
	}
	climData[,colTave] <- rowMeans(climData[,6:7])
	
	# init and baseline
	m_grsT <- NULL
	m_grsR <- NULL
	for(y in climData[1,colYear]:climData[dim(climData)[1],colYear]){
		grsT <- NULL
		grsR <- NULL
		for(m in basePeriod){
			grsT <- c(grsT,climData[climData[,colYear]==y&climData[,colMonth]==m,colTave])
			grsR <- c(grsR,climData[climData[,colYear]==y&climData[,colMonth]==m,colRain])
		}
		m_grsT <- c(m_grsT,mean(grsT))
		m_grsR <- c(m_grsR,mean(grsR))
	}	
	basT<-mean(m_grsT)
	basR<-mean(m_grsR)
	thrT<-.36*sd(m_grsT,na.rm=T)
	thrR<-.36*sd(m_grsR,na.rm=T)

	#futures
	fut <- array(NA,dim=c(2,length(GCMs)))
	for(i in GCMs){
		# digit 5 : future period x RCP AgMIP style ID
		fileName <- paste(paste(fileNsplit[1],fileNsplit[2],fileNsplit[3],fileNsplit[4],futurePeriod,i,fileNsplit[7],biasMeth,sep=""),"AgMIP",sep=".")
#		if(!file.exists(paste(futureFolder,fileName,sep=locSep))) next
		climData <- read.table(paste(futureFolder,fileName,sep=locSep), skip = 5, sep = '')
		while(dim(climData)[2]<12){	# simple scenario case
			climData <- cbind(climData,array(-99,dim=dim(climData)[1]))
		}
		climData[,colTave] <- rowMeans(climData[,6:7])
		grsT <- NULL
		grsR <- NULL
		for(y in climData[1,colYear]:climData[dim(climData)[1],colYear]){
			for(m in basePeriod){
				grsT <- c(grsT,climData[climData[,colYear]==y&climData[,colMonth]==m,colTave])
				grsR <- c(grsR,climData[climData[,colYear]==y&climData[,colMonth]==m,colRain])
			}	
		}
		fut[1,which(GCMs==i)]<-mean(grsT)
		fut[2,which(GCMs==i)]<-mean(grsR)
	}
	# plot
#	xmin <- floor(min(basT,fut[1,],na.rm=TRUE))
#	xmax <- ceiling(max(basT,fut[1,],na.rm=TRUE))
#	ymin <- min(basR,fut[2,],na.rm=TRUE)-.5
#	ymax <- max(basR,fut[2,],na.rm=TRUE)+.5
	xmin <- min((basT-3*thrT),fut[1,],na.rm=TRUE)-.1
	xmax <- max((basT+thrT),fut[1,],na.rm=TRUE)+.1
	ymin <- min((basR-thrR),fut[2,],na.rm=TRUE)-.1
	ymax <- max((basR+thrR),fut[2,],na.rm=TRUE)+.1
	diff <- max(abs(basR-ymin),abs(basR-ymax))
	ymin <- basR-diff
	ymax <- basR+diff
	if(length(basePeriod)==7){
		if(all(basePeriod==c(10,11,12,1,2,3,4))){
			xl<-"ONDJFMA temperature averages (oC)"	
			yl<-"ONDJFMA precipitation averages (mm/day)"
		}
	}
	if(length(basePeriod)==6){
		if(all(basePeriod==c(10,11,12,1,2,3))){
			xl<-"ONDJFM temperature averages (oC)"
			yl<-"ONDJFM precipitation averages (mm/day)"
		}
	}

	jpeg(filename=paste(figFile,"-TP.jpg",sep=""),width=480,height=480)

	plot(1,1,type='n',bty='n',
		xlim=c(xmin,xmax),ylim=c(ymin,ymax),
		xlab=xl,ylab=yl,
		main=paste(figTitle,location,"\r\nT and P from 20 GCMs",sep=", "))
	points(basT,basR,pch=8)

	abline(h=c(basR+thrR,basR-thrR),lty=2)
	abline(v=c(basT+thrT,basT-thrT),lty=2)
#	text((basT+thrT),(basR+thrR),labels="significance")

	for(i in GCMs){
# LETTERS
#		points(fut[1,which(i==GCMs)],fut[2,which(i==GCMs)],pch=i)
# disk/circles
		ifelse(any(i==fiveGCM_l),myPch<-i,myPch<-5)
		points(fut[1,which(i==GCMs)],fut[2,which(i==GCMs)],pch=myPch)
	}

#savePlot(paste(figFile,"-TP.jpg",sep=""),"jpeg");graphics.off()
dev.off()
}
