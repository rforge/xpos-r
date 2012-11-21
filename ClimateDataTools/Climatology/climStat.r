
# need climTools.r


### MEANS
########################################################################

# compute the annual mean
# over years with no more than maxMV% (maxMV: maximum missing value percentage) missing data per year
# data format defined in readFormats.r
stat_annualMeans <- function(data,maxMV=10)
{
	aMeans <- NULL
	# I just compute it all, even if computing the date does not make any sense
	for (v in 1:length(data)){
		tmp <- array(NA,dim=(1+data$yyyy[length(data$yyyy)]-data$yyyy[1]))
		y<-data$yyyy[1]
		while(y<=data$yyyy[length(data$yyyy)]){
			fullY <- array(NA,dim=as.numeric(julian(as.Date(paste(y,"12","31",sep="-")),origin=as.Date(paste(y,"01","01",sep="-"))))+1)
			fullY[1:length(data[[v]][data$yyyy==y])] <- data[[v]][data$yyyy==y]
			mMV <- maxMV*length(data[[v]][data$yyyy==y])/100
			## only if less than maxMV% missing data per set
			if(length(fullY[is.na(fullY)])<=mMV){
				c <- 1+y-data$yyyy[1]
				tmp[c] <- mean(fullY,na.rm=T)
			}
		y<-y+1
		}
		aMeans <- c(aMeans,list(tmp))
		names(aMeans)[length(aMeans)] <- names(data)[v]
	}

return(aMeans)
# rm tmp objects
rm(tmp,v,y,maxMV,mMV,c)
}

# compute the monthly mean
# over months with no more than maxMV% (maxMV: maximum missing value percentage) missing data per month
# data format defined in readFormats.r
stat_monthlyMeans <- function(data,maxMV=10)
{
	mMeans <- NULL
	# I just compute it all, even if computing the date does not make any sense
	for (v in 1:length(data)){
		tmp <- array(NA,dim=(1+data$yyyy[length(data$yyyy)]-data$yyyy[1])*12)
		y<-data$yyyy[1]
		while(y<=data$yyyy[length(data$yyyy)]){
			m<-data$mm[data$yyyy==y][1]
			while(m<=data$mm[data$yyyy==y][length(data$mm[data$yyyy==y])]){
				fullM <- array(NA,dim=as.numeric(julian(as.Date(paste(y,m,maxNo_days(y,m),sep="-")),origin=as.Date(paste(y,m,"01",sep="-"))))+1)
				fullM[1:length(data[[v]][data$mm==m & data$yyyy==y])] <- data[[v]][data$mm==m & data$yyyy==y]
				mMV <- maxMV*length(data[[v]][data$mm==m & data$yyyy==y])/100
				## only if less than maxMV% missing data per set
				if(length(fullM[is.na(fullM)])<=mMV){
					c <- 12*(y-data$yyyy[1])+m
					tmp[c] <- mean(fullM,na.rm=T)
				}
			m<-m+1
			}
		y<-y+1
		}
		mMeans <- c(mMeans,list(tmp))
		names(mMeans)[length(mMeans)] <- names(data)[v]
	}

return(mMeans)
# rm tmp objects
rm(tmp,v,y,m,maxMV,mMV,c)
}

# compute the winWD time-window mean (winWidth: window width)
# over months with no more than maxMV% (maxMV: maximum missing value percentage) missing data per time-window
# data format defined in readFormats.r
stat_windowMeans <- function(data,maxMV=10,winWidth=29)
{
	wMeans <- NULL
	# winWidth-1 because I'm gonna look at one day -aHalfWidth and +pHalfWidth, hence a aHalfWidth+1+pHalfWidth wide window
	aHalfWidth <- floor((winWidth-1)/2)
	pHalfWidth <- (winWidth-1) - aHalfWidth

	# I just compute it all, even if computing the date does not make any sense
	for (v in 1:length(data)){
		tmp <- array(NA,dim=length(data$date))
		for(l in 1:length(data$date)){
			mMV <- maxMV*winWidth/100
			## only if less than maxMV% missing data per set
			windowA <- array(NA,dim=aHalfWidth+1)
			windowP <- array(NA,dim=pHalfWidth+1)
			ifelse(aHalfWidth>=l,windowA[(length(windowA)-length(data[[v]][1:l])+1):length(windowA)]<-data[[v]][1:l],windowA<-data[[v]][(l-aHalfWidth):l])
			ifelse(pHalfWidth>=(length(data$date)-l),windowP[1:(length(data$date)-l+1)]<-data[[v]][l:length(data$date)],windowP<-data[[v]][l:(l+pHalfWidth)])
			window <- c(windowA,windowP[2:length(windowP)])
			if(length(window[is.na(window)])<=mMV){
				tmp[l] <- mean(window,na.rm=T)
			}
		}
		wMeans <- c(wMeans,list(tmp))
		names(wMeans)[length(wMeans)] <- names(data)[v]
	}

return(wMeans)
# rm tmp objects
rm(tmp,aHalfWidth,pHalfWidth,v,l,maxMV,mMV,windowA,windowP,window)
}

### TOTALS
########################################################################

# compute the annual totals
# over years with no more than maxMV% (maxMV: percentage upper limit) missing data per year
# data format defined in readFormats.r
stat_annualTotals <- function(data,maxMV=5)
{
	aTotals <- NULL
	# I just compute it all, even if computing the date does not make any sense
	for (v in 1:length(data)){
		tmp <- array(NA,dim=(1+data$yyyy[length(data$yyyy)]-data$yyyy[1]))
		y<-data$yyyy[1]
		while(y<=data$yyyy[length(data$yyyy)]){
			fullY <- array(NA,dim=as.numeric(julian(as.Date(paste(y,"12","31",sep="-")),origin=as.Date(paste(y,"01","01",sep="-"))))+1)
			fullY[1:length(data[[v]][data$yyyy==y])] <- data[[v]][data$yyyy==y]
			mMV <- maxMV*length(data[[v]][data$yyyy==y])/100
			## only if less than maxMV% missing data per set
			if(length(fullY[is.na(fullY)])<=mMV){
				c <- 1+y-data$yyyy[1]
				tmp[c] <- sum(fullY,na.rm=T)
			}
		y<-y+1
		}
		aTotals <- c(aTotals,list(tmp))
		names(aTotals)[length(aTotals)] <- names(data)[v]
	}

return(aTotals)
# rm tmp objects
rm(tmp,v,y,maxMV,mMV,c)
}

# compute the monthly totals
# over months with no more than maxMV% (maxMV: maximum missing value percentage) missing data per month
# data format defined in readFormats.r
stat_monthlyTotals <- function(data,maxMV=5)
{
	mTotals <- NULL
	# I just compute it all, even if computing the date does not make any sense
	for (v in 1:length(data)){
		tmp <- array(NA,dim=(1+data$yyyy[length(data$yyyy)]-data$yyyy[1])*12)
		y<-data$yyyy[1]
		while(y<=data$yyyy[length(data$yyyy)]){
			m<-data$mm[data$yyyy==y][1]
			while(m<=data$mm[data$yyyy==y][length(data$mm[data$yyyy==y])]){
				fullM <- array(NA,dim=as.numeric(julian(as.Date(paste(y,m,maxNo_days(y,m),sep="-")),origin=as.Date(paste(y,m,"01",sep="-"))))+1)
				fullM[1:length(data[[v]][data$mm==m & data$yyyy==y])] <- data[[v]][data$mm==m & data$yyyy==y]
				mMV <- maxMV*length(data[[v]][data$mm==m & data$yyyy==y])/100
				## only if less than maxMV% missing data per set
				if(length(fullM[is.na(fullM)])<=mMV){
					c <- 12*(y-data$yyyy[1])+m
					tmp[c] <- sum(fullM,na.rm=T)
				}
			m<-m+1
			}
		y<-y+1
		}
		mTotals <- c(mTotals,list(tmp))
		names(mTotals)[length(mTotals)] <- names(data)[v]
	}

return(mTotals)
# rm tmp objects
rm(tmp,v,y,m,maxMV,mMV,c)
}

# compute the winWD time-window mean (winWidth: window width)
# over months with no more than maxMV% (maxMV: maximum missing value percentage) missing data per time-window
# data format defined in readFormats.r
stat_windowTotals <- function(data,maxMV=5,winWidth=29)
{
	wTotals <- NULL
	# winWidth-1 because I'm gonna look at one day -aHalfWidth and +pHalfWidth, hence a aHalfWidth+1+pHalfWidth wide window
	aHalfWidth <- floor((winWidth-1)/2)
	pHalfWidth <- (winWidth-1) - aHalfWidth

	# I just compute it all, even if computing the date does not make any sense
	for (v in 1:length(data)){
		tmp <- array(NA,dim=length(data$date))
		for(l in 1:length(data$date)){
			mMV <- maxMV*winWidth/100
			## only if less than maxMV% missing data per set
			windowA <- array(NA,dim=aHalfWidth+1)
			windowP <- array(NA,dim=pHalfWidth+1)
			ifelse(aHalfWidth>=l,windowA[(length(windowA)-length(data[[v]][1:l])+1):length(windowA)]<-data[[v]][1:l],windowA<-data[[v]][(l-aHalfWidth):l])
			ifelse(pHalfWidth>=(length(data$date)-l),windowP[1:(length(data$date)-l+1)]<-data[[v]][l:length(data$date)],windowP<-data[[v]][l:(l+pHalfWidth)])
			window <- c(windowA,windowP[2:length(windowP)])
			if(length(window[is.na(window)])<=mMV){
				tmp[l] <- sum(window,na.rm=T)
			}
		}
		wTotals <- c(wTotals,list(tmp))
		names(wTotals)[length(wTotals)] <- names(data)[v]
	}

return(wTotals)
# rm tmp objects
rm(tmp,aHalfWidth,pHalfWidth,v,l,maxMV,mMV,windowA,windowP,window)
}

### QUANTILES
########################################################################

### OBSOLETE
##############################################################################
temp_quantiles <- function(metDat,figTit=figTitDef)
{
	yeaC <- metDat$yeaC;
	julC <- metDat$julC;
	tmnC <- metDat$tmnC;
	tmxC <- metDat$tmxC;
	pptC <- metDat$pptC;
	solC <- metDat$solC;

	## put NA on 29-FEB when leap year
	met365<-metDat$data;
	remove29feb <- FALSE;
	for(l in seq(dim(met365)[1],1,-1)){
		if(met365[l,julC]==366){
			remove29feb <- TRUE;
		}
		if(remove29feb==TRUE){
			if(met365[l,julC]==(31+29)){
				met365[l,julC]<-NA;
				remove29feb <- FALSE;
			}else{
				ifelse(l>1,met365[l,julC]<-met365[l-1,julC],met365[l,julC]<-(met365[l,julC]-1));
			}
		}
	}

	# compute quantiles
	quaTmn <- array(NA,dim=c(365,3));
	quaTmx <- array(NA,dim=c(365,3));
	xDay<-1:365;
	for (day in xDay){
		quaTmn[day,]<-quantile(met365[met365[,julC]==day,tmnC],na.rm=T,probs=c(0.2,0.5,0.8));
		quaTmx[day,]<-quantile(met365[met365[,julC]==day,tmxC],na.rm=T,probs=c(0.2,0.5,0.8));
	}

	# plot
	x11(width=11,height=8);
	plot.new();

	par(mar=c(5,5,2,0));
	plot(	x=xDay,
		y=xDay,
		type="n",
		xlim=c(1,365),	# X limit
		ylim=c(min(quaTmn[,1],na.rm=T),max(quaTmx[,3],na.rm=T)),
		axes=FALSE, xlab="Day", ylab="Temperature (oC)"
	);
	title(xlab=NULL,ylab=NULL,main=figTit);
	axis(1,	at=c(m00,m01,m02,m03,m04,m05,m06,m07,m08,m09,m10,m11,m12),
		labels=c("01-jan","31-jan","28-feb","31-mar","30-apr","31-may","30-jun","31-jul","31-aug","30-sep","31-oct","30-nov","31-dec")
	);
	axis(2,at=NULL); grid(nx=NA,ny=NULL);
	legend(	"bottomleft",
		legend=c("80th","50th max temp","20th","80th","50th min temp","20th",paste("na.rm=(",length(met365[is.na(met365[,tmnC]),tmnC]),",",length(met365[is.na(met365[,tmxC]),tmxC]),")",sep="")),
		col=c("red","red","red","blue","blue","blue","white"),
		lty=c(3,1,3,3,1,3,NULL),
		lwd=c(1,1,1,1,1,1,NULL)
	);

	lines(xDay,quaTmn[,1],type="l",lty=3,col="blue");
	lines(xDay,quaTmn[,2],type="l",lty=1,col="blue");
	lines(xDay,quaTmn[,3],type="l",lty=3,col="blue");
#	lines(lowess(quaTmn[,2],f=1/12,iter=3),type="l",lty=1,lwd=2,col="black");	

	lines(xDay,quaTmx[,1],type="l",lty=3,col="red");
	lines(xDay,quaTmx[,2],type="l",lty=1,col="red");
	lines(xDay,quaTmx[,3],type="l",lty=3,col="red");
#	lines(lowess(quaTmx[,2],f=1/12,iter=3),type="l",lty=1,lwd=2,col="black");	
}

##############################################################################
prec_perDay <- function(metDat,figTit=figTitDef)
{
	yeaC <- metDat$yeaC;
	julC <- metDat$julC;
	tmnC <- metDat$tmnC;
	tmxC <- metDat$tmxC;
	pptC <- metDat$pptC;
	solC <- metDat$solC;
	figTit <- paste("Precipitation (mm)",figTit,sep=" - ");

	## put NA on 29-FEB when leap year
	met365<-metDat$data;
	remove29feb <- FALSE;
	for(l in seq(dim(met365)[1],1,-1)){
		if(met365[l,julC]==366){
			remove29feb <- TRUE;
		}
		if(remove29feb==TRUE){
			if(met365[l,julC]==(31+29)){
				met365[l,julC]<-NA;
				remove29feb <- FALSE;
			}else{
				ifelse(l>1,met365[l,julC]<-met365[l-1,julC],met365[l,julC]<-(met365[l,julC]-1));
			}
		}
	}
	# remove 0s
	met365[met365[,pptC]==0,pptC] <- NA;

	# compute quantiles
	quaPpt <- array(NA,dim=c(365,3));
	xDay<-1:365;
	for (day in xDay){
		quaPpt[day,]<-quantile(met365[met365[,julC]==day,pptC],na.rm=TRUE,probs=c(0.5,0.8,1));
	}

	# plot
	x11(width=11,height=8);
	plot.new();
	par(mar=c(3,3,1,1));
	plot(	x=xDay,
		y=xDay,
		type="n",
		xlim=c(1,365),	# X limit
		ylim=c(0,quantile(quaPpt[,3],probs=0.995,na.rm=TRUE)),
		axes=FALSE
	);
	title(xlab=NULL,ylab=NULL,main=figTit);
	axis(1,	at=c(m00,m01,m02,m03,m04,m05,m06,m07,m08,m09,m10,m11,m12),
		labels=c("01-jan","31-jan","28-feb","31-mar","30-apr","31-may","30-jun","31-jul","31-aug","30-sep","31-oct","30-nov","31-dec")
	);
	axis(2,at=NULL);
	legend(	"center",
		legend=c("max precipitation","80th precipitation","50th precipitation"),
		col=c("lightblue","lightblue","blue"),
		lty=c(3,1,1),
		lwd=c(1,2,1)
	);

#	lines(xDay,quaPpt[,1],type="l",lty=2,col="blue");
	lines(xDay,quaPpt[,3],type="h",lty=3,col="lightblue");
	lines(xDay,quaPpt[,2],type="h",lwd=2,col="lightblue");
	lines(xDay,quaPpt[,1],type="h",lwd=1,col="blue");

	# replace NAs with 0s
	quaPpt[is.na(quaPpt[,1]),1] <-0;
	quaPpt[is.na(quaPpt[,2]),2] <-0;
	quaPpt[is.na(quaPpt[,3]),3] <-0;
#	lines(lowess(quaPpt[,3],f=1/12,iter=3),type="l",lty=3,col="lightblue");	
#	lines(lowess(quaPpt[,2],f=1/12,iter=3),type="l",lty=1,col="lightblue");	
#	lines(lowess(quaPpt[,1],f=1/12,iter=3),type="l",lty=1,col="blue");	
}

##############################################################################
prec_perMonth <- function(metDat,figTit=figTitDef)
{
	yeaC <- metDat$yeaC;
	julC <- metDat$julC;
	tmnC <- metDat$tmnC;
	tmxC <- metDat$tmxC;
	pptC <- metDat$pptC;
	solC <- metDat$solC;
	figTit <- paste("Precipitation (mm)",figTit,sep=" - ");

	# separate data per month
	monthData <- 	list(jan=NULL,feb=NULL,mar=NULL,apr=NULL,may=NULL,jun=NULL,jul=NULL,aug=NULL,sep=NULL,oct=NULL,noc=NULL,dec=NULL);
	monthWas <- 12; year <- 0;
	for (l in 1:dim(metDat$data)[1]){
		month <- as.numeric(format(as.Date(metDat$data[l,julC],origin=paste(metDat$data[l,yeaC],"01-01",sep="-")),"%m"));
		if(monthWas != month){
			# create a new year in that month
			if(monthWas==12){	year <- year + 1;}
			monthData[[month]] <- c(monthData[[month]],list("yea"=NULL));
			monthData[[month]][[year]] <- list("dat"=NULL,"no0"=NULL,"tot"=NULL);
		}
		monthWas <- month;
		monthData[[month]][[year]]$dat <- c(monthData[[month]][[year]]$dat,metDat$data[l,pptC]);
	}

	# compute monthly totals
	for(m in 1:length(monthData)){
		for(y in 1:length(monthData[[m]])){
			# no0
			monthData[[m]][[y]]$no0 <- array(NA,dim=length(monthData[[m]][[y]]$dat));
			for(l in 1:length(monthData[[m]][[y]]$dat)){
				if(monthData[[m]][[y]]$dat[l]!=0){monthData[[m]][[y]]$no0[l] <- monthData[[m]][[y]]$dat[l];}
			}
			monthData[[m]][[y]]$tot <- sum(monthData[[m]][[y]]$no0,na.rm=TRUE);
		}
	}

	# put it in matrix for plotting
	monthStat <- array(0,dim=c(6,12));
	for(m in 1:length(monthData)){
		tmp <- array(NA,dim=length(monthData[[m]]));
		for(y in 1:length(monthData[[m]])){
			monthStat[1,m] <- monthStat[1,m]+ifelse(is.na(monthData[[m]][[y]]$tot),0,monthData[[m]][[y]]$tot);
			tmp[y] <- ifelse(is.na(monthData[[m]][[y]]$tot),0,monthData[[m]][[y]]$tot);
		}
		monthStat[1,m] <- monthStat[1,m]/length(monthData[[m]]);
		monthStat[2:6,m] <- quantile(tmp,probs=c(0,0.2,0.5,0.8,1))
#		for(q in 2:6){
#			monthStat[q,m] <- monthStat[q,m]/length(monthData[[m]]);
#		}
	}
	aaa <-boxplot(monthStat[2:6,],plot=FALSE);
	aaa$stats[1:5,] <- monthStat[2:6,];

	# plot
	xMon <- 1:12;
	x11(width=11,height=8);
	plot.new();
	par(mar=c(3,3,2,1));
	bxp(aaa,outline=FALSE,boxfill="lightblue",names=FALSE,axes=FALSE,wiskcol="blue")
	title(xlab=NULL,ylab=NULL,main=figTit);
	axis(1,	at=c(1,2,3,4,5,6,7,8,9,10,11,12),
		labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
	);
	axis(2,at=NULL);
	legend(	"top",
		legend=c("max monthly totals","80th monthly totals","50th monthly totals","20th monthly totals","min monthly totals","mean monthly totals"),
		col=c("black","lightblue","black","lightblue","black","red"),
		lty=c(1,1,1,1,1,1),
		lwd=c(1,2,2,2,1,2)
	);

	tmp <- array(NA,dim=(length(monthStat[1,])+2));
	tmp[2:(length(monthStat[1,])+1)] <- monthStat[1,]; 
	tmp[1] <- monthStat[1,length(monthStat[1,])];
	tmp[(length(monthStat[1,])+2)] <- monthStat[1,1];
#	lines(xMon,monthStat[1,],type="l",lty=1,lwd=2,col="red");
	lines(0:13,tmp,type="l",lty=1,lwd=2,col="red");
}

##############################################################################
prec_runMonth <- function(metDat,figTit=figTitDef,noRainThreshold=0,windowSemiWidth=15,rDayFac=10)
{
	yeaC <- metDat$yeaC;
	julC <- metDat$julC;
	tmnC <- metDat$tmnC;
	tmxC <- metDat$tmxC;
	pptC <- metDat$pptC;
	solC <- metDat$solC;
	metDat$data <- as.matrix(metDat$data);

	# compute totals
	metCol <- dim(metDat$data)[2];
	winTot <- array(NA,dim=c(dim(metDat$data)[1],(metCol+2)));
	winTot[,1:metCol] <- as.numeric(metDat$data[,]);
	for(d in (windowSemiWidth+1):(dim(metDat$data)[1]-windowSemiWidth)){
		window <- array(NA,dim=(windowSemiWidth+windowSemiWidth+1));
		window[] <- as.numeric(metDat$data[(d-windowSemiWidth):(d+windowSemiWidth),pptC]);
		winTot[d,(metCol+1)] <- length(window[window[]>noRainThreshold]);
		winTot[d,(metCol+2)] <- sum(window[window[]>noRainThreshold]);
	}

	## put NA on 29-FEB when leap year
	met365<-winTot;
	remove29feb <- FALSE;
	for(l in seq(dim(met365)[1],1,-1)){
		if(met365[l,julC]==366){
			remove29feb <- TRUE;
		}
		if(remove29feb==TRUE){
			if(met365[l,julC]==(31+29)){
				met365[l,julC]<-NA;
				remove29feb <- FALSE;
			}else{
				ifelse(l>1,met365[l,julC]<-met365[l-1,julC],met365[l,julC]<-(met365[l,julC]-1));
			}
		}
	}

	# compute quantiles
	quaPpt <- array(NA,dim=c(365,5));
	xDay<-1:365;
	for (day in xDay){
		quaPpt[day,1]<-mean(met365[met365[,julC]==day,(metCol+1)],na.rm=T);
		quaPpt[day,2]<-mean(met365[met365[,julC]==day,(metCol+2)],na.rm=T);
		quaPpt[day,3:5]<-quantile(met365[met365[,julC]==day,(metCol+2)],na.rm=T,probs=c(0.2,0.5,0.8));
	}

	# plot
	xDay <- 1:365
	x11(width=11,height=8);
	plot.new();
	par(mar=c(5,5,2,5));
	plot(	x=xDay,
		y=xDay,
		type="n",
		ylim=c(0,max(quaPpt[,5],na.rm=TRUE)),
		axes=FALSE, xlab="Day", ylab="Precipitation (mm)"
	);
	mtext(text="Number of day",side=4,line=3)
	title(xlab=NULL,ylab=NULL,main=figTit);
	axis(1,	at=c(m00,m01,m02,m03,m04,m05,m06,m07,m08,m09,m10,m11,m12),
		labels=c("01-jan","31-jan","28-feb","31-mar","30-apr","31-may","30-jun","31-jul","31-aug","30-sep","31-oct","30-nov","31-dec")
	);
	axis(2,at=NULL,	col="blue"); grid(nx=NA,ny=NULL);
	axis(4,	at=seq(0,max(quaPpt[,5],na.rm=TRUE),5*rDayFac),
		labels=seq(0,max(quaPpt[,5],na.rm=TRUE)/rDayFac,5),
		col="red"
	);
	winWidth<-windowSemiWidth*2+1;
	legend(	"top",
		legend=c("80th",paste("50th ",winWidth," days totals",sep=""),"20th",paste("mean ",winWidth," days totals",sep=""),"mean # of rainy day",paste("na.rm = ",length(met365[is.na(met365[,pptC]),pptC]),sep="")),
		col=c("blue","blue","blue","black","red","white"),
		lty=c(3,1,3,1,1,NULL),
		lwd=c(2,2,2,1,1,NULL)
	);
	winWidth<-windowSemiWidth*2+1;
	legend(	"top",
		legend=c("80th",paste("50th ",winWidth," days totals",sep=""),"20th",paste("mean ",winWidth," days totals",sep=""),"mean # of rainy day",paste("na.rm = ",length(met365[is.na(met365[,pptC]),pptC]),sep="")),
		col=c("blue","blue","blue","black","red","white"),
		lty=c(3,1,3,1,1,NULL),
		lwd=c(2,2,2,1,1,NULL)
	);

	lines(xDay,(quaPpt[,1]*rDayFac),type="h",lty=1,lwd=1,col="pink");
	lines(xDay,quaPpt[,2],type="l",lty=1,lwd=1,col="black");
	lines(xDay,quaPpt[,3],type="l",lty=3,lwd=2,col="blue");
	lines(xDay,quaPpt[,4],type="l",lty=1,lwd=2,col="blue");
	lines(xDay,quaPpt[,5],type="l",lty=3,lwd=2,col="blue");
#	lines(lowess(quaPpt[,4],f=1/16,iter=3),type="l",lty=1,col="red");
# that's cool but need a right hand scale

}
