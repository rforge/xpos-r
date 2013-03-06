# INIT ALL OVER
source('/home/crespo/Desktop/Optimisation/xpos-r/ClimateDataTools/ClimFormats/dataRead.r')
source('/home/crespo/Desktop/Optimisation/xpos-r/ClimateDataTools/Climatology/climStat.r')
outFo <- '/home/crespo/Desktop/09_WRC/Metfiles/A2/Graphics/VredendalB1'
inGCM <- '/home/crespo/Desktop/09_WRC/Metfiles/B1/VredendalDscl'
loc <- 'Vredendal'

plotClim <- function(inFoGCM=inGCM,outFolder=outFo)
{
	# for every GCM-RCP
	rcp_t <- list.files(inFoGCM)
	for(r in 1:length(rcp_t)){
		print(paste("    > ",rcp_t[r],sep=""),quote=F)
		tmpIn1 <- paste(inFoGCM,rcp_t[r],sep="/")

		# for every stations
		sta_t <- list.files(paste(tmpIn1,"ppt",sep="/"))
		if(length(sta_t)==0) next			
		for(s in 1:length(sta_t)){
			print(paste("",rcp_t[r]," > ",sta_t[s],sep=""),quote=F)

			# read it
			obsD <- read_oldCSAGformat(tmpIn1,sta_t[s])	# requires dataRead.r
		
			# compute totals
			s_wt <- stat_windowTotals(obsD$data,maxMV=5,winWidth=31)

			# compute stat to plot
			quaTmn <- array(NA,dim=c(365,3));
			quaTmx <- array(NA,dim=c(365,3));
			quaPpt <- array(NA,dim=c(365,3));
			for (day in 1:365){
				m <- as.numeric(format(as.Date(day,origin="1999-01-01"),"%m"))
				d <- as.numeric(format(as.Date(day,origin="1999-01-01"),"%d"))
				quaTmn[day,]<-quantile(obsD$data$tmin[obsD$data$mm==m & obsD$data$dd==d],na.rm=T,probs=c(0.2,0.5,0.8))
				quaTmx[day,]<-quantile(obsD$data$tmax[obsD$data$mm==m & obsD$data$dd==d],na.rm=T,probs=c(0.2,0.5,0.8))
				quaPpt[day,]<-quantile(s_wt$rain[s_wt$mm==m & s_wt$dd==d],na.rm=T,probs=c(0.5,0.8,1))
			}

			# plot stat
			tit<-paste(loc,rcp_t[r],obsD$period$start,obsD$period$end,sep=", ")
			fil<-paste(loc,rcp_t[r],'temp',sep="_")
			outFi<-paste(outFo,fil,sep="/")
			plotTemp(obsD$data,quaTmn,quaTmx,tit,outFi)
			fil<-paste(loc,rcp_t[r],'rain',sep="_")
			outFi<-paste(outFo,fil,sep="/")
			plotRain(obsD$data,quaPpt,tit,outFi)
		}
	}

rm(rcp_t,r)
}

plotTemp <- function(metD,quaTmn,quaTmx,figTit,outFi)
{
	graphics.off()

	# plot
	x11(width=11,height=8)
	plot.new()

	xDay<-1:365;
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
		legend=c("80th","50th max temp","20th","80th","50th min temp","20th",paste("na.rm=(",length(metD$tmin[is.na(metD$tmin)]),",",length(metD$tmax[is.na(metD$tmax)]),")",sep="")),
		col=c("red","red","red","blue","blue","blue","white"),
		lty=c(3,1,3,3,1,3,NULL),
		lwd=c(1,1,1,1,1,1,NULL)
	);

	#tmin
	lines(xDay,quaTmn[,1],type="l",lty=3,col="blue");
	lines(xDay,quaTmn[,2],type="l",lty=1,col="blue");
	lines(xDay,quaTmn[,3],type="l",lty=3,col="blue");
#	lines(lowess(quaTmn[,2],f=1/12,iter=3),type="l",lty=1,lwd=2,col="black");	

	#tmax
	lines(xDay,quaTmx[,1],type="l",lty=3,col="red");
	lines(xDay,quaTmx[,2],type="l",lty=1,col="red");
	lines(xDay,quaTmx[,3],type="l",lty=3,col="red");
#	lines(lowess(quaTmx[,2],f=1/12,iter=3),type="l",lty=1,lwd=2,col="black");

	copyDev2eps(file=paste(outFi,".eps",sep=""))
	copyDev2jpg(file=paste(outFi,".jpg",sep=""))
}

plotRain <- function(metD,quaPpt,figTit,outFi)
{
	graphics.off()

	# plot
	x11(width=11,height=8)
	plot.new()

	xDay<-1:365;
	par(mar=c(5,5,2,0));
	plot(	x=xDay,
		y=xDay,
		type="n",
		xlim=c(1,365),	# X limit
		ylim=c(min(quaPpt[,1],na.rm=T),max(quaPpt[,3],na.rm=T)),
		axes=FALSE, xlab="Day", ylab="Rainfall (mm)"
	);
	title(xlab=NULL,ylab=NULL,main=figTit);
	axis(1,	at=c(m00,m01,m02,m03,m04,m05,m06,m07,m08,m09,m10,m11,m12),
		labels=c("01-jan","31-jan","28-feb","31-mar","30-apr","31-may","30-jun","31-jul","31-aug","30-sep","31-oct","30-nov","31-dec")
	);
	axis(2,at=NULL); grid(nx=NA,ny=NULL);
	legend(	"top", title="31 day totals",
		legend=c("max precipitation","80th precipitation","50th precipitation"),
		col=c("lightblue","lightblue","blue"),
		lty=c(3,1,1),
		lwd=c(1,2,1)
	);

	lines(xDay,quaPpt[,3],type="h",lty=3,col="lightblue");
	lines(xDay,quaPpt[,2],type="h",lwd=2,col="lightblue");
	lines(xDay,quaPpt[,1],type="h",lwd=1,col="blue");

	copyDev2eps(file=paste(outFi,".eps",sep=""))
	copyDev2jpg(file=paste(outFi,".jpg",sep=""))
}
