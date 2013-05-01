##
 # FILE AgMIP_fct.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
 ###############################################################################
# AgMIP specific fct

# INIT ALL OVER
source('/home/crespo/Desktop/Optimisation/xpos-r/ClimateDataTools/ClimFormats/dataRead.r')
source('/home/crespo/Desktop/Optimisation/xpos-r/ClimateDataTools/ClimFormats/dataWrite.r')
source('/home/crespo/Desktop/Optimisation/xpos-r/ClimateDataTools/ClimFormats/dataPerturbe.r')
source('/home/crespo/Desktop/Optimisation/xpos-r/ClimateDataTools/Climatology/climFuture.r')
source('/home/crespo/Desktop/Optimisation/xpos-r/ClimateDataTools/Climatology/climAgro.r')
#inFo <- '/home/crespo/Desktop/wine_shared/12_AgMIP/RZA-CLIP/D_RCP85_raw'
#outFo <- '/home/crespo/Desktop/wine_shared/12_AgMIP/RZA-CLIP/E_RCP85_split/Lambani'
inGCM <- '/home/crespo/Desktop/wine_shared/12_AgMIP/RZA-AMIIP/E_RCP85_split/Malmesbury'
outFo <- '/home/crespo/Desktop/wine_shared/12_AgMIP/RZA-AMIIP/F_fut85'
inObs <- '/home/crespo/Desktop/wine_shared/12_AgMIP/RZA-AMIIP/C_presCorrectedObs/Malmesbury/CSAG/Control'
oID <- '0041347.1.txt'


 ###############################################################################
 ###############################################################################
agmip_period<-function(per)
{
	switch(per,{	# 1 - control
			start<-as.Date("1980-01-01")
			end<-as.Date("2010-12-31")
			folder<-"1980_2010"
		},{	# 2 - near future
			start<-as.Date("2010-01-01")
			end<-as.Date("2040-12-31")
			folder<-"2010_2040"
		},{	# 3 - mid century
			start<-as.Date("2040-01-01")
			end<-as.Date("2070-12-31")
			folder<-"2040_2070"
		},{	# 4 - end century # do not take after 2094 for downscaled data
			# be carefull, not only the same number of year, but needs the same number of days (different number of leap years?)
			start<-as.Date("2068-12-31")	# start<-as.Date("2064-01-01")
			end<-as.Date("2099-12-31")	# end<-as.Date("2094-12-31")
			folder<-"2069_2099"		# folder<-"2064_2094"
		}
	)

return(list("start"=start,"end"=end,"folder"=folder))
}

 ###############################################################################
 ###############################################################################
# CMIP5 style
# split CMIP5 continuous into AgMIP periods
agmip_splitPeriods <- function(inFolder=inFo,outFolder=outFo)
{
	rcp_t <- list.files(inFolder)
	for(r in 1:length(rcp_t)){
		# not for ncep
#		if(r == grep("ncep",rcp_t)) next		
		tmpOut <- paste(outFolder,rcp_t[r],sep="/")	
		if(!file.exists(tmpOut))	dir.create(tmpOut,showWarnings=TRUE,recursive=FALSE,mode="0777")

		tmpIn <- paste(inFolder,rcp_t[r],sep="/")
		sta_t <- list.files(paste(tmpIn,"ppt",sep="/"))
		for(s in 1:length(sta_t)){
			print(paste("",rcp_t[r],sta_t[s],sep=" > "),quote=F)
			# read it
			metD <- read_oldCSAGformat(tmpIn,sta_t[s])				# requires dataRead.r
			# AgMIP periods are defined above
			con <- pert_period(metD,agmip_period(1)$start,agmip_period(1)$end)	# requires dataPerturbe.r
			fu1 <- pert_period(metD,agmip_period(2)$start,agmip_period(2)$end)	#
			fu2 <- pert_period(metD,agmip_period(3)$start,agmip_period(3)$end)	#
			fu3 <- pert_period(metD,agmip_period(4)$start,agmip_period(4)$end)	#
			# write it
			tmpOutCon <- paste(tmpOut,agmip_period(1)$folder,sep="/");	if(!file.exists(tmpOutCon))	dir.create(tmpOutCon,showWarnings=TRUE,recursive=FALSE,mode="0777")
			tmpOutFu1 <- paste(tmpOut,agmip_period(2)$folder,sep="/");	if(!file.exists(tmpOutFu1))	dir.create(tmpOutFu1,showWarnings=TRUE,recursive=FALSE,mode="0777")
			tmpOutFu2 <- paste(tmpOut,agmip_period(3)$folder,sep="/");	if(!file.exists(tmpOutFu2))	dir.create(tmpOutFu2,showWarnings=TRUE,recursive=FALSE,mode="0777")	
			tmpOutFu3 <- paste(tmpOut,agmip_period(4)$folder,sep="/");	if(!file.exists(tmpOutFu3))	dir.create(tmpOutFu3,showWarnings=TRUE,recursive=FALSE,mode="0777")	
			write_oldCSAGformat(con,tmpOutCon)					# requires dataWrite.r
			write_oldCSAGformat(fu1,tmpOutFu1)					# requires dataWrite.r
			write_oldCSAGformat(fu2,tmpOutFu2)					# requires dataWrite.r
			write_oldCSAGformat(fu3,tmpOutFu3)					# requires dataWrite.r
		}
	}

rm(rcp_t,r,tmpOut,sta_t,s,con,fu1,fu2,fu3,tmpOutCon,tmpOutFu1,tmpOutFu2,tmpOutFu3)
}

 ###############################################################################
 ###############################################################################
agmip_pertObs <- function(inFoGCM=inGCM,inFoObs=inObs,outFolder=outFo,obsID=oID,check=FALSE)
{
	# for every GCM-RCP
	rcp_t <- list.files(inFoGCM)
	for(r in 1:length(rcp_t)){
		print(paste("    > ",rcp_t[r],sep=""),quote=F)
		tmpOut1 <- paste(outFolder,rcp_t[r],sep="/")	
		if(!file.exists(tmpOut1))	dir.create(tmpOut1,showWarnings=TRUE,recursive=FALSE,mode="0777")

		# for every time period
		tmpIn1 <- paste(inFoGCM,rcp_t[r],sep="/")
		tPe_t <- list.files(tmpIn1)
		for(t in 1:length(tPe_t)){
			# which agmip period
			if(t == grep(agmip_period(1)$folder,tPe_t)) next	# no need to change control	
			if(t == grep(agmip_period(2)$folder,tPe_t)) agPeriod <- 2
			if(t == grep(agmip_period(3)$folder,tPe_t)) agPeriod <- 3
			if(t == grep(agmip_period(4)$folder,tPe_t)) agPeriod <- 4

			print(paste("",rcp_t[r]," > ",tPe_t[t],sep=""),quote=F)
			tmpOut2 <- paste(tmpOut1,tPe_t[t],sep="/")	
			if(!file.exists(tmpOut2))	dir.create(tmpOut2,showWarnings=TRUE,recursive=FALSE,mode="0777")
			tmpIn2 <- paste(tmpIn1,tPe_t[t],sep="/")

			# compute stat change for that GCM-RCP for that time period
			cha <- future_mtChange(read_oldCSAGformat(paste(tmpIn1,agmip_period(1)$folder,sep="/"),obsID),read_oldCSAGformat(tmpIn2,obsID))
			print(round(cha,digits=1))

			# perturb every stations in there
			sta_t <- list.files(paste(inFoObs,"ppt",sep="/"))
			for(s in 1:length(sta_t)){
				print(paste("",rcp_t[r]," > ",tPe_t[t]," > ",sta_t[s],sep=""),quote=F)
				# read it
				obsD <- read_oldCSAGformat(inFoObs,sta_t[s])				# requires dataRead.r

				# update dates
				obsD$period$start <- agmip_period(agPeriod)$start
				obsD$period$end <- agmip_period(agPeriod)$end

				# perturbe temp and rain
				obsD <- pert_temp(obsD,cha[1,],cha[2,],cha[4,],check)
				obsD <- pert_facRain(obsD,cha[3,],check)

				# update tav and amp
				obsD <- agro_tavamp(obsD)						# requires climAgro.r

				# check length (may be more/less leap years?)
				if (length(obsD$data$date)!=(difftime(obsD$period$end,obsD$period$start,units='days')+1)){
					if (length(obsD$data$date)==(difftime(obsD$period$end,obsD$period$start,units='days')+2)){
						print("#### you are trying to fit a 30 years period (01-01 until 12-31) with another 30 years period (01-01 until 12-31 as well) but with different amount of leap years so that it crashes. play with the start or end of the one you can play with",quote=F)
						browser()
	
					}else{
						print(paste("####","WARNING","data lenght issue in AgMIP_fct.r",sep=" > "),quote=F)
						browser()
					}
				}

				# write it
				write_oldCSAGformat(obsD,tmpOut2)					# requires dataWrite.r
			}
		}
	}

rm(rcp_t,r)
}

###############################################################################
###############################################################################
### AgMIP plots
agmip_plot <- function(metD_obs, metD_p1, metD_p2)
{
	x <- 1:length(metD_obs$data$date)
#	mainTitle <- "Bloemfontein - SA-AMIIP fast track - 30 years"
	mainTitle <- "Nkayi - SA-CLIP fast track - 30 years"

	# tmin
#	plot(	x,y=metD_p2$data$tmin,type="l", lwd=.5, col="red", xlab="day", ylab="Minimum daily temperatures (oC)",main=mainTitle)#
#	lines(	x,y=metD_p1$data$tmin,type="l", lwd=.5, col="green")
#	lines(	x,y=metD_obs$data$tmin,type="l", lwd=.5, col="blue")
	# tmax
#	plot(	x,y=metD_p2$data$tmax,type="l", lwd=.5, col="red", xlab="day", ylab="Maximum daily temperatures (oC)",main=mainTitle)
#	lines(	x,y=metD_p1$data$tmax,type="l", lwd=.5, col="green")
#	lines(	x,y=metD_obs$data$tmax,type="l", lwd=.5, col="blue")
	# legend
#	legend(	"bottomright",
#		legend=c("observations (1980-2010)","RCP45-BNU (2040-2070)","RCP85-BNU (2040-2070)"),
#		col=c("blue","green","red"),
#		lty=c(1,1,1)
#	);

	# rain-obs
#	plot(	x,y=metD_p2$data$rain,type="l", lwd=.5, col="blue", xlab="day", ylab="Daily rainfall (mm)",main=mainTitle)
#	legend(	"topright",
#		legend=c("observations (1980-2010)"),
#		col=c("blue"),
#		lty=c(1)
#	);

	# rain - difference
	plot(	x,y=(metD_p2$data$rain-metD_obs$data$rain),type="l", lwd=.5, col="red", xlab="day", ylab="Daily rainfall difference from baseline (mm)",main=mainTitle)
	lines(	x,y=(metD_p1$data$rain-metD_obs$data$rain),type="l", lwd=.5, col="green")
	legend(	"topright",
		legend=c("RCP45-BNU (2040-2070)","RCP85-BNU (2040-2070)"),
		col=c("green","red"),
		lty=c(1,1)
	);

}


 ###############################################################################
 ###############################################################################
### TMP
## TRANSLATE
translate <- function(inFolder=inFo,outFolder=outFo)
{
	lat <- -18.75
	lon <- 28.75
	alt <- 1200
	col <- 5
	id <- 'nkayi'
	comm <-'GCM watch grid -18.5,-19 28.5,29'
	sDate <- as.Date('1960-01-01')
	eDate <- as.Date('2100-12-31')

	fiName<-list.files(paste(inFolder,'ppt',sep="/"))
	stName <-strsplit(fiName,'\\.')
	for (f in 1:length(fiName)){
		gcm <- stName[[f]][5]
		outTmp <- paste(outFolder,gcm,sep="/")
		if(!file.exists(outTmp))	dir.create(outTmp,showWarnings=TRUE,recursive=FALSE,mode="0777")

		metD <- read_ASCIIformat(inFolder,gcm,col,lat,lon,alt,sDate,eDate,id,comm)
		write_oldCSAGformat(metD,outTmp)
	}


rm(lat,lon,alt,col,comm,sDate,eDate,gcm,id,f,metD)
}

