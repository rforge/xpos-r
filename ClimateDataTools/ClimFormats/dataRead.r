##
 # FILE dataRead.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
 ###############################################################################

## all format get out as
#list(	"file"=path to the file
#	"station"=list("id","lat","lon","alt","comm")
#	"clim"=list("tav","amp","refht","wndht")
#	"period"=list("start","end","type")	# type is CSAG style {0-real,1-365,2-360 days per year}
#	"data"=list("date","yyyy","mm","dd","juld","tmin","tmax","rain","srad","wind","dewp","vprs","rhum")
# MISSING VALUES = NA

########################################################################
## create empty forma
createNULLlist <- function()
{
	file <- 	NULL;
	station <- 	list("id"=NULL,"lat"=NULL,"lon"=NULL,"alt"=NULL,"comm"=NULL)
	clim <- 	list("tav"=NULL,"amp"=NULL,"refht"=NULL,"wndht"=NULL)
	period <- 	list("start"=NULL,"end"=NULL,"type"=NULL)
	data <- 	list("date"=NULL,"yyyy"=NULL,"mm"=NULL,"dd"=NULL,"juld"=NULL,"tmin"=NULL,"tmax"=NULL,"rain"=NULL,"srad"=NULL,"wind"=NULL,"dewp"=NULL,"vprs"=NULL,"rhum"=NULL)

	l <- 	list("file"=file,"station"=station,"clim"=clim,"period"=period,"data"=data)
return(l)
}

########################################################################
##### AGMIP
########################################################################
# input:
# path to the file to read in
#
# output:
# list filled up acording to empty list above
#########################################################################
#inFi<-'/home/crespo/Desktop/12_AgMIP/2012-10-01_fastTrack/AMIP/MerraData_CM/SABA0QXX.AgMIP'
# inFi<-'/home/crespo/Desktop/Link\ to\ WinShared/12_AgMIP/2012-10-01_fastTrack/AMIP/MerraData_CM/SABA0QXX.AgMIP'
## AgMIP
read_AgMIPformat <- function(inFile=inFi)
{
	# read AgMIP format data
	data <-	scan(inFile,skip=5)
	data <- matrix(data,ncol=12,byrow=T)
	colnames(data) <- scan(inFile,what='raw',skip=4,nlines=1)

	# read header
	head <- scan(inFile,what='raw',skip=3,nlines=1)
	head <- matrix(head,ncol=8,byrow=T)
	nam <- scan(inFile,what='raw',skip=2,nlines=1)
	colnames(head) <- nam[2:length(nam)]

	agmip <- createNULLlist()
	agmip$file <- 	inFile

	agmip$station$id <- 	head[colnames(head)=="INSI"]
	agmip$station$lat <- 	as.numeric(head[colnames(head)=="LAT"])
	agmip$station$lon <- 	as.numeric(head[colnames(head)=="LONG"])
	agmip$station$alt <- 	as.numeric(head[colnames(head)=="ELEV"])
	agmip$station$comm <- 	scan(inFile,what='character',sep="\n",nlines=1)

	agmip$clim$tav <- 	as.numeric(head[colnames(head)=="TAV"])
	agmip$clim$amp <- 	as.numeric(head[colnames(head)=="AMP"])
	agmip$clim$refht <- 	as.numeric(head[colnames(head)=="REFHT"])
	agmip$clim$wndht <- 	as.numeric(head[colnames(head)=="WNDHT"])

	agmip$data$date <- 	data[,colnames(data)=="@DATE"]
	agmip$data$yyyy <- 	data[,colnames(data)=="YYYY"]
	agmip$data$mm <- 	data[,colnames(data)=="MM"]
	agmip$data$dd <- 	data[,colnames(data)=="DD"]
	agmip$data$juld <- 	as.numeric(format(as.Date(paste(agmip$data$yyyy,agmip$data$mm,agmip$data$dd,sep="-")),"%j"))

	agmip$data$srad <- 	data[,colnames(data)=="SRAD"]
	agmip$data$tmax <- 	data[,colnames(data)=="TMAX"]
	agmip$data$tmin <- 	data[,colnames(data)=="TMIN"]
	agmip$data$rain <- 	data[,colnames(data)=="RAIN"]
	agmip$data$wind <- 	data[,colnames(data)=="WIND"]
	agmip$data$dewp <- 	data[,colnames(data)=="DEWP"]
	agmip$data$vprs <- 	data[,colnames(data)=="VPRS"]
	agmip$data$rhum <- 	data[,colnames(data)=="RHUM"]

	agmip$period$start <- 	as.Date(as.character(paste(agmip$data$yyyy[1],agmip$data$mm[1],agmip$data$dd[1],sep="-")))
	agmip$period$end <-	as.Date(as.character(paste(agmip$data$yyyy[length(agmip$data$yyyy)],agmip$data$mm[length(agmip$data$mm)],agmip$data$dd[length(agmip$data$dd)],sep="-")))
	agmip$period$type <-	0

	# missing values
	for (d in 1:length(agmip$data)){
		agmip$data[[d]][agmip$data[[d]][]==-999] <- NA 
		agmip$data[[d]][agmip$data[[d]][]==-99] <- NA 
		agmip$data[[d]][agmip$data[[d]][]==NaN] <- NA 
	}

return(agmip)
rm(data,head)
}

########################################################################
##### old CSAG
########################################################################
#inFo<- '/home/crespo/Desktop/12_AgMIP/2012-10-01_fastTrack/AMIP/E_RCP85/BNU-ESM-rcp85'
#fNa<- '0261516.1.txt'
#fNa<- '0261516B.txt'
## old CSAG
# inFolder is the direct parent folder of tmin,tmax,ppt
# fName is the station file name
# e.g. the tmax file is here: inFolder/tmax/fName
read_oldCSAGformat <- function(inFolder=inFo,fName=fNa)
{
	cVar <- list.files(inFolder)	# very likely tmin, tmax, ppt
	head <- array(NA,dim=c(length(cVar),8))
	colnames(head) <- c('ID','LAT','LON','ALT','sDate','eDate','TYPE','COMM')
	rownames(head) <- cVar

	csag <- createNULLlist()
	csag$file <- 	paste(inFolder,'{tmin,tmax,ppt,rad}',fName,sep="/")

	# read all headers
	inconsistences <- FALSE
	for (v in 1:length(cVar)){
		inFile<- paste(inFolder,cVar[v],fName,sep="/")
		line <- readLines(inFile,n=3)
		line<-	strsplit(line,split=" ")
		# rm blank cells
		for(l in 1:length(line)){
			line[[l]] <- line[[l]][line[[l]][]!=""]
		}
		# fill in head
		head[v,1] <- line[[1]][1]	# ID
		head[v,2] <- line[[1]][2]	# LAT
		head[v,3] <- line[[1]][3]	# LON
		head[v,4] <- line[[1]][4]	# ALT
		head[v,5] <- as.character(as.Date(line[[2]][1],"%Y%m%d"))	# sDate
		head[v,6] <- as.character(as.Date(line[[2]][2],"%Y%m%d"))	# eDate
		head[v,7] <- line[[2]][3]	# TYPE
		head[v,8] <- line[[3]][1]	# COMM
	
		# data
		if(cVar[v]=="ppt")				rain <- as.array(as.numeric(scan(inFile,what="numeric",sep="\n",skip=3,nlines=-1,quiet=TRUE)));
		if(any(cVar[v]=="tmax",cVar[v]=="tmx"))	tmax <- as.array(as.numeric(scan(inFile,what="numeric",sep="\n",skip=3,nlines=-1,quiet=TRUE)));
		if(any(cVar[v]=="tmin",cVar[v]=="tmn"))	tmin <- as.array(as.numeric(scan(inFile,what="numeric",sep="\n",skip=3,nlines=-1,quiet=TRUE)));

		if(v>1){
			if(!all(head[v,]==head[1,])) inconsistences <- TRUE
		}
	}
	if(inconsistences){
		print("### > WARNING")
		print("#     there is inconsistencies in the file headers")
		print("#     we will assume ppt (id,lat,lon,alt,type) header")
		print("#     and take the longest possible period (filling with NA)")
		print(head[,1:7])
#		browser()
	}

	# make the local format
	csag$station$id <- 	head[rownames(head)=="ppt",colnames(head)=='ID']
	csag$station$lat <- 	as.numeric(head[rownames(head)=="ppt",colnames(head)=='LAT'])
	csag$station$lon <- 	as.numeric(head[rownames(head)=="ppt",colnames(head)=='LON'])
	csag$station$alt <- 	as.numeric(head[rownames(head)=="ppt",colnames(head)=='ALT'])
	csag$station$comm <- 	head[rownames(head)=="ppt",colnames(head)=='COMM']

	csag$clim$tav <- 	NA
	csag$clim$amp <- 	NA
	csag$clim$refht <- 	2	# WMO standard, may be different in some cases...
	csag$clim$wndht <- 	NA

#	csag$period$start <- 	as.Date(max(head[,5]))
#	csag$period$end <-	as.Date(min(head[,6]))
	csag$period$start <- 	as.Date(min(head[,5]))
	csag$period$end <-	as.Date(max(head[,6]))
	csag$period$type <-	as.numeric(head[rownames(head)=="ppt",7])

	csag$data$date <- 	seq(csag$period$start,csag$period$end,1)
	csag$data$yyyy <- 	as.numeric(format(csag$data$date,"%Y"))
	csag$data$mm <- 	as.numeric(format(csag$data$date,"%m"))
	csag$data$dd <- 	as.numeric(format(csag$data$date,"%d"))
	csag$data$juld <- 	as.numeric(format(csag$data$date,"%j"))
	csag$data$date <- 	as.numeric(format(csag$data$date,"%Y%j"))

	csag$data$tmin <- 	array(NA,dim=length(csag$data$date))
	csag$data$tmax <- 	array(NA,dim=length(csag$data$date))
	csag$data$rain <- 	array(NA,dim=length(csag$data$date))

	csag$data$srad <- 	array(NA,dim=length(csag$data$date))
	csag$data$wind <- 	array(NA,dim=length(csag$data$date))
	csag$data$dewp <- 	array(NA,dim=length(csag$data$date))
	csag$data$vprs <- 	array(NA,dim=length(csag$data$date))
	csag$data$rhum <- 	array(NA,dim=length(csag$data$date))

	# longest possible period
	for (v in 1:length(cVar)){
		#start
		startD <- difftime(as.Date(head[rownames(head)==cVar[v],5]),csag$period$start,units="days")
#		if (startD>0){
			if(cVar[v]=="ppt")			csag$data$rain[(startD+1):(length(rain)+startD)] <- rain
			if(cVar[v]=="tmax" || cVar[v]=="tmx")	csag$data$tmax[(startD+1):(length(tmax)+startD)] <- tmax
			if(cVar[v]=="tmin" || cVar[v]=="tmn")	csag$data$tmin[(startD+1):(length(tmin)+startD)] <- tmin
#		}
		#end
#		endD <- difftime(csag$period$end,as.Date(head[rownames(head)==cVar[v],6]),units="days")
#		if (endD>0){
#			if(cVar[v]=="ppt")			csag$data$rain <- csag$data$rain[1:(length(csag$data$rain)-endD)]
#			if(any(cVar[v]=="tmax",cVar[v]=="tmx"))	csag$data$tmax <- csag$data$tmax[1:(length(csag$data$tmax)-endD)]
#			if(any(cVar[v]=="tmin",cVar[v]=="tmn"))	csag$data$tmin <- csag$data$tmin[1:(length(csag$data$tmin)-endD)]
#		}
	}

	# different types (real,365,360)
#	source('metTransformations.r')
	if (csag$period$type==1){
		# add up NA on 29-FEB
		feb29 <- csag$data$date[csag$data$mm==2 & csag$data$dd==29]
		for (l in 1:length(feb29)){
			w <- which(csag$data$date==feb29[l])
			csag$data$tmin <- c(csag$data$tmin[1:(w-1)],NA,csag$data$tmin[w:length(csag$data$tmin)]) 
			csag$data$tmax <- c(csag$data$tmax[1:(w-1)],NA,csag$data$tmax[w:length(csag$data$tmax)]) 
			csag$data$rain <- c(csag$data$rain[1:(w-1)],NA,csag$data$rain[w:length(csag$data$rain)]) 
		}
	}	
	if (csag$period$type==2){
		# on non-leap year add
		add_l <- NULL
		add_l <- c(add_l,csag$data$date[csag$data$mm==2 & csag$data$dd==29])
		add_l <- c(add_l,csag$data$date[csag$data$mm==4 & csag$data$dd==15])
		add_l <- c(add_l,csag$data$date[csag$data$mm==6 & csag$data$dd==15])
		add_l <- c(add_l,csag$data$date[csag$data$mm==8 & csag$data$dd==15])
		add_l <- c(add_l,csag$data$date[csag$data$mm==10 & csag$data$dd==15])
		add_l <- c(add_l,csag$data$date[csag$data$mm==12 & csag$data$dd==15])
		for (l in 1:length(add_l)){
			w <- which(csag$data$date==add_l[l])
			csag$data$tmin <- c(csag$data$tmin[1:(w-1)],NA,csag$data$tmin[w:length(csag$data$tmin)]) 
			csag$data$tmax <- c(csag$data$tmax[1:(w-1)],NA,csag$data$tmax[w:length(csag$data$tmax)]) 
			csag$data$rain <- c(csag$data$rain[1:(w-1)],NA,csag$data$rain[w:length(csag$data$rain)]) 
		}
	}	

	# possible different data length
	if (length(csag$data$tmin)!=length(csag$data$date)){
		print(paste("tmin data length",length(csag$data$tmin),sep=" : "))
		print(paste("date data length",length(csag$data$date),sep=" : "))
		browser()
	}
	if (length(csag$data$tmax)!=length(csag$data$date)){
		print(paste("tmax data length",length(csag$data$tmax),sep=" : "))
		print(paste("date data length",length(csag$data$date),sep=" : "))
		browser()
	}
	if (length(csag$data$rain)!=length(csag$data$date)){
		print(paste("rain data length",length(csag$data$rain),sep=" : "))
		print(paste("date data length",length(csag$data$date),sep=" : "))
		browser()
	}

	# missing values
	for (d in 1:length(csag$data)){
		csag$data[[d]][csag$data[[d]][]==-999] <- NA 
		csag$data[[d]][csag$data[[d]][]==-99] <- NA 
		csag$data[[d]][csag$data[[d]][]==NaN] <- NA 
	}

return(csag)
rm(cVar,head,line,d,o,jD,startD,endD)
}

########################################################################
##### APSIM
########################################################################
# inFi<-'~/Desktop/12_AgMIP/2012-10-01_fastTrack/CLIP/Bulawayo_51_2002.met'
# inFi<-'/home/crespo/Desktop/Link\ to\ WinShared/12_AgMIP/2012-10-01_fastTrack/AMIP/MerraData_CM/SABA0QXX.AgMIP'
## AgMIP
read_APSIMformat <- function(inFile=inFi)
{
	# read APSIM format data
	head <-	readLines(inFile,n=50)
	dTop <- grep("MJ/m2",head)
	tmp <- strsplit(head[(dTop-1)],split=" ")
	cNames <- array(tmp[[1]][tmp[[1]]!=""],dim=length(tmp[[1]][tmp[[1]]!=""]))
	data <-	scan(inFile,what='raw',skip=(dTop+1))
	data <- matrix(data,ncol=length(cNames),byrow=T)
	colnames(data) <- cNames

	# read header
	head <- head[1:dTop]
	head<-strsplit(head,split=" ")
	for(i in 1:length(tmp)){
		head[[i]]<-head[[i]][tmp[[i]]!=""]
		if (length(head[[i]])==0) head[[i]] <- NA
	}
	rLat <- head[[grep("atitude",head)]]	# row with lat
	rLon <- ifelse(length(grep("ongitude",head))==0,NA,head[[grep("ongitude",head)]])	# row with lon (if exists, not mandatory in APSIM format)
	rAlt <- ifelse(length(grep("ltitude",head))==0,NA,head[[grep("ltitude",head)]])		# row with alt (if exists, not mandatory in APSIM format)
	
	apsim <- createNULLlist()
	apsim$file <- inFile
	apsim$station$id <- 	data[1,colnames(data)=="site"]
	apsim$station$lat <- 	as.numeric(rLat[grep("\\.",rLat)])
	apsim$station$lon <- 	ifelse(is.na(rLon),NA,as.numeric(rLon[grep("\\.",rLon)]))
	apsim$station$alt <- 	ifelse(is.na(rAlt),NA,as.numeric(rAlt[grep("\\.",rAlt)]))
	apsim$station$comm <- 	NA

	tTav <- grep("tav",head)
	tAmp <- grep("amp",head)
	rTav <- tTav[which((tTav-tAmp)!=0)]
	rAmp <- tAmp[which((tTav-tAmp)!=0)]
	cTav <- array(head[[rTav]][head[[rTav]]!=""],dim=length(head[[rTav]][head[[rTav]]!=""]))
	cAmp <- array(head[[rAmp]][head[[rAmp]]!=""],dim=length(head[[rAmp]][head[[rAmp]]!=""]))
	apsim$clim$tav <- 	as.numeric(cTav[grep("\\.",cTav)])
	apsim$clim$amp <- 	as.numeric(cAmp[grep("\\.",cAmp)])
	apsim$clim$refht <- 	NA
	apsim$clim$wndht <- 	NA

	apsim$data$yyyy <- 	as.numeric(data[,colnames(data)=="year"])
	apsim$data$juld <- 	as.numeric(data[,colnames(data)=="day"])
	apsim$period$start <- 	as.Date(paste(apsim$data$yyyy[1],apsim$data$juld[1],sep="-"),"%Y-%j")
	apsim$period$end <-	as.Date(paste(apsim$data$yyyy[length(apsim$data$yyyy)],apsim$data$juld[length(apsim$data$juld)],sep="-"),"%Y-%j")
	apsim$period$type <-	0

	apsim$data$date <- 	seq(apsim$period$start,apsim$period$end,1)
	apsim$data$mm <- 	as.numeric(format(apsim$data$date,"%m"))
	apsim$data$dd <- 	as.numeric(format(apsim$data$date,"%d"))
	apsim$data$date <- 	as.numeric(format(apsim$data$date,"%Y%j"))
	if(length(apsim$data$yyyy)!=length(apsim$data$date)){
		print(" ### > WARNING in read_APSIMformat",quote=F)
		print("     > data length issue",quote=F)
		stop("cannot resolve")
	}

	apsim$data$srad <- 	as.numeric(data[,colnames(data)=="radn"])
	apsim$data$tmax <- 	as.numeric(data[,colnames(data)=="maxt"])
	apsim$data$tmin <- 	as.numeric(data[,colnames(data)=="mint"])
	apsim$data$rain <- 	as.numeric(data[,colnames(data)=="rain"])
	apsim$data$wind <- 	array(NA,dim=length(apsim$data$date))
	apsim$data$dewp <- 	array(NA,dim=length(apsim$data$date))
	apsim$data$vprs <- 	array(NA,dim=length(apsim$data$date))
	apsim$data$rhum <- 	array(NA,dim=length(apsim$data$date))

	# missing values
	for (d in 1:length(apsim$data)){
		apsim$data[[d]][apsim$data[[d]][]==-999] <- NA 
		apsim$data[[d]][apsim$data[[d]][]==-99] <- NA 
		apsim$data[[d]][apsim$data[[d]][]==NaN] <- NA
	}

return(apsim)
rm(data,head,dTop,tmp,cNames,tLat,rLon,rAlt,rTav,rAmp,tTav,tAmp,cTav,cAmp)
}

########################################################################
##### NCDF/ASCII
########################################################################
# actually not ncdf,
# I used CDO to subselect and reformat to ascii
# so I'm reading ONE column of this ascii file 
########################################################################
#inFo <- '/home/crespo/Desktop/12_AgMIP/2012-10-01_fastTrack/CLIP/D_RCP45_raw/ASCII'
#gc <- 'bnu-esm'
# inFolder is the direct parent folder of tmin,tmax,ppt
#la <- -18.75
#lo <- 28.75
#al <- 1200
#co <- 5
#id <- 'BNU-45'
#com <-'GCM watch grid -18.5,-19 28.5,29'
#sDa <- as.Date('1960-01-01')
#eDa <- as.Date('2100-12-31')
read_ASCIIformat <- function(inFolder=inFo,gcm=gc,col=co,lat=la,lon=lo,alt=al,sDate=sDa,eDate=eDa,iden=id,comm=com)
{
print(gcm)
	cVar <- list.files(inFolder)	# very likely tmin, tmax, ppt
	ascii <- createNULLlist()
	ascii$file <- 	paste(inFolder,'{tmin,tmax,ppt}',gcm,sep="/")
	# read all headers
	for (v in 1:length(cVar)){
		fiName <- list.files(paste(inFolder,cVar[v],sep="/"))		
		inFile<- paste(inFolder,cVar[v],fiName[grep(fiName,pattern=gcm)],sep="/")
print(inFile)
		if(cVar[v]=="ppt")	ascii$data$rain <- read.table(inFile)[,col]
		if(cVar[v]=="tmax")	ascii$data$tmax <- read.table(inFile)[,col]-273.15	# as far as I know it is in absolute degress celsius
		if(cVar[v]=="tmin")	ascii$data$tmin <- read.table(inFile)[,col]-273.15
	}

	# make the local format
	ascii$station$id <- 	iden
	ascii$station$lat <- 	lat
	ascii$station$lon <- 	lon
	ascii$station$alt <- 	alt
	ascii$station$comm <- 	comm

	ascii$clim$tav <- 	NA
	ascii$clim$amp <- 	NA
	ascii$clim$refht <- 	NA	# 2: WMO standard, may be different in some cases...
	ascii$clim$wndht <- 	NA

	ascii$period$start <- 	sDate
	ascii$period$end <-	eDate

	ascii$data$date <- 	seq(ascii$period$start,ascii$period$end,1)
	ascii$data$yyyy <- 	as.numeric(format(ascii$data$date,"%Y"))
	ascii$data$mm <- 	as.numeric(format(ascii$data$date,"%m"))
	ascii$data$dd <- 	as.numeric(format(ascii$data$date,"%d"))
	ascii$data$juld <- 	as.numeric(format(ascii$data$date,"%j"))
	ascii$data$date <- 	as.numeric(format(ascii$data$date,"%Y%j"))
	if(length(ascii$data$date)==length(ascii$data$rain)){	ascii$period$type <- 0
	}else{
		if(floor((ascii$data$yyyy[length(ascii$data$yyyy)]-ascii$data$yyyy[1]+1)/4)==(length(ascii$data$date)-length(ascii$data$rain))){
			ascii$period$type <- 1
		}else{
			print('### WARNING')
			print('### there is a problem here : dataRead:read_ASCIIformat:lengths')
			print(paste('length difference',length(ascii$data$date)-length(ascii$data$rain),sep=" > "))
			print(paste('(assumed) number of leap year within',floor((ascii$data$yyyy[length(ascii$data$yyyy)]-ascii$data$yyyy[1]+1)/4),sep=" > "))		
			print(paste('unexplained',(length(ascii$data$date)-length(ascii$data$rain))-(floor((ascii$data$yyyy[length(ascii$data$yyyy)]-ascii$data$yyyy[1]+1)/4)),sep=" > "))
#			browser()
# for now
		}
	}

	ascii$data$srad <- 	array(NA,dim=length(ascii$data$date))
	ascii$data$wind <- 	array(NA,dim=length(ascii$data$date))
	ascii$data$dewp <- 	array(NA,dim=length(ascii$data$date))
	ascii$data$vprs <- 	array(NA,dim=length(ascii$data$date))
	ascii$data$rhum <- 	array(NA,dim=length(ascii$data$date))

	# longest common period
	# does not deal with that, maybe should	

	# different types (real,365,360)
#	source('metTransformations.r')
	if (ascii$period$type==1){
		# add up NA on 29-FEB
		feb29 <- ascii$data$date[ascii$data$mm==2 & ascii$data$dd==29]
		for (l in 1:length(feb29)){
			w <- which(ascii$data$date==feb29[l])
			ascii$data$tmin <- c(ascii$data$tmin[1:(w-1)],NA,ascii$data$tmin[w:length(ascii$data$tmin)]) 
			ascii$data$tmax <- c(ascii$data$tmax[1:(w-1)],NA,ascii$data$tmax[w:length(ascii$data$tmax)]) 
			ascii$data$rain <- c(ascii$data$rain[1:(w-1)],NA,ascii$data$rain[w:length(ascii$data$rain)]) 
		}
	}	
	if (ascii$period$type==2){
		print("#### > WARNING : do not deal with type==2 yet")
		browser();
	}	

	# possible different data length
	if (length(ascii$data$tmin)!=length(ascii$data$date)){
		print(paste("tmin data length",length(ascii$data$tmin),sep=" : "))
		print(paste("date data length",length(ascii$data$date),sep=" : "))
		browser()
	}
	if (length(ascii$data$tmax)!=length(ascii$data$date)){
		print(paste("tmax data length",length(ascii$data$tmax),sep=" : "))
		print(paste("date data length",length(ascii$data$date),sep=" : "))
		browser()
	}
	if (length(ascii$data$rain)!=length(ascii$data$date)){
		print(paste("rain data length",length(ascii$data$rain),sep=" : "))
		print(paste("date data length",length(ascii$data$date),sep=" : "))
		browser()
	}

	# missing values
	for (d in 1:length(ascii$data)){
		ascii$data[[d]][ascii$data[[d]][]<(-99)] <- NA 
		ascii$data[[d]][ascii$data[[d]][]==NaN] <- NA 
	}

return(ascii)
rm(cVar,ascii,fiName,inFile,v,feb29,l,d)
}


### for Mduduzi
source('/home/crespo/Desktop/Optimisation/xpos-r/ClimateDataTools/Climatology/climTools.r')
read_mduCLICOM <- function(fileN=fileName)
{
	# read lines
	Lin <- readLines(fileN)

	# create your list
	mduCLICOM <- createNULLlist()
	
	# store file path
	mduCLICOM$file<-fileN
	
	# store start date (1st line)
	fLin <- strsplit(Lin[1],split=",")
	mduCLICOM$period$start<- as.Date(paste(fLin[[1]][5],"01",sep="-"))	
	
	# store end date (last line)
	ifelse(length(Lin[length(Lin)])==1,lLin <- strsplit(Lin[length(Lin)-1],split=","),lLin <- strsplit(Lin[length(Lin)],split=","))
	dat <- as.Date(paste(lLin[[1]][5],"01",sep="-"))
	mduCLICOM$period$end<- as.Date(paste(lLin[[1]][5],as.character(maxNo_days(as.numeric(format(dat,"%Y")),as.numeric(format(dat,"%m")))),sep="-"))	
	
	# set type (observed==0)
	mduCLICOM$period$type<-0

	# create daily date info
	mduCLICOM$data$date <- 	seq(mduCLICOM$period$start,mduCLICOM$period$end,1)
	mduCLICOM$data$yyyy <- 	as.numeric(format(mduCLICOM$data$date,"%Y"))
	mduCLICOM$data$mm <- 	as.numeric(format(mduCLICOM$data$date,"%m"))
	mduCLICOM$data$dd <- 	as.numeric(format(mduCLICOM$data$date,"%d"))
	mduCLICOM$data$juld <- 	as.numeric(format(mduCLICOM$data$date,"%j"))
	mduCLICOM$data$date <- 	as.numeric(format(mduCLICOM$data$date,"%Y%j"))

	# initialisetmin, tmax, rain
	mduCLICOM$data$tmin <- 	array(NA,dim=length(mduCLICOM$data$date))
	mduCLICOM$data$tmax <- 	array(NA,dim=length(mduCLICOM$data$date))
	mduCLICOM$data$rain <- 	array(NA,dim=length(mduCLICOM$data$date))

	# loop for every line
	for (l in 1:ifelse(length(Lin[length(Lin)])==1,length(Lin)-1,length(Lin))){
		# avoid empty lines
		if(Lin[l]=="") next
		# split 1 line
		sLin <- strsplit(Lin[l],split=",")
		
		# remove empty item	
		rLin <- array(sLin[[1]][sLin[[1]]!=""],dim=length(sLin[[1]][sLin[[1]]!=""]))
		rLin <- array(rLin[rLin!=" "],dim=length(rLin[rLin!=" "]))
		rLin <- array(rLin[rLin!="M"],dim=length(rLin[rLin!="M"]))

		# get date of the line (item 4)
		dat <- as.Date(paste(rLin[4],"01",sep="-"))	

		# store station id (item 2)
		mduCLICOM$station$id<-rLin[2]

		# depending on variable {tmin,tmax,rain} data (the rest)
		switch(as.numeric(rLin[3]),{
			#1
			},{
			#2:tmax
				dayInMonth <- maxNo_days(as.numeric(format(dat,"%Y")),as.numeric(format(dat,"%m")))
				# where to put the data
				r <- which(mduCLICOM$data$yyyy==as.numeric(format(dat,"%Y")) & mduCLICOM$data$mm==as.numeric(format(dat,"%m")))[1]
				# store the data at the right position
				mduCLICOM$data$tmax[r:(r+dayInMonth-1)] <- as.numeric(rLin[5:(5+dayInMonth-1)])
			},{
			#3:tmin
				dayInMonth <- maxNo_days(as.numeric(format(dat,"%Y")),as.numeric(format(dat,"%m")))
				r <- which(mduCLICOM$data$yyyy==as.numeric(format(dat,"%Y")) & mduCLICOM$data$mm==as.numeric(format(dat,"%m")))[1]
				mduCLICOM$data$tmin[r:(r+dayInMonth-1)] <- as.numeric(rLin[5:(5+dayInMonth-1)])
			},{
			#4
			},{
			#5:rain
				dayInMonth <- maxNo_days(as.numeric(format(dat,"%Y")),as.numeric(format(dat,"%m")))
				r <- which(mduCLICOM$data$yyyy==as.numeric(format(dat,"%Y")) & mduCLICOM$data$mm==as.numeric(format(dat,"%m")))[1]
				mduCLICOM$data$rain[r:(r+dayInMonth-1)] <- as.numeric(rLin[5:(5+dayInMonth-1)])
			}
		)	
	}

	# missing values
	for (d in 1:length(mduCLICOM$data)){
		mduCLICOM$data[[d]][mduCLICOM$data[[d]][]==-99999] <- NA 
		mduCLICOM$data[[d]][mduCLICOM$data[[d]][]==-999] <- NA 
		mduCLICOM$data[[d]][mduCLICOM$data[[d]][]==-99] <- NA 
		mduCLICOM$data[[d]][mduCLICOM$data[[d]][]==NaN] <- NA
	}
	

return(mduCLICOM)
rm()
}

# var: 1:tmin, 2:tmax, 3:rain
read_mdu2 <- function(fileName,var)
{
	# uncomplete
	tab1 <- read.table(fileName,sep=";",colClasses="character")

	# create mduCLISYS

	# take the station code into mduCLISYS$station$id

	# get start date (first row of table) into mduCLISYS$period$start

	# get end date (last row of table) into mduCLISYS$period$ednd

	# create date,yyyy,mm,dd,juld from start to end
	
	# initialisetmin, tmax, rain
	
	# for every row
		
		# extract second column value

		# transform it into year month day

		# save the third column value in the right variable (mduCLISYS$data${tmin,tmax,rain})
		# according to the date you read above (SAME LINE) fitting into the date mduCLISYS$data${date,yyyy,mm,dd,jul}
		# !!! how do you define which variable it is ? tmin, tmax, rain? maybe an additional function input

	# close for loop

}

