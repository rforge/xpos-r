
## all format get out as
#list(	"file"=path to the file
#	"station"=list("id","lat","lon","alt","comm")
#	"clim"=list("tav","amp","refht","wndht")
#	"period"=list("start","end","type")	# type is CSAG style {0-real,1-365,2-360 days per year}
#	"data"=list("date","yyyy","mm","dd","juld","srad","tmax","tmin","rain","wind","dewp","vprs","rhum")
# MISSING VALUES = NA

## create empty forma
createNULLlist <- function()
{
	file <- 	NULL;
	station <- 	list("id"=NULL,"lat"=NULL,"lon"=NULL,"alt"=NULL,"comm"=NULL)
	clim <- 	list("tav"=NULL,"amp"=NULL,"refht"=NULL,"wndht"=NULL)
	period <- 	list("start"=NULL,"end"=NULL,"type"=NULL)
	data <- 	list("date"=NULL,"yyyy"=NULL,"mm"=NULL,"dd"=NULL,"juld"=NULL,"srad"=NULL,"tmax"=NULL,"tmin"=NULL,"rain"=NULL,"wind"=NULL,"dewp"=NULL,"vprs"=NULL,"rhum"=NULL)

	l <- 	list("file"=file,"station"=station,"clim"=clim,"period"=period,"data"=data)
return(l)
}

inFi<-'/home/crespo/Desktop/12_AgMIP/2012-10-01_fastTrack/AMIP/MerraData_CM/SABA0QXX.AgMIP'
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

print("### > your output is formated as follow",quote=F)
print("    > $file    <- path to file you read",quote=F)
print("    > $station <- list(\'id\',\'lat\',\'lon\',\'alt\',\'comm\')",quote=F)
print("    > $clim    <- list(\'tav\',\'amp\',\'refht\',\'wndht\')",quote=F)
print("    > $period  <- list(\'start\',\'end\',\'type\')",quote=F)
print("    > $data    <- list(\'date\',\'yyyy\',\'mm\',\'dd\',\'juld\',",quote=F)
print("                       \'srad\',\'tmax\',\'tmin\',\'rain\',\'wind\',\'dewp\',\'vprs\',\'rhum\')",quote=F)

return(agmip)
rm(data,head)
}

inFo<- '/home/crespo/Desktop/12_AgMIP/2012-07_Sentinel/CSAGformat/RZA/obs'
#inFo <- '/home/crespo/Desktop/Link\ to\ WinShared/12_AgMIP/2012-07_Sentinel/CSAGformat/RZA/obs'
fNa<- '0261516.1.txt'
## old CSAG
# inFolder is the direct parent folder of tmn,tmx,ppt
# fName is the station file name
# e.g. the tmx file is here: inFolder/tmx/fName
read_oldCSAGformat <- function(inFolder=inFo,fName=fNa)
{
	cVar <- list.files(inFolder)	# very likely tmn, tmax, ppt
	head <- array(NA,dim=c(length(cVar),8))
	colnames(head) <- c('ID','LAT','LON','ALT','sDate','eDate','TYPE','COMM')
	rownames(head) <- cVar

	csag <- createNULLlist()
	csag$file <- 	paste(inFolder,'{tmn,tmx,ppt,rad}',fName,sep="/")

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
		if(cVar[v]=="ppt")	csag$data$rain <- as.array(as.numeric(scan(inFile,what="numeric",sep="\n",skip=3,nlines=-1,quiet=TRUE)));
		if(cVar[v]=="tmx" || cVar[v]=="tmax")	csag$data$tmax <- as.array(as.numeric(scan(inFile,what="numeric",sep="\n",skip=3,nlines=-1,quiet=TRUE)));
		if(cVar[v]=="tmn" || cVar[v]=="tmin")	csag$data$tmin <- as.array(as.numeric(scan(inFile,what="numeric",sep="\n",skip=3,nlines=-1,quiet=TRUE)));

		if(v>1){
			if(!all(head[v,]==head[1,])) inconsistences <- TRUE
		}
	}
	if(inconsistences){
		print("## WARNING ##")
		print("##         there is inconsistencies in the file headers")
		print("##         if you resume, we will assume tmn (id,lat,lon,alt,type) header from now on and take the longest period and filling with NA")
		print(head)
		browser()
	}

	# make the local format
	csag$station$id <- 	head[rownames(head)=="tmn",colnames(head)=='ID']
	csag$station$lat <- 	as.numeric(head[rownames(head)=="tmn",colnames(head)=='LAT'])
	csag$station$lon <- 	as.numeric(head[rownames(head)=="tmn",colnames(head)=='LON'])
	csag$station$alt <- 	as.numeric(head[rownames(head)=="tmn",colnames(head)=='ALT'])
	csag$station$comm <- 	head[rownames(head)=="tmn",colnames(head)=='COMM']

	csag$clim$tav <- 	NA
	csag$clim$amp <- 	NA
	csag$clim$refht <- 	NA
	csag$clim$wndht <- 	NA

	csag$period$start <- 	as.Date(min(head[,5]))
	csag$period$end <-	as.Date(max(head[,6]))
	csag$period$type <-	as.numeric(head[rownames(head)=="tmn",7])

	csag$data$date <- 	seq(csag$period$start,csag$period$end,1)
	csag$data$yyyy <- 	as.numeric(format(csag$data$date,"%Y"))
	csag$data$mm <- 	as.numeric(format(csag$data$date,"%m"))
	csag$data$dd <- 	as.numeric(format(csag$data$date,"%d"))
	csag$data$juld <- 	as.numeric(format(csag$data$date,"%j"))
	csag$data$date <- 	as.numeric(format(csag$data$date,"%Y%j"))

	csag$data$srad <- 	array(NA,dim=length(csag$data$date))
	csag$data$wind <- 	array(NA,dim=length(csag$data$date))
	csag$data$dewp <- 	array(NA,dim=length(csag$data$date))
	csag$data$vprs <- 	array(NA,dim=length(csag$data$date))
	csag$data$rhum <- 	array(NA,dim=length(csag$data$date))

	# fill in with NA to make same length data
	for (v in 1:length(cVar)){
		#start
		startD <- difftime(as.Date(head[rownames(head)==cVar[v],5]),csag$period$start,units="days")
		if (startD>0){
			if(cVar[v]=="ppt")	csag$data$rain <- c(array(NA,dim=startD),csag$data$rain)
			if(cVar[v]=="tmx" || cVar[v]=="tmax")	csag$data$tmax <- c(array(NA,dim=startD),csag$data$tmax)
			if(cVar[v]=="tmn" || cVar[v]=="tmin")	csag$data$tmin <- c(array(NA,dim=startD),csag$data$tmin)
		}
		#end
		endD <- difftime(csag$period$end,as.Date(head[rownames(head)==cVar[v],6]),units="days")
		if (endD>0){
			if(cVar[v]=="ppt")	csag$data$rain <- c(csag$data$rain,array(NA,dim=endD))
			if(cVar[v]=="tmx" || cVar[v]=="tmax")	csag$data$tmax <- c(csag$data$tmax,array(NA,dim=endD))
			if(cVar[v]=="tmn" || cVar[v]=="tmin")	csag$data$tmin <- c(csag$data$tmin,array(NA,dim=endD))
		}
	}

	# different types (real,365,360)
#	source('metTransformations.r')
	if (csag$period$type!=0){
		print("#>	do not deal with type!=0 yet")
		browser();
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

print("### > your output is formated as follow",quote=F)
print("    > $file    <- path to file you read",quote=F)
print("    > $station <- list(\'id\',\'lat\',\'lon\',\'alt\',\'comm\')",quote=F)
print("    > $clim    <- list(\'tav\',\'amp\',\'refht\',\'wndht\')",quote=F)
print("    > $period  <- list(\'start\',\'end\',\'type\')",quote=F)
print("    > $data    <- list(\'date\',\'yyyy\',\'mm\',\'dd\',\'juld\',",quote=F)
print("                       \'srad\',\'tmax\',\'tmin\',\'rain\',\'wind\',\'dewp\',\'vprs\',\'rhum\')",quote=F)

return(csag)
rm(cVar,head,line,d,o,jD,startD,endD)
}

