##
 # FILE dataWrite.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
 ###############################################################################

## all format get in as
#list(	"file"=path to the file
#	"station"=list("id","lat","lon","alt","comm")
#	"clim"=list("tav","amp","refht","wndht")
#	"period"=list("start","end","type")	# type is metD style {0-real,1-365,2-360 days per year}
#	"data"=list("date","yyyy","mm","dd","juld","srad","tmax","tmin","rain","wind","dewp","vprs","rhum")
# MISSING VALUES = NA

########################################################################
##### AGMIP
########################################################################
#outFiAgmip<-'/home/crespo/Desktop/12_AgMIP/2012-10-01_fastTrack/AMIP/WorkingDir/SABA0Qtest.AgMIP'
write_AgMIPformat <- function(metD,outFile=outFiAgmip,checkMiss=TRUE)
{
	
	# template order
	# @DATE    YYYY  MM  DD  SRAD  TMAX  TMIN  RAIN  WIND  DEWP  VPRS  RHUM
	table <- array(NA,dim=c(length(metD$data[[1]]),12))
	for (v in 1:dim(table)[2]){
		switch(v,
			table[,v] <- metD$data[[which(names(metD$data)=="date")]][],
			table[,v] <- metD$data[[which(names(metD$data)=="yyyy")]][],
			table[,v] <- metD$data[[which(names(metD$data)=="mm")]][],
			table[,v] <- metD$data[[which(names(metD$data)=="dd")]][],
			table[,v] <- metD$data[[which(names(metD$data)=="srad")]][],
			table[,v] <- metD$data[[which(names(metD$data)=="tmax")]][],
			table[,v] <- metD$data[[which(names(metD$data)=="tmin")]][],
			table[,v] <- metD$data[[which(names(metD$data)=="rain")]][],
			table[,v] <- metD$data[[which(names(metD$data)=="wind")]][],
			table[,v] <- metD$data[[which(names(metD$data)=="dewp")]][],
			table[,v] <- metD$data[[which(names(metD$data)=="vprs")]][],
			table[,v] <- metD$data[[which(names(metD$data)=="rhum")]][]
		)
	}

	# missing values
	# there should be no missing values at this stage
	if(checkMiss){
	for (d in 1:length(metD$data)){
		if(any(is.na(metD$data[[d]][]))){
			print("### > WARNING missing values",quote=F)
			print("    > Do you expect any at this stage?",quote=F)
			browser()
		}
	}}	

	# file name
	if(!file.copy("./agmipTemplate.AgMIP",outFile,overwrite=TRUE)){
		print("### > FAILED TO CREATE OUTPUT FILE",quote=F)
		stop("cannot resolve")
	}
	ifelse(Sys.info()["sysname"]=="Linux",newline <- "\r\n",newline<-"\n")

	# change header
	header <- readLines(outFile)
	header <- sub("v_staLocation",	paste(metD$station$comm,"(metD-",metD$station$id,")",sep=""),	header)
	header <- sub("v_lat",		round(metD$station$lat,2),					header)
	header <- sub("v_lon",		round(metD$station$lon,2),					header)
	header <- sub("v_alt",		round(metD$station$alt,0),					header)
	header <- sub("v_tav",		round(metD$clim$tav,1),						header)
	header <- sub("v_amp",		round(metD$clim$amp,1),						header)
	header <- sub("v_ref",		round(metD$clim$refht,2),					header)
	header <- sub("v_wnd",		round(metD$clim$wndht,2),					header)
	write.table(header,outFile,append=FALSE,quote=FALSE,col.names=FALSE,row.names=FALSE,eol=newline)

	# add up body
	table[,1] <- format.AsIs(table[,1],justify="right")		# DATE
	table[,2] <- format.AsIs(table[,2],justify="right")		# YYYY
	table[,3:4] <- format.AsIs(table[,3:4],justify="right")		# MM and DD
	table[,5:12] <- format.AsIs(table[,5:12],justify="right")	# all the rest
	write.table(table,outFile,append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE,eol=newline,sep=" ")

rm(table,v,header)
}

########################################################################
##### oldCSAG
########################################################################
#outFometD<-'/home/crespo/Desktop/12_AgMIP/2012-10-01_fastTrack/AMIP/WorkingDir'
write_oldCSAGformat <- function(metD,outFolder=outFometD)
{
	# if you built a full set, then switch type to 0
	if(metD$period$type==1)
	{	# so far in that case I have added NAs on February 29th, let's remove those
		feb29 <- metD$data$date[metD$data$mm==2 & metD$data$dd==29]
		for (l in length(feb29):1){
			w <- which(metD$data$date==feb29[l])
			metD$data$tmin <- c(metD$data$tmin[1:(w-1)],metD$data$tmin[(w+1):length(metD$data$tmin)]) 
			metD$data$tmax <- c(metD$data$tmax[1:(w-1)],metD$data$tmax[(w+1):length(metD$data$tmax)]) 
			metD$data$rain <- c(metD$data$rain[1:(w-1)],metD$data$rain[(w+1):length(metD$data$rain)]) 
		}
	}
	if(metD$period$type==2)
	{
		print("### > WARNING type == 2")
		print("    > nothing done for that yet")
		browser()
	}
	
	# missing values
	for (d in 6:8){		# only for tmin, tmax and ppt
		if(any(is.na(metD$data[[d]][]))){
			print("### > WARNING missing values",quote=F)
		}
		metD$data[[d]][is.na(metD$data[[d]][])] <- -999
	}	

	# folders
	if(!file.exists(paste(outFolder,"tmin",sep="/")))	dir.create(paste(outFolder,"tmin",sep="/"),showWarnings=TRUE,recursive=FALSE,mode="0777")
	if(!file.exists(paste(outFolder,"tmax",sep="/")))	dir.create(paste(outFolder,"tmax",sep="/"),showWarnings=TRUE,recursive=FALSE,mode="0777")
	if(!file.exists(paste(outFolder,"ppt",sep="/")))	dir.create(paste(outFolder,"ppt",sep="/"),showWarnings=TRUE,recursive=FALSE,mode="0777")
	
	# files
	outTmin <- paste(outFolder,"tmin",paste(metD$station$id,"txt",sep="."),sep="/")
	outTmax <- paste(outFolder,"tmax",paste(metD$station$id,"txt",sep="."),sep="/")
	outPpt <- paste(outFolder,"ppt",paste(metD$station$id,"txt",sep="."),sep="/")
	if(!file.copy("/home/crespo/Desktop/Optimisation/xpos-r/ClimateDataTools/ClimFormats/csagTemplates/csagTemplate.txt",outTmin,overwrite=TRUE)){
		print("### > FAILED TO CREATE OUTPUT FILE",quote=F)
		stop("cannot resolve")
	}
	if(Sys.info()["sysname"]=="Linux") newline <- "\r\n"

	# change header
	header <- readLines(outTmin)
	header <- sub("v_id",		metD$station$id,			header)
	header <- sub("v_lat",		round(metD$station$lat,2),		header)
	header <- sub("v_lon",		round(metD$station$lon,2),		header)
	header <- sub("v_alt",		round(metD$station$alt,0),		header)
	header <- sub("v_sDate",	format(metD$period$start,"%Y%m%d12"),	header)
	header <- sub("v_eDate",	format(metD$period$end,"%Y%m%d12"),	header)
	header <- sub("v_type",		metD$period$type,			header)
	header <- sub("v_description",	metD$station$comm,			header)
	write.table(header,outTmin,append=FALSE,quote=FALSE,col.names=FALSE,row.names=FALSE,eol=newline)

	# duplicate header
	if(!file.copy(outTmin,outTmax,overwrite=TRUE)){
		print("### > FAILED TO COPY TMIN HEADER INTO TMAX",quote=F)
		stop("cannot resolve")
	}
	if(!file.copy(outTmin,outPpt,overwrite=TRUE)){
		print("### > FAILED TO COPY TMIN HEADER INTO PPT",quote=F)
		stop("cannot resolve")
	}
	
	# add up body
	write.table(metD$data$tmin,outTmin,append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE,eol=newline,sep=" ")
	write.table(metD$data$tmax,outTmax,append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE,eol=newline,sep=" ")
	write.table(metD$data$rain,outPpt,append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE,eol=newline,sep=" ")

rm(d,outTmin,outTmax,outPpt,header)
}


