library('foreign')

source("~/Desktop/Optimisation/xpos-r/apsimPlugIn/rwfileOp.r")
csagT <- "~/Desktop/Optimisation/xpos-r/convertBruceFormat/csagTemplates/csagTemplate"

outFolder <- "~/Desktop/12_AgMIP/2012-10-01_fastTrack/AMIP/WiltData_CSAG/";
inFile <- "~/Desktop/12_AgMIP/2012-10-01_fastTrack/AMIP/Weather_Bloem_Thaba.dbf"

readWformat <- function(inF=inFile)
{
	a <- read.dbf(inF);
#	1:OBJECTID, 2:Compno, 3:Lat, 4:Lon, 5:StationNam, 6:nYear, 7:nMonth, 8:nDay, 9:Rain, 10:Tx, 11:Tn, 12:tRadN
#	dataRaw <- matrix(a,ncol=12,byrow=TRUE);
return(a)
}

createFiles <- function(dataRaw)
{
# into CSAG format
# 	header StationNam, Lat, Lon
#	1 file for Tx, 1 for Tn, 1 for Rain, 1 for tRadN
	
	# init with line 1
	i<-1
	station <- list("id"=dataRaw[i,2],"lat"=as.numeric(dataRaw[i,3]),"lon"=as.numeric(dataRaw[i,4]),"alt"=NA);
	period <- list("start"=as.Date(paste(dataRaw[i,6],dataRaw[i,7],dataRaw[i,8],sep="-")),"end"=NA,"type"=0,"startH"=12,"endH"=12);
	comm <- dataRaw[i,5];
	head <- list("station"=station,"period"=period,"comment"=comm)

	rai <- as.numeric(dataRaw[i,9]);
	tmn <- as.numeric(dataRaw[i,11]);
	tmx <- as.numeric(dataRaw[i,10]);
	rad <- as.numeric(dataRaw[i,12]);
	
# read it all
	while (i < dim(dataRaw)[1]){
	i<-i+1;
		if (dataRaw[i,2] != station$id){
print(paste(i-1,dim(dataRaw)[1],sep=" / "));
			head$period$end <- as.Date(paste(dataRaw[(i-1),6],dataRaw[(i-1),7],dataRaw[(i-1),8],sep="-"))
			data <- list("tmn"=tmn,"tmx"=tmx,"rai"=rai,"rad"=rad)
			for (v in 1:4){
				formatToCSAG(data,head,outFolder,v);
			}

			station <- list("id"=dataRaw[i,2],"lat"=as.numeric(dataRaw[i,3]),"lon"=as.numeric(dataRaw[i,4]),"alt"=NA);
			period <- list("start"=as.Date(paste(dataRaw[i,6],dataRaw[i,7],dataRaw[i,8],sep="-")),"end"=NA,"type"=0,"startH"=12,"endH"=12);
			comm <- dataRaw[i,5];
			head <- list("station"=station,"period"=period,"comment"=comm)

			rai <- as.numeric(dataRaw[i,9]);
			tmn <- as.numeric(dataRaw[i,11]);
			tmx <- as.numeric(dataRaw[i,10]);
			rad <- as.numeric(dataRaw[i,12]);
		}else{
			rai <- c(rai,as.numeric(dataRaw[i,9]));
			tmn <- c(tmn,as.numeric(dataRaw[i,11]));
			tmx <- c(tmx,as.numeric(dataRaw[i,10]));
			rad <- c(rad,as.numeric(dataRaw[i,12]));
		}
	}
print(paste(i,dim(dataRaw)[1],sep=" / "));
	head$period$end <- as.Date(paste(dataRaw[i,6],dataRaw[i,7],dataRaw[i,8],sep="-"))
	rai <- c(rai,as.numeric(dataRaw[i,9]));
	tmn <- c(tmn,as.numeric(dataRaw[i,11]));
	tmx <- c(tmx,as.numeric(dataRaw[i,10]));
	rad <- c(rad,as.numeric(dataRaw[i,12]));
	data <- list("tmn"=tmn,"tmx"=tmx,"rai"=rai,"rad"=rad)
	for (v in 1:4){
		formatToCSAG(data,head,outFolder,v);
	}
}

formatToCSAG <- function(data,head,outF,v)
{

# make one table from all the data
	switch(v, # 1:4
		{	#1 - tmn
			outF <- paste(outF,"tmn/",sep="");
			table <- data$tmn;
		},{	#2 - tmx
			outF <- paste(outF,"tmx/",sep="");
			table <- data$tmx;
		},{	#3 - ppt
			outF <- paste(outF,"ppt/",sep="");
			table <- data$rai;
		},{	#4 - rad
			outF <- paste(outF,"rad/",sep="");
			table <- data$rad;
		}
	);
#	table <- format(table,digits=1,nsmall=2);

# write it into a .txt file
	outName <- paste(outF,head$sta$id,".txt",sep="");
	if(!file.exists(outF)){	# create output dir if does not exists
		dir.create(outF, showWarnings = FALSE, recursive = FALSE, mode = "0777");
	}else{			# remove output file if does exists
		if(file.exists(outName))	file.remove(outName);
	}
	# head
	file.copy(	csagT,	outName,overwrite=TRUE);
	changeVar(	"ID",		head$station$id,outName,outName);
	changeVar(	"LAT",		format(head$station$lat,nsmall=2),outName,outName);
	changeVar(	"LON",		format(head$station$lon,nsmall=2),outName,outName);
	changeVar(	"ALT",		format(head$station$alt,nsmall=2),outName,outName);
	changeVar(	"START",	paste(format(head$period$start,"%Y%m%d"),head$period$startH,sep=""),outName,outName);
	changeVar(	"END",		paste(format(head$period$end,"%Y%m%d"),head$period$endH,sep=""),outName,outName);
	changeVar(	"TYPE",		head$period$type,outName,outName);
	changeVar(	"DESCRIPTION",	head$comm,outName,outName);
	# body
	write.table(table,outName,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE);
}

