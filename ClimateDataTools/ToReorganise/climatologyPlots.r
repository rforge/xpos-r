

source('plotFct.r')
source('../convertBruceFormat/bruceFormat.r')

pathToMet<-"/home/crespo/Desktop/12_AgMIP/2012-10-01_fastTrack/AMIP/WiltData_CM/SREStmp/GCMtmp/";
pathToCsag<-"/home/crespo/Desktop/12_AgMIP/2012-10-01_fastTrack/AMIP/WiltData_CSAG/SREStmp/GCMtmp/ppt/";
pathToClim<-"/home/crespo/Desktop/12_AgMIP/2012-10-01_fastTrack/AMIP/WiltData_Clim/";

allStation <- list.files(pathToMet);

for (s in 1:length(allStation)){
	if(strsplit(allStation[s],split="\\.")[[1]][2]!="met") next;	
	print(paste(s,length(allStation),sep="/"));
	graphics.off()

	head <- read_bruceHeadFile(paste(pathToCsag,strsplit(allStation[s],split="\\.")[[1]][1],".txt",sep=""));
	title<-paste("ID:",head$station$id,", coo:",head$station$lat,",",head$station$lon,", period:",head$period$start,head$period$end);
	outFile<-"/home/crespo/Desktop/12_AgMIP/2012-10-01_fastTrack/AMIP/WiltData_Clim"

	sta <- load_obs(paste(pathToMet,allStation[s],sep=""));
	fil <- paste(pathToClim,strsplit(allStation[s],split="\\.")[[1]][1],sep="");

	temp_quantiles(sta,title)
	copyDev2eps(file=paste(fil,"_temp.eps",sep="_"))
	copyDev2jpg(file=paste(fil,"_temp.jpg",sep="_"))
	
	prec_runMonth(metDat=sta,figTit=title,rDayFac=5)
	copyDev2eps(file=paste(fil,"_prec.eps",sep="_"))
	copyDev2jpg(file=paste(fil,"_prec.jpg",sep="_"))
}
