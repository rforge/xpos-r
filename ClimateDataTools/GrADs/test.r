##
 # FILE test.r
 # AUTHOR olivier crespo
 ####################################################################

## for test purpose only
ctlFile<-'/terra/blue/users/mtadross/umdata/caafa/Grads/ppt.monmn.1960-2008.ctl'
path2world<-'~/Desktop/Optimisation/Sensitivity/DataMap/world.txt'
path2africaBdy<-'~/Desktop/Optimisation/Sensitivity/DataMap/my-africa-bdy.txt'
path2africaCil<-'~/Desktop/Optimisation/Sensitivity/DataMap/my-africa-cil.txt'
path2africaRiv<-'~/Desktop/Optimisation/Sensitivity/DataMap/my-africa-riv.txt'

## read the descriptor file
 # REF http://www.iges.org/grads/gadoc/aboutgriddeddata.html#descriptor
 ####################################################################
read_desctiptorFile<-function(ctlFile)
{
	## test that it is a descriptor file ('.ctl')
	extension <- strsplit(ctlFile,"\\.")[[1]];
	if(extension[length(extension)]!="ctl"){
		print("***** input file is not a GrADs descriptor file, must be *.ctl");
		print("you enter step-by-step mode");
		browser();
	}

	## path to ctl file
	temp <- strsplit(ctlFile,"/")[[1]];
	path2file <- temp[1];
	for (i in 2:(length(temp)-1)){
		path2file <- paste(path2file,temp[i],sep="/");
	}
	path2file <- paste(path2file,"/",sep="");

	meta <- vector("list",10);
	names(meta)[[1]]<-"path";	meta$path<-path2file;
	names(meta)[[2]]<-"name";	meta$name<-temp[length(temp)];

	## read meta file line by line
	temp <- readLines(ctlFile);
	for (l in 1:length(temp)){
		line <- strsplit(temp[l]," ")[[1]];
		
		# meta[[3]]: data set file
		if (line[1]=='dset'){
			dataSet <- 	line[2:length(line)];
			names(meta)[[3]]<-"dset";
			meta$dset <- list("path"=NULL,"name"=NULL);
			if(strsplit(dataSet,"")[[1]][1]=="^"){
				meta$dset$path <- meta$path;
				## all but "^"
				aa <- strsplit(dataSet,"")[[1]];
				meta$dset$name <- aa[2];
				for (i in 3:length(aa)){
					meta$dset$name <- paste(meta$dset$name,aa[i],sep="");
				}
			}else{
				## path to dat file
				aa <- strsplit(dataSet,"/")[[1]];
				path2file <- aa[1];
				for (i in 2:(length(aa)-1)){
					path2file <- paste(path2file,aa[i],sep="/");
				}
				meta$dset$path <- paste(path2file,"/",sep="");
				meta$dset$name <- aa[length(aa)];
			}
		}
		
		# meta[[4]]: title
		if (line[1]=='title'){
			names(meta)[[4]]<-"title";
			meta$title <- line[2];
			for (i in 3:length(line)){
				meta$title <- paste(meta$title,line[i],sep=" ");
			}
		}

		# meta[[5]]: undef
		if (line[1]=='undef'){
			names(meta)[[5]]<-"undef";
			meta$undef <- line[2];
		}

		# meta[[6,7,8,9]]: respectively longitude/latitude/altitude/time
		if (line[1]=='xdef' || line[1]=='ydef' || line[1]=='zdef' || line[1]=='tdef'){
			mappingOption <- line[3];
			if(mappingOption=='linear' || mappingOption=='LINEAR'){
				tmpList <- list("xnum"=line[2],"mapping"=line[3],"start"=line[4],"incr"=line[5]);
			}
			if(mappingOption=='levels' || mappingOption=='LEVELS'){
				# untested
				tmpList <- vector("list",length(line)-1);
				for (i in 2:(4+line[2])){
					tmpList[[i]] <- line[i+1];
					if(i==2)	names(tmpList[[i-1]])<-"xnum";
					if(i==3)	names(tmpList[[i-1]])<-"mapping";
					if(i>3)		names(tmpList[[i-1]])<-paste("lev",i-3,sep="");
				}
			}
			if(line[1]=='xdef'){	names(meta)[[6]]<-"xdef";	meta$xdef <- tmpList;}
			if(line[1]=='ydef'){	names(meta)[[7]]<-"ydef";	meta$ydef <- tmpList;}
			if(line[1]=='zdef'){	names(meta)[[8]]<-"zdef";	meta$zdef <- tmpList;}
			if(line[1]=='tdef'){	names(meta)[[9]]<-"tdef";	meta$tdef <- tmpList;}
		}
		if (line[1]=='vars'){
			names(meta)[[10]] <- "vars";
			meta$vars <- vector("list",as.integer(line[2]));
			for (i in 1:length(meta$vars)){
				l<-l+1;
				line <- strsplit(temp[l]," ")[[1]];
				meta$vars[[i]] <- list("name"=line[1],"levs"=line[2],"units"=line[3],"descr"=line[4]);
			}
			l<-l+1;
		}
	}

return (meta);
}

## read the binary file
 ####################################################################
 # NB I know we're reading the binary file here,
 # but you still need to put in the *.ctl file as input
 # to get all necessary dimension info
 ####################################################################
read_binaryFile<-function(ctlFile)
{
	meta <- read_desctiptorFile(ctlFile);
	nLon <- as.integer(meta$xdef$xnum); lonIncr <- as.numeric(meta$xdef$incr);
	nLat <- as.integer(meta$ydef$xnum); latIncr <- as.numeric(meta$ydef$incr);
	nAlt <- as.integer(meta$zdef$xnum);
	nTim <- as.integer(meta$tdef$xnum);
	gridSize <- nLon*nLat*nAlt*nTim;

	data <- array(readBin(paste(meta$dset$path,meta$dset$name,sep="/"),numeric(),n=gridSize,size=4),dim=c(nLon,nLat,nAlt,nTim));

#	world <- read.table(path2world);
#	image(data[,,1,1]);
#	lines(x=((world[,1]+180)/lonIncr/nLon),y=((world[,2]+90)/latIncr/nLat),type="l",lty=1);

return(data);
}

## plot partial binary file (default: RZA)
 ####################################################################
 # NB I know we're reading the binary file here,
 # but you still need to put in the *.ctl file as input
 # to get all necessary dimension info
 ####################################################################
plot_partBinaryFile<-function(ctlFile,lonLim=c(16,33),latLim=c(-35,-22),altLim)
{
	if(dev.cur()!=1)	dev.off();

	meta <- read_desctiptorFile(ctlFile);
	nLon <- as.integer(meta$xdef$xnum); lonIncr <- as.numeric(meta$xdef$incr);
	nLat <- as.integer(meta$ydef$xnum); latIncr <- as.numeric(meta$ydef$incr);
	nAlt <- as.integer(meta$zdef$xnum);
	nTim <- as.integer(meta$tdef$xnum);
	gridSize <- nLon*nLat*nAlt*nTim;

	data <- array(readBin(paste(meta$dset$path,meta$dset$name,sep="/"),numeric(),n=gridSize,size=4),dim=c(nLon,nLat,nAlt,nTim));
	world <- read.table(path2world);
	afrBorders <- read.table(path2africaBdy);
	afrContinent <- read.table(path2africaCil);
	afrRivers <- read.table(path2africaRiv);

	# translate lon and lat limits in rowLim and colLim
	rowLim <- (lonLim+180)/lonIncr/nLon;
	colLim <- (latLim+90)/latIncr/nLat;

	image(data[,,1,1],xlim=rowLim,ylim=colLim,asp=.55);
#	lines(x=((world[,1]+180)/lonIncr/nLon),y=((world[,2]+90)/latIncr/nLat),type="l",lty=1);
	lines(x=((afrBorders[,1]+180)/lonIncr/nLon),y=((afrBorders[,2]+90)/latIncr/nLat),type="l",lty=1);
	lines(x=((afrContinent[,1]+180)/lonIncr/nLon),y=((afrContinent[,2]+90)/latIncr/nLat),type="l",lty=1);
#	lines(x=((afrRivers[,1]+180)/lonIncr/nLon),y=((afrRivers[,2]+90)/latIncr/nLat),type="l",lty=1);
}

## a few short cuts
 ####################################################################
plot_RZA<-function(ctlFile)
{
	plot_partBinaryFile(ctlFile,lonLim=c(16,33),latLim=c(-35,-22));
}
plot_SA<-function(ctlFile)
{
# eastern variations
	plot_partBinaryFile(ctlFile,lonLim=c(8,43),latLim=c(-35,0));
}
plot_Af<-function(ctlFile)
{
	plot_partBinaryFile(ctlFile,lonLim=c(-18,52),latLim=c(-35,38));
# eastern variations:
#	lonLim(x,52) Continent only (Madagascar included)
#	lonLim(x,56) Reunion included
#	lonLim(x,58) Mauritius included
#	lonLim(x,64) Port Mathurin included
# western variations:
#	lonLim(-18,x) Continent only
#	lonLim(-26,x) Cape Verde inluded
	
}


