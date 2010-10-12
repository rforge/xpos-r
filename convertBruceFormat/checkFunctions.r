##
 # FILE checkFunctions.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/convertBruceFormat
 ###############################################################################
 #
 # various data checking usually used in the first step
 # either to stop the process when the conversion functions cannot handle it
 # or to warn the user about unexpected data
 #
 ###############################################################################


##
 # CHECK coordinate consistency
 ###############################################################################
 # give WARNINGS that have to be acknowledge to go further
 ###############################################################################
checkCoordinates <- function(fileHead_tmn,fileHead_tmx,fileHead_ppt)
{
	stopProcess<-0;

# check on latitudes
	minLat<-min(fileHead_tmn$station$lat,fileHead_tmx$station$lat,fileHead_ppt$station$lat);
	maxLat<-max(fileHead_tmn$station$lat,fileHead_tmx$station$lat,fileHead_ppt$station$lat);
	if(minLat!=maxLat){
		stopProcess<-1;
		print("# WARNING: unconsitent latitudes -> take tmn one",quote=FALSE);
	}

# check on longitudes
	minLon<-min(fileHead_tmn$station$lon,fileHead_tmx$station$lon,fileHead_ppt$station$lon);
	maxLon<-max(fileHead_tmn$station$lon,fileHead_tmx$station$lon,fileHead_ppt$station$lon);
	if(minLon!=maxLon){
		stopProcess<-2;
		print("# WARNING: unconsitent longitudes -> take tmn one",quote=FALSE);
	}

# check on altitudes
	minAlt<-min(fileHead_tmn$station$alt,fileHead_tmx$station$alt,fileHead_ppt$station$alt);
	maxAlt<-max(fileHead_tmn$station$alt,fileHead_tmx$station$alt,fileHead_ppt$station$alt);
	if(minAlt!=maxAlt){
		stopProcess<-3;
		print("# WARNING: unconsitent altitudes -> take tmn one",quote=FALSE);
	}
	if(minAlt<(-90) || maxAlt>7000){
		stopProcess<-3;
		print("# WARNING: unrealistic altitude -> take tmn one",quote=FALSE);
	}

# print warnings and wait for acknowledgment
#	if(stopProcess>0){
#		print("### are you aware of the above warning(s)?",quote=FALSE);
#		print("### TYPE either: 'c' (resume the process) or 'Q' (quit the process)",quote=FALSE);
#		browser();
#	}
}

##
 # CHECK dates validity
 ###############################################################################
 # give ERRORS and stop the process
 ###############################################################################
checkDates <- function(fileHead_tmn,fileHead_tmx,fileHead_ppt)
{
	stopProcess<-0;

# check on tmn
	if(fileHead_tmn$period$end-fileHead_tmn$period$start < 0){
		stopProcess <- -1;
		print("# ERROR: tmn ending date is prior to tmn starting date",quote=FALSE);
	}

# check on tmx
	if(fileHead_tmx$period$end-fileHead_tmx$period$start < 0){
		stopProcess <- -2;
		print("# ERROR: tmx ending date is prior to tmx starting date",quote=FALSE);
	}

# check on ppt
	if(fileHead_ppt$period$end-fileHead_ppt$period$start < 0){
		stopProcess <- -3;
		print("# ERROR: ppt ending date is prior to ppt starting date",quote=FALSE);
	}

# print errors and stop the process
	if(stopProcess<0){
		print("### The formatting process cannot deal with the above error(s)!",quote=FALSE);
		stop();
	}
}

##
 # CHECK dates validity
 ###############################################################################
 # give ERRORS and stop the process
 ###############################################################################
longestPeriod <- function(fileHead_tmn,fileHead_tmx,fileHead_ppt)
{
	stopProcess <- 0;

# init with tmn fileHead
	fileHead<-fileHead_tmn;
	fileHead$period$start<-max(fileHead_tmn$period$start,fileHead_tmx$period$start,fileHead_ppt$period$start);
	fileHead$period$end<-min(fileHead_tmn$period$end,fileHead_tmx$period$end,fileHead_ppt$period$end);

# check on realistic altitudes
	if(fileHead$station$alt<(-90) || fileHead$station$alt>7000){
		print("# WARNING: unrealistic altitude -> assume altitude = 0",quote=FALSE);
		fileHead$station$alt=0;
	}

# check that there is actually a positive common period
	if(fileHead$period$end-fileHead$period$start < 0){
		stopProcess <- 1;
		print("# ERROR: there is no common period of time given tmn, tmx and ppt data files",quote=FALSE);
	}

# check that we're dealing with same data types
	minType<-min(fileHead_tmn$period$type,fileHead_tmx$period$type,fileHead_ppt$period$type);
	maxType<-max(fileHead_tmn$period$type,fileHead_tmx$period$type,fileHead_ppt$period$type);
	if(minType!=maxType){
		stopProcess <- 2;
		print("# ERROR: there data type (0,1,2,3) are not consitent across tmn, tmx and ppt data files",quote=FALSE);
	}

# stop the process if required
	if(stopProcess>0)	stop();

return(fileHead);
}

################################################################################
 # MAIN FUNCTIONS
 ###############################################################################

##
 # CHECK data consistency and return fileHead with common period
 ###############################################################################
checkData <- function (path)
{
	
	path2file <- paste(path$input,path$folder$tmn,path$file$temp,sep="");
	fileHead_tmn <- read_bruceHeadFile(path2file);

	path2file <- paste(path$input,path$folder$tmx,path$file$temp,sep="");
	fileHead_tmx <- read_bruceHeadFile(path2file);

	path2file <- paste(path$input,path$folder$ppt,path$file$prec,sep="");
	fileHead_ppt <- read_bruceHeadFile(path2file);

	checkCoordinates(fileHead_tmn,fileHead_tmx,fileHead_ppt);
	checkDates(fileHead_tmn,fileHead_tmx,fileHead_ppt);
	fileHead<-longestPeriod(fileHead_tmn,fileHead_tmx,fileHead_ppt);

return(fileHead);
}

##
 # CHECK for missing data
 ###############################################################################
checkMissing <- function(data)
{
	tmn <- data$tmn;	tmn_missing<-FALSE;
	tmx <- data$tmx;	tmx_missing<-FALSE;
	ppt <- data$ppt;	ppt_missing<-FALSE;

# NA
	if (any(is.na(tmn))){
		tmn_missing<-TRUE;
		tmn[is.na(tmn)] <- as.numeric("Na");
	}
	if (any(is.na(tmx))){
		tmx_missing<-TRUE;
		tmx[is.na(tmx)] <- as.numeric("Na");
	}
	if (any(is.na(ppt))){
		ppt_missing<-TRUE;
		ppt[is.na(ppt)] <- as.numeric("Na");
	}
	
# NaN
	if (any(is.nan(tmn))){
		tmn_missing<-TRUE;
		tmn[is.nan(tmn)] <- as.numeric("Na");
	}
	if (any(is.nan(tmx))){
		tmx_missing<-TRUE;
		tmx[is.nan(tmx)] <- as.numeric("Na");
	}
	if (any(is.nan(ppt))){
		ppt_missing<-TRUE;
		ppt[is.nan(ppt)] <- as.numeric("Na");
	}

# null
	if (any(is.null(tmn))){
		tmn_missing<-TRUE;
		tmn[is.null(tmn)] <- as.numeric("Na");
	}
	if (any(is.null(tmx))){
		tmx_missing<-TRUE;
		tmx[is.null(tmx)] <- as.numeric("Na");
	}
	if (any(is.null(ppt))){
		ppt_missing<-TRUE;
		ppt[is.null(ppt)] <- as.numeric("Na");
	}

# -999
	if (any(tmn==-999)){
		tmn_missing<-TRUE;
		tmn[tmn==-999] <- as.numeric("Na");
	}
	if (any(tmx==-999)){
		tmx_missing<-TRUE;
		tmx[tmx==-999] <- as.numeric("Na");
	}
	if (any(ppt==-999)){
		ppt_missing<-TRUE;
		ppt[ppt==-999] <- as.numeric("Na");
	}

# WARNINGS
	if(tmn_missing){
		data$tmn <- tmn;
		print("# WARNING: we detected missing values in the TEMP MIN file",quote=FALSE);
	}
	if(tmx_missing){
		data$tmx <- tmx;
		print("# WARNING: we detected missing values in the TEMP MAX file",quote=FALSE);
	}
	if(ppt_missing){
		data$ppt <- ppt;
		print("# WARNING: we detected missing values in the PREC file",quote=FALSE);
	}


return(data);
}

##
 # CHECK TEMP AND RAIN DATA
 ###############################################################################
checkTmpRain <- function(data)
{
	tmn <- data$tmn;	tmn_missing<-FALSE;
	tmx <- data$tmx;	tmx_missing<-FALSE;
	ppt <- data$ppt;	ppt_missing<-FALSE;

	stopProcess <- 0;
# tmin
	if (any(as.numeric(tmn)<(-50),na.rm=TRUE)){
		stopProcess <- 1;
		print("# WARNING: there is min temperature < -50 !!",quote=FALSE);
	}
# tmax
	if (any(as.numeric(tmx)>70,na.rm=TRUE)){
		stopProcess <- 2;
		print("# WARNING: there is max temperature > 70 !!",quote=FALSE);
	}
# tmin - tmax
	if (any((as.numeric(tmx)-as.numeric(tmn))<0,na.rm=TRUE)){
		stopProcess <- 3;
		print("# WARNING: there is max temperature < min temperature !!",quote=FALSE);
	}
# ppt
	if (any(as.numeric(ppt)<0,na.rm=TRUE)){
		stopProcess <- 4;
		print("# WARNING: there is ppt < 0 !!",quote=FALSE);
	}

# print errors and wait for acknowledgment
	if(stopProcess>0){
		print("# WARNING: suspicious data in your set, are you aware of these ?",quote=FALSE);
		print("### TYPE either: 'c' (resume the process) or 'Q' (quit the process)",quote=FALSE);
		browser();
	}
}

