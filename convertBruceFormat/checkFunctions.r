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
		print("# WARNING: latitudes are not consistent accross the tmn, tmx and ppt files",quote=FALSE);
	}

# check on longitudes
	minLon<-min(fileHead_tmn$station$lon,fileHead_tmx$station$lon,fileHead_ppt$station$lon);
	maxLon<-max(fileHead_tmn$station$lon,fileHead_tmx$station$lon,fileHead_ppt$station$lon);
	if(minLon!=maxLon){
		stopProcess<-2;
		print("# WARNING: longitudes are not consistent accross the tmn, tmx and ppt files",quote=FALSE);
	}

# check on altitudes
	minAlt<-min(fileHead_tmn$station$alt,fileHead_tmx$station$alt,fileHead_ppt$station$alt);
	maxAlt<-max(fileHead_tmn$station$alt,fileHead_tmx$station$alt,fileHead_ppt$station$alt);
	if(minAlt!=maxAlt){
		stopProcess<-3;
		print("# WARNING: altitudes are not consistent accross the tmn, tmx and ppt files",quote=FALSE);
	}

# print warnings and wait for acknowledgment
	if(stopProcess>0){
		print("### are you aware of the above warning(s)?",quote=FALSE);
		print("### TYPE either: 'c' (resume the process) or 'Q' (quit the process)",quote=FALSE);
		browser();
	}
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
longestPperiod <- function(fileHead_tmn,fileHead_tmx,fileHead_ppt)
{
	stopProcess <- 0;

# init with tmn fileHead
	fileHead<-fileHead_tmn;
	fileHead$period$start<-max(fileHead_tmn$period$start,fileHead_tmx$period$start,fileHead_ppt$period$start);
	fileHead$period$end<-min(fileHead_tmn$period$end,fileHead_tmx$period$end,fileHead_ppt$period$end);

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
	fileHead<-longestPperiod(fileHead_tmn,fileHead_tmx,fileHead_ppt);

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
	if (any(is.na(tmn),na.rm=FALSE)) tmn_missing<-TRUE;
	if (any(is.na(tmx),na.rm=FALSE)) tmx_missing<-TRUE;
	if (any(is.na(ppt),na.rm=FALSE)) ppt_missing<-TRUE;
	
# NaN
	if (any(is.nan(tmn),na.rm=FALSE)) tmn_missing<-TRUE;
	if (any(is.nan(tmx),na.rm=FALSE)) tmx_missing<-TRUE;
	if (any(is.nan(ppt),na.rm=FALSE)) ppt_missing<-TRUE;

# null
	if (any(is.null(tmn),na.rm=FALSE)) tmn_missing<-TRUE;
	if (any(is.null(tmx),na.rm=FALSE)) tmx_missing<-TRUE;
	if (any(is.null(ppt),na.rm=FALSE)) ppt_missing<-TRUE;

# -999
	if (any(tmn==-999,na.rm=TRUE)) tmn_missing<-TRUE;
	if (any(tmx==-999,na.rm=TRUE)) tmx_missing<-TRUE;
	if (any(ppt==-999,na.rm=TRUE)) ppt_missing<-TRUE;

# WARNINGS
	if(tmn_missing){
		print("# WARNING: we detected missing values in the TEMP MIN file");
		print("# are you aware than any additional computation (radn, ET0 ..) is compromised?");
		print("### TYPE either: 'c' (resume the process) or 'Q' (quit the process)",quote=FALSE);
		browser();
	}
	if(tmx_missing){
		print("# WARNING: we detected missing values in the TEMP MAX file");
		print("# are you aware than any additional computation (radn, ET0 ..) is compromised?");
		print("### TYPE either: 'c' (resume the process) or 'Q' (quit the process)",quote=FALSE);
		browser();
	}
	if(ppt_missing){
		print("# WARNING: we detected missing values in the PREC file");
		print("# are you aware than any additional computation (radn, ET0 ..) is compromised?");
		print("### TYPE either: 'c' (resume the process) or 'Q' (quit the process)",quote=FALSE);
		browser();
	}
}
