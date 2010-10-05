##
 # FILE bruceFormat.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/convertBruceFormat
 ###############################################################################
 #
 # functions required to read, and work on bruce data format
 #
 ###############################################################################


## 
 # HEAD FILES FORMAT
 ###############################################################################
read_bruceHeadFile <- function(path2file)
{
	## first line
	temp <- scan(path2file,what="character",sep=" ",nlines=1,quiet=TRUE);
	temp <- temp[temp!=""];
	station <- list("id"=temp[1],"lat"=as.numeric(temp[2]),"lon"=as.numeric(temp[3]),"alt"=as.numeric(temp[4]));

	## second line
	temp <- scan(path2file,what="character",sep=" ",skip=1,nlines=1,quiet=TRUE);
	temp <- temp[temp!=""];
	period <- list("start"=as.Date(temp[1],"%Y%m%d"),"end"=as.Date(temp[2],"%Y%m%d"),"type"=as.numeric(temp[3]));
	## for info
	# year	=	format(date,"%Y")
	# month	=	format(date,"%m")
	# day	=	format(date,"%d")

	## third line
	temp <- scan(path2file,what="character",sep=" ",skip=2,nlines=1,quiet=TRUE);
	temp <- temp[temp!=""];
	comm <- temp[1];

return(list("station"=station,"period"=period,"comment"=comm));
}

## 
 # IMPORT DATA according to common period
 ###############################################################################
importData <- function(path,fileHead)
{
# import all
	# tmn
	path2file <- paste(path$input,path$folder$tmn,path$file$temp,sep="");
	head_tmn <- read_bruceHeadFile(path2file);
	tmn <- as.array(as.numeric(scan(path2file,what="numeric",sep="\n",skip=3,nlines=-1,quiet=TRUE)));
	# tmx
	path2file <- paste(path$input,path$folder$tmx,path$file$temp,sep="");
	head_tmx <- read_bruceHeadFile(path2file);
	tmx <- as.array(as.numeric(scan(path2file,what="numeric",sep="\n",skip=3,nlines=-1,quiet=TRUE)));
	# ppt
	path2file <- paste(path$input,path$folder$ppt,path$file$prec,sep="");
	head_ppt <- read_bruceHeadFile(path2file);
	ppt <- as.array(as.numeric(scan(path2file,what="numeric",sep="\n",skip=3,nlines=-1,quiet=TRUE)));

# cut the tail of files that do not belong into the common period
	# tmn
	tail <- 0; tail <- head_tmn$period$end-fileHead$period$end;
	if(tail>0) tmn <- tmn[1:(dim(tmn)[1]-tail)];
	# tmx
	tail <- 0; tail <- head_tmx$period$end-fileHead$period$end;
	if(tail>0) tmx <- tmx[1:(dim(tmx)[1]-tail)];
	# ppt
	tail <- 0; tail <- head_ppt$period$end-fileHead$period$end;
	if(tail>0) ppt <- ppt[1:(dim(ppt)[1]-tail)];

# cut the head of files that do not belong into the common period
	# tmn
	head <- 0; head <- fileHead$period$start-head_tmn$period$start;
	if(head>0) tmn <- tmn[(head+1):dim(tmn)[1]];
	# tmx
	head <- 0; head <- fileHead$period$start-head_tmx$period$start;
	if(head>0) tmx <- tmx[(head+1):dim(tmx)[1]];
	# ppt
	head <- 0; head <- fileHead$period$start-head_ppt$period$start;
	if(head>0) ppt <- ppt[(head+1):dim(ppt)[1]];

# check period consistencies
	if( dim(tmn)!=dim(tmx) || dim(tmn)!=dim(ppt) || dim(tmx)!=dim(ppt)){
		print("# ERROR: not your fault, but the importData function (in bruceFormat.r) is failing producing files of same length :(");
		stop();
	}

return(list("tmn"=tmn,"tmx"=tmx,"ppt"=ppt));
}

##
 # is that year a leap year?
 ###############################################################################
is.leapYear <- function(year)
{
	## trusting R date classe
	## otherwise there would be a conflict sooner or later anyway
	start <- as.Date(paste("01","01",year,sep="-"),"%d-%m-%Y");
	end <- as.Date(paste("31","12",year,sep="-"),"%d-%m-%Y");

	dayNo <- end-start +1;
	switch(dayNo-364,
		leap <- FALSE,
		leap <- TRUE,
	);
return(leap);
}

##
 # TRANSFORM 365 DAYS A YEAR INTO 366
 ###############################################################################
transform_365into366 <- function(oldYear)
{
     ## my guess
	# 365 days is for real years without leap years
	# i.e. with no 29th of February
	# to be confirmed
	
     ## add 1 day in the middle of february
	# ppt is 0
	# tmin and tmax are copied (randomly) either from previous or following day

	# 01-01 +44 days is the 14-02
	# 01-01 +45 days is the 15-02
	ifelse(runif(1)<0.5,day<-44,day<-45);
	new1 <- oldYear[day,1];
	new2 <- oldYear[day,2];
	new3 <- 0;

	# 01-01 +44 as before
	# 15-02 is new
	# 01-01 +45 starts the 16-02 
	newYear <- oldYear[1:44,1:dim(oldYear)[2]];
	newYear <- rbind(newYear,c(new1,new2,new3));
	newYear <- rbind(newYear,oldYear[45:dim(oldYear)[1],1:dim(oldYear)[2]]);

return(newYear);
}

##
 # TRANSFORM 360 DAYS A YEAR INTO REAL DAYS No
 ###############################################################################
transform_360intoREAL <- function(oldYear,year)
{
     ## leap year: add 1 day in the middle of jan, mar, may, jul, sep, nov
     ## non leap year: add 1 day in the middle of mar, may, jul, sep, nov
	# ppt is 0
	# tmin and tmax are copied (randomly) either from previous or following day

	ifelse (is.leapYear(year),step<-52,step<-60);
	d<-step;
	while(d<(365-step)){
		ifelse(runif(1)<0.5,day<-d,day<-d+1);
		new1 <- oldYear[day,1];
		new2 <- oldYear[day,2];
		new3 <- 0;

		newYear <- oldYear[1:d,1:dim(oldYear)[2]];
		newYear <- rbind(newYear,c(new1,new2,new3));
		newYear <- rbind(newYear,oldYear[(d+1):dim(oldYear)[1],1:dim(oldYear)[2]]);
		oldYear <- newYear;

		d <- d+step;
	}

return(newYear);
}

##
 # CHECK THAT DIM OF THE TABLE FITS THE REAL No OF DAYS
 ###############################################################################
check_dayVSdim <- function(sDate,eDate,linNo)
{
	dayNo <- eDate-sDate +1;
	if (dayNo != linNo){
		stop("*** wrong number of Days: check_dayVSdim (bruceFormat.r)");
	}
}

##
 # TRANSFORM DAY No OVER A PERIOD TO REAL _ type 365 days
 ###############################################################################
transform_type1 <- function(data,head)
{
	table <- data$tmn;
	table <- cbind(table,data$tmx);
	table <- cbind(table,data$ppt);

	for(y in format(head$period$start,"%Y"):format(head$period$end,"%Y")){
		if (is.leapYear(y)){
			# cut before
			dayNo_bef <- as.Date(paste("01","01",y,sep="-"),"%d-%m-%Y") - head$period$start;
			if(dayNo_bef>0){
				table_bef <- table[1:dayNo_bef,1:dim(table)[2]];
			}else{
				table_bef <- NULL;
			}
			# pull out section
			oldSection <- table[(dayNo_bef+1):(dayNo_bef+365),1:dim(table)[2]];
			# cut after
			if(y!=format(head$period$end,"%Y")){
				table_aft <- table[(dayNo_bef+365+1):dim(table)[1],1:dim(table)[2]];
			}else{ # no after
				table_aft <- NULL;
			}
			# transform
			newSection <- transform_365into366(oldSection);
			# paste
			table <- table_bef;
			table <- rbind(table,newSection);
			table <- rbind(table,table_aft);
		}
	}
	check_dayVSdim(head$period$start,head$period$end,dim(table)[1]);

	data$tmn <- as.array(table[,1],dim=dim(table)[1]);
	data$tmx <- as.array(table[,2],dim=dim(table)[1]);
	data$ppt <- as.array(table[,3],dim=dim(table)[1]);
return(data);
}

##
 # TRANSFORM DAY No OVER A PERIOD TO REAL _ type 360 days
 ###############################################################################
transform_type2 <- function(table,head)
{
	table <- data$tmn;
	table <- cbind(table,data$tmx);
	table <- cbind(table,data$ppt);

	for(y in format(head$period$start,"%Y"):format(head$period$end,"%Y")){
		# cut before
		dayNo_bef <- as.Date(paste("01","01",y,sep="-"),"%d-%m-%Y") - head$period$start;
		if(dayNo_bef>0){
			table_bef <- table[1:dayNo_bef,1:dim(table)[2]];
		}else{
			table_bef <- NULL;
		}
		# pull out section
		oldSection <- table[(dayNo_bef+1):(dayNo_bef+360),1:dim(table)[2]];
		# cut after
		if(y!=format(head$period$end,"%Y")){
			table_aft <- table[(dayNo_bef+360+1):dim(table)[1],1:dim(table)[2]];
		}else{ # no after
			table_aft <- NULL;
		}
		# transform
		newSection <- transform_360intoREAL(oldSection,y);
		# paste
		table <- table_bef;
		table <- rbind(table,newSection);
		table <- rbind(table,table_aft);
	}
	check_dayVSdim(head$period$start,head$period$end,dim(table)[1]);

	data$tmn <- as.array(table[,1],dim=dim(table)[1]);
	data$tmx <- as.array(table[,2],dim=dim(table)[1]);
	data$ppt <- as.array(table[,3],dim=dim(table)[1]);
return(data);
}

