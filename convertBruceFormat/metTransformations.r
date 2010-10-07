##
 # FILE metTransformations.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/convertBruceFormat
 ###############################################################################
 #
 # functions required to adapt, complete transform weather data
 #
 ###############################################################################

##
 # CREATE YEAR AND JULIAN DAYS
 ###############################################################################
createYearJulianDays <- function(data,fileHead)
{
	firstYear <- format(fileHead$period$start,"%Y");
	lastYear <- format(fileHead$period$end,"%Y");
	firstDay <- fileHead$period$start-as.Date(paste(firstYear,"01","01",sep="-"),"%Y-%m-%d")+1;
	lastDay <- fileHead$period$end-as.Date(paste(lastYear,"01","01",sep="-"),"%Y-%m-%d")+1;
	
	apsim_year <- array(firstYear,dim=1);
	apsim_julDay <- array(firstDay,dim=1);
	li <- 1;
	for (y in firstYear:lastYear){
		if (y == firstYear) d <- firstDay;
		repeat{
			# boundaries conditions
			if(y==lastYear && d==lastDay)	break;
			if(!is.leapYear(y) && d==365)	{d<-0; break;}
			if(d==366)	{d<-0; break;}
			d <- d+1;
			li <- li+1;
			# apsim table update
			apsim_year <- array(c(apsim_year,y),dim=dim(apsim_year)+1); 
			apsim_julDay <- array(c(apsim_julDay,d),dim=dim(apsim_julDay)+1);
		}
	}

	data <- list("tmn"=data$tmn,"tmx"=data$tmx,"ppt"=data$ppt,"year"=apsim_year,"julDay"=apsim_julDay);
return(data)
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

##
 # CHANGE an old known variable (characters string) into a new one
 ###############################################################################
 # not really attached to bruce data
 # but useful for all, and bruce data functions are used by all
 ###############################################################################
changeVar <- function(oldVar,newVar,oldFile,newFile)
{	
	tempFile <- readLines(oldFile,n=-1,warn=FALSE);
	tempFile <- sub(oldVar, newVar, tempFile);
	# for double occurences on the same line (e.g. sowing date at day)
	tempFile <- sub(oldVar, newVar, tempFile);
	writeLines(tempFile,newFile,sep="\n");
}

