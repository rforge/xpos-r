##
 # FILE convertFunctions.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
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
	# day		=	format(date,"%d")

	## third line
	temp <- scan(path2file,what="character",sep=" ",skip=2,nlines=1,quiet=TRUE);
	temp <- temp[temp!=""];
	comm <- temp[1];

return(list("station"=station,"period"=period,"comment"=comm));
}
################################################################################
## USER SETTINGS END HERE
## meaning: be careful if you change anything below
################################################################################

################################################################################
## FUNCTIONS THAT YOU MAY WANT TO PLAY WITH
################################################################################
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
 # SOLAR RADIATION ESTIMATION
 ###############################################################################
 # see
 # Ball, R.A. and Purcell, L.C. and Carey, S.K.,
 # Evaluation of Solar Radiation Prediction Models in North America,
 # Agronomy Journal vol.96(2), pages 391-397, 2004
 ###############################################################################
compute_radn <- function(table,station,inland)
{	
	if (is.null(inland)){
		print("missing parameter: (inland=TRUE) for inland station, inland=FALSE for coastal station");
		stop();
	}

	# table is made of year,julianDay,tmin,tmax,ppt
	table <- array(as.numeric(table),dim=dim(table));
	table <- cbind(table,array(NA,dim=dim(table)[1]));

	for (line in 1:dim(table)[1]){
		a 	<- ifelse(inland,0.16,0.19);	# a in [0.1,1.2] for example 0.16 inland, 0.19 coastal
		latr 	<- station$lat * pi /180;
		delta <- 0.4093*sin((2*pi*table[line,2]/365)-1.39);
		ws	<- acos(-tan(latr)*tan(delta));
		Rsolar<- 118.08*(1+0.033*cos(0.0172*table[line,2]))*(ws*sin(latr)*sin(delta)+cos(latr)*cos(delta)*sin(ws))/pi;
		ks	<- a*(1+2.7*10^(-5)*station$alt)*sqrt(table[line,4]-table[line,3]);

		table[line,6] <- ks*Rsolar;
	}
return(table);
}

################################################################################
## FUNCTIONS THAT YOU DO NOT WANT TO PLAY WITH
################################################################################
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
 # CHECK THAT DIM OF THE TABLE FITS THE REAL No OF DAYS
 ###############################################################################
check_dayVSdim <- function(sDate,eDate,linNo)
{
	dayNo <- eDate-sDate +1;
	if (dayNo != linNo){
		stop("*** wrong number of Days: check_dayVSdim (convertD2Afct.r)");
	}
}

##
 # TRANSFORM DAY No OVER A PERIOD _ type 365 days
 ###############################################################################
transform_type1 <- function(table,head)
{
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
return(table);
}

##
 # TRANSFORM DAY No OVER A PERIOD _ type 360 days
 ###############################################################################
transform_type2 <- function(table,head)
{
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
return(table);
}

##
 # COMPUTE TAV AND AMP APSIM CONSTANTS
 ###############################################################################
compute_tavNamp <- function(table)
{
	# daily mean
	table <- cbind(table,array(NA,dim=dim(table)[1]));
	for (line in 1:dim(table)[1]){
		table[line,dim(table)[2]] <- mean(table[line,3:4]);
	}
	monthlyMean <- array(0,dim=c(12,3));
	for (line in 1:dim(table)[1]){
		month <- as.numeric(format(as.Date(table[line,2]-1,origin=paste(table[line,1],"-01-01",sep="")),"%m"));
		monthlyMean[month,1] <- monthlyMean[month,1]+table[line,dim(table)[2]];
		monthlyMean[month,2] <- monthlyMean[month,2]+1;
	}
	for (m in 1:12){
		monthlyMean[m,3] <- monthlyMean[m,1]/monthlyMean[m,2];
	}

	amp <- format(max(monthlyMean[,3])-min(monthlyMean[,3]),digits=4);
	tav <- format(mean(monthlyMean[,3]),digits=4);

return(list("amp"=amp,"tav"=tav));
}

################################################################################
## MAIN
################################################################################
##
 # CONVERT 1 station for 1 time period
 ###############################################################################
convert_OneStation4OnePeriod <- function(path,stationName_tem,stationName_ppt,outName,inland=NULL)
{
# read data files
	path2file <- paste(path$input,path$data$tmin,stationName_tem,sep="");
	fileHead <- read_bruceHeadFile(path2file);

	# make one table per station-period
	table <- as.array(as.numeric(scan(path2file,what="numeric",sep="\n",skip=3,nlines=-1,quiet=TRUE)));
	path2file <- paste(path$input,path$data$tmax,stationName_tem,sep="");
	table <- cbind(table,as.array(as.numeric(scan(path2file,what="numeric",sep="\n",skip=3,nlines=-1,quiet=TRUE))));
	path2file <- paste(path$input,path$data$ppt,stationName_ppt,sep="");
	table <- cbind(table,as.array(as.numeric(scan(path2file,what="numeric",sep="\n",skip=3,nlines=-1,quiet=TRUE))));

# transform it if needed into real
	switch(fileHead$period$type+1,
		{	# 0 is for real
		},{	# 1 is for 365
			table <- transform_type1(table,fileHead);
		},{	# 2 is for 360
			table <- transform_type2(table,fileHead);
		}
	);
	check_dayVSdim(fileHead$period$start,fileHead$period$end,dim(table)[1]);

# format it to APSIM requirements
	firstYear <- format(fileHead$period$start,"%Y");
	lastYear <- format(fileHead$period$end,"%Y");
	firstDay <- fileHead$period$start-as.Date(paste(firstYear,"01","01",sep="-"),"%Y-%m-%d")+1;
	lastDay <- fileHead$period$end-as.Date(paste(lastYear,"01","01",sep="-"),"%Y-%m-%d")+1;
	apsim_table <- array(c(firstYear,firstDay,table[1,1],table[1,2],table[1,3]),dim=c(1,5));
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
			apsim_line <- array(c(y,d,table[li,1],table[li,2],table[li,3]),dim=c(1,5));
			apsim_table <- rbind(apsim_table,apsim_line);
		}
	}

# compute radiation
	apsim_table <- compute_radn(apsim_table,fileHead$station,inland);
# compute tav and amp
	tavNamp <- compute_tavNamp(apsim_table);

# waiting for a nicer solution within apsim
# make year a fake, and realY the actual year
	fake_year <- array(apsim_table[,1]-ifelse(format(fileHead$period$end,"%Y")>=2065,100,0),dim=dim(apsim_table)[1]);
	apsim_table <- cbind(fake_year,apsim_table);

# format numeric values
	apsim_table <- format(apsim_table,digits=2);
	apsim_table[,1:3] <- as.integer(apsim_table[,1:3]);

# write it into a .met file
	# create output dir if does not exists
	if(!file.exists(path$output)){
		dir.create(path$output, showWarnings = FALSE, recursive = FALSE, mode = "0777");
	}
	# head
#	station <- strsplit(stationName_tem,"\\.")[[1]][1];
	file.copy("metFileTemplate.met",paste(path$output,outName,".met",sep=""),overwrite=TRUE);
	changeVar(	"station_id",fileHead$station$id,	paste(path$output,outName,".met",sep=""),paste(path$output,outName,".met",sep=""));
	changeVar(	"station_comm",fileHead$comm,		paste(path$output,outName,".met",sep=""),paste(path$output,outName,".met",sep=""));
	changeVar(	"stat_lat",fileHead$station$lat,	paste(path$output,outName,".met",sep=""),paste(path$output,outName,".met",sep=""));
	changeVar(	"stat_lon",fileHead$station$lon,	paste(path$output,outName,".met",sep=""),paste(path$output,outName,".met",sep=""));
	changeVar(	"stat_alt",fileHead$station$alt,	paste(path$output,outName,".met",sep=""),paste(path$output,outName,".met",sep=""));
	changeVar(	"period_tav",tavNamp$tav,		paste(path$output,outName,".met",sep=""),paste(path$output,outName,".met",sep=""));
	changeVar(	"period_amp",tavNamp$amp,		paste(path$output,outName,".met",sep=""),paste(path$output,outName,".met",sep=""));
	# body
	apsim_table <- format(apsim_table,justify="right",width=6);
	write.table(apsim_table,paste(path$output,outName,".met",sep=""),quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE);
}
