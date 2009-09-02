##
 # FILE convertFunctions.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
 ###############################################################################

################################################################################
## USER SETTINGS START HERE
## fill in what you need only. I know it sounds obvious, but ...
################################################################################
## 
 # DATA: where and what (REQUIRED)
 ###############################################################################
init_data <- function()
{
data <- 	list(	"path2input"=		"../../Conversion/Inputs/",
			"path2output"=		"../../Conversion/Outputs/",
			"folderNames"=list(	"tmin"=	"ex_tmn/",
							"tmax"=	"ex_tmx/",
							"ppt"=	"ex_ppt/"
					)
			);
return(data);
}

## 
 # STATIONS: names (NOT REQUIRED)
 ###############################################################################
init_stationNames <- function()
{
station <- array(c(	"Buffalo_Range.txt",
				"Buhera.txt",
				"Chipinge.txt",
				"Chisengu.txt",
				"Chisumbanje.txt",
				"Masvingo.txt",
				"Middle_Save.txt",
				"Mutare_Fire.txt",
				"Rusape.txt",
				"Wedza.txt",
				"ZAKA.txt",
				"ZVISHAVANE.txt"
			)
		);
return(station);
}

## 
 # GC MODELS NAMES (MODEL 1 IS NCEP) (NOT REQUIRED)
 ###############################################################################
init_gcmNames <- function()
{
gcmNames <- list(	"ncep"=	"ncep2.1",
			"cccm"=	list(	"con"=	"cccma_cgcm3_1",
						"futA"=	"cccma_cgcm3_1-fa",
						"futB"=	"cccma_cgcm3_1-fb"),
			"cnrm"=	list(	"con"=	"cnrm_cm3",
						"futA"=	"cnrm_cm3-fa",
						"futB"=	"cnrm_cm3-fb"),
			"csiro30"=	list(	"con"=	"csiro_mk3_0",
						"futA"=	"csiro_mk3_0-fa",
						"futB"=	"csiro_mk3_0-fb"),
			"csiro35"=	list(	"con"=	"csiro_mk3_5",
						"futA"=	"csiro_mk3_5-fa",
						"futB"=	"csiro_mk3_5-fb"),
			"gfdl"=	list(	"con"=	"gfdl_cm2_0",
						"futA"=	"gfdl_cm2_0-fa",
						"futB"=	"gfdl_cm2_0-fb"),
			"giss"=	list(	"con"=	"giss_model_e_r",
						"futA"=	"giss_model_e_r-fa",
						"futB"=	"giss_model_e_r-fb"),
			"ipsl"=	list(	"con"=	"ipsl_cm4",
						"futA"=	"ipsl_cm4-fa",
						"futB"=	"ipsl_cm4-fb"),
			"echo"=	list(	"con"=	"miub_echo_g",
						"futA"=	"miub_echo_g-fa",
						"futB"=	"miub_echo_g-fb"),
			"echam"=	list(	"con"=	"mpi_echam5",
						"futA"=	"mpi_echam5-fa",
						"futB"=	"mpi_echam5-fb"),
			"cgcm"=	list(	"con"=	"mri_cgcm2_3_2a",
						"futA"=	"mri_cgcm2_3_2a-fa",
						"futB"=	"mri_cgcm2_3_2a-fb")
	);
return(gcmNames);
}

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
	# tmin and tmax are the mean of 14/02 and 15/02

	# 01-01 +44 days is the 14-02
	# 01-01 +45 days is the 15-02
	new1 <- mean(oldYear[44:45,1]);
	new2 <- mean(oldYear[44:45,2]);
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
     ## my guess
	# 360 days is for 30 days a month all year round
	# to be confirmed
	
     ## add 1 day in the middle of jan, mar, may, jul, aug, oct, dec
	# in feb: minus 2 for non leap, minus 1 for leap year
	# ppt is 0
	# tmin and tmax are the mean of 28/02 and 01/03

	# 15jan(15)
	new1 <- mean(oldYear[15:16,1]);
	new2 <- mean(oldYear[15:16,2]);
	new3 <- 0;
	temp <- oldYear[1:15,1:dim(oldYear)[2]];
	temp <- rbind(temp,c(new1,new2,new3));
	temp <- rbind(temp,oldYear[16:dim(oldYear)[1],1:dim(oldYear)[2]]);

	# feb
	before <- temp[1:31,1:dim(temp)[2]];
	february <- temp[32:61,1:dim(temp)[2]];
	after <- temp[62:dim(temp)[1],1:dim(temp)[2]];
	pptOldFeb <- sum(february[,3]);
	february <- array(february[-(round(runif(1)*dim(february)[1]-1)+1)],dim=c(29,dim(oldYear)[2]));
	if (!is.leapYear(year))	february <- array(february[-(round(runif(1)*dim(february)[1]-1)+1)],dim=c(28,dim(oldYear)[2]));
	pptNewFeb <- sum(february[,3]);
	if (pptNewFeb<pptOldFeb){
		day1 <- round(runif(1)*dim(february)[1]-1)+1;
		february[day1,3] <- february[day1,3]+ 0.5* (pptOldFeb-pptNewFeb);
		day2 <- round(runif(1)*dim(february)[1]-1)+1;
		february[day2,3] <- february[day2,3]+ 0.5* (pptOldFeb-pptNewFeb);
	}	
	temp <- before;
	temp <- rbind(temp,february);
	temp <- rbind(temp,after);

	# 15mar(73) - 15may(134) - 15jul(195) - 15aug(226) - 15oct(287) - 15dec(348)
	for (d in c(73,134,195,226,287,348)){
		if (is.leapYear(year)) d <- d+1;
		new1 <- mean(temp[d:(d+1),1]);
		new2 <- mean(temp[d:(d+1),2]);
		new3 <- 0;
		newYear <- temp[1:d,1:dim(temp)[2]];
		newYear <- rbind(newYear,c(new1,new2,new3));
		newYear <- rbind(newYear,temp[(d+1):dim(temp)[1],1:dim(temp)[2]]);
		temp <- newYear;
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
compute_radn <- function(table,station)
{	# table is made of year,julianDay,tmin,tmax,ppt

	table <- array(as.numeric(table),dim=dim(table));
	table <- cbind(table,array(NA,dim=dim(table)[1]));

	for (line in 1:dim(table)[1]){
		a 	<- 0.16;	# a in [0.1,1.2] for example 0.16 inland, 0.19 coastal
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
			table_bef <- table[1:dayNo_bef,1:dim(table)[2]];
			# pull out section
			oldSection <- table[(dayNo_bef+1):(dayNo_bef+365),1:dim(table)[2]];
			# cut after
			table_aft <- table[(dayNo_bef+365+1):dim(table)[1],1:dim(table)[2]];
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
		table_bef <- table[1:dayNo_bef,1:dim(table)[2]];
		# pull out section
		oldSection <- table[(dayNo_bef+1):(dayNo_bef+365),1:dim(table)[2]];
		# cut after
		table_aft <- table[(dayNo_bef+365+1):dim(table)[1],1:dim(table)[2]];
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
convert_OneStation4OnePeriod <- function(data,stationName)
{
# read data files
	path2file <- paste(data$path2input,data$folderName$tmin,stationName,sep="");
	fileHead <- read_bruceHeadFile(path2file);

	# make one table per station-period
	table <- as.array(as.numeric(scan(path2file,what="numeric",sep="\n",skip=3,nlines=-1,quiet=TRUE)));
	path2file <- paste(data$path2input,data$folderName$tmax,stationName,sep="");
	table <- cbind(table,as.array(as.numeric(scan(path2file,what="numeric",sep="\n",skip=3,nlines=-1,quiet=TRUE))));
	path2file <- paste(data$path2input,data$folderName$ppt,stationName,sep="");
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
	apsim_table <- compute_radn(apsim_table,fileHead$station);
# compute tav and amp
	tavNamp <- compute_tavNamp(apsim_table);

# format numeric values
	apsim_table <- format(apsim_table,digits=2);
	apsim_table[,1:2] <- as.integer(apsim_table[,1:2]);

# write it into a .met file
	# head
	station <- strsplit(stationName,"\\.")[[1]][1];
	file.copy("metFileTemplate.met",paste(data$path2output,station,".met",sep=""),overwrite=TRUE);
	changeVar(	"station_id",fileHead$station$id,	paste(data$path2output,station,".met",sep=""),paste(data$path2output,station,".met",sep=""));
	changeVar(	"station_comm",fileHead$comm,		paste(data$path2output,station,".met",sep=""),paste(data$path2output,station,".met",sep=""));
	changeVar(	"stat_lat",fileHead$station$lat,	paste(data$path2output,station,".met",sep=""),paste(data$path2output,station,".met",sep=""));
	changeVar(	"stat_lon",fileHead$station$lon,	paste(data$path2output,station,".met",sep=""),paste(data$path2output,station,".met",sep=""));
	changeVar(	"stat_alt",fileHead$station$alt,	paste(data$path2output,station,".met",sep=""),paste(data$path2output,station,".met",sep=""));
	changeVar(	"period_tav",tavNamp$tav,		paste(data$path2output,station,".met",sep=""),paste(data$path2output,station,".met",sep=""));
	changeVar(	"period_amp",tavNamp$amp,		paste(data$path2output,station,".met",sep=""),paste(data$path2output,station,".met",sep=""));
	# body
	apsim_table <- format(apsim_table,justify="right",width=6);
	write.table(apsim_table,paste(data$path2output,station,".met",sep=""),quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE);
}