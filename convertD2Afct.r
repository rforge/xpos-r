##
 # FILE convertD2Afct.r
 # AUTHOR olivier crespo
 #
 # main needed procedures involved in the dowscaled to apsim climate data format
 ###############################################################################

################################################################################
## USER SETTINGS START HERE
################################################################################

## 
 # DATA: where and what
 ###############################################################################
init_data <- function()
{
	data <- 	list(	"path2input"=		"../Conversion/Inputs/",
				"path2output"=		"../Conversion/Outputs/",
				"folderNames"=list(	"tmin"=	"ex_tmn/",
								"tmax"=	"ex_tmx/",
								"ppt"=	"ex_ppt/"
								#,"rad"=	NA
				)
			);

return(data);
}

## 
 # STATIONS: names
 ###############################################################################
init_stationName <- function()
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
	));

return(station);
}


## 
 # GC MODELS NAMES (MODEL 1 IS NCEP)
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
	if (dayNo != linNo)	stop("*** wrong number of Days: check_dayVSdim (convertD2Afct.r)");
}

##
 # TRANSFORM DAY No INTO REAL DAY No FOR ONE YEAR
 ###############################################################################
transform_365into366 <- function(temp)
{
     ## add 1 day at the 29th of february
	# ppt is 0
	# tmin and tmax are the mean of 28/02 and 01/03

print(temp[57:61,]);

	# 01-01 +58 days is the 28-02
	# 01-01 +59 days is the 01-03
	new1 <- mean(temp[58:59,1]);
	new2 <- mean(temp[58:59,2]);
	new3 <- 0;
	
	# 01-01 +59 becomes the 29-02
	# 01-01 +60 becomes the 01-03
	newYear <- temp[1:58,1:dim(temp)[2]];
	newYear <- rbind(newYear,c(new1,new2,new3));
	newYear <- rbind(newYear,temp[59:dim(temp)[1],1:dim(temp)[2]]);

return(newYear);
}

##
 # TRANSFORM DAY No OVER A PERIOD
 ###############################################################################
transform_type1 <- function(table,head)
{
	for(y in format(head$period$start,"%Y"):format(head$period$end,"%Y")){
		if (is.leapYear(y)){
			# cut before
			dayNo_bef <- as.Date(paste("01","01",y,sep="-"),"%d-%m-%Y") - head$period$start;
			table_bef <- table[1:dayNo_bef,1:dim(table)[2]];
			# pull out section
			temp <- table[(dayNo_bef+1):(dayNo_bef+365),1:dim(table)[2]];
			# cut after
			table_aft <- table[(dayNo_bef+365+1):dim(table)[1],1:dim(table)[2]];
			# transform
			temp <- transform_365into366(temp);
			# paste
		}
	}
}

##
 # CONVERT 1 station for 1 time period
 ###############################################################################
convert_OneStation4OnePeriod <- function(data,stationName)
{
	path2file <- paste(data$path2input,data$folderName$tmin,stationName,sep="");
	fileHead <- read_bruceHeadFile(path2file);

	# make one table per station-period
	table <- as.array(as.numeric(scan(path2file,what="numeric",sep="\n",skip=3,nlines=-1,quiet=TRUE)));
	path2file <- paste(data$path2input,data$folderName$tmax,stationName,sep="");
	table <- cbind(table,as.array(as.numeric(scan(path2file,what="numeric",sep="\n",skip=3,nlines=-1,quiet=TRUE))));
	path2file <- paste(data$path2input,data$folderName$ppt,stationName,sep="");
	table <- cbind(table,as.array(as.numeric(scan(path2file,what="numeric",sep="\n",skip=3,nlines=-1,quiet=TRUE))));

	# change into real no of days
	switch(fileHead$period$type+1,
		{	# 0 is for real
		},{	# 1 is for 365
			table <- transform_type1(table,fileHead);
		},{	# 2 is for 360
			table <- transform_type2(table,fileHead);
		}
	);
	check_dayVSdim(fileHead$period$start,fileHead$period$end,dim(table)[1]);

return(table);
}

################################################################################
## REWRITE
################################################################################

## DATES
 ###############################################################################
yearOf_real <- function(table,name,sYear,sMonth,sDay,eYear,eMonth,eDay,dayPerYear)
{
headName <- strsplit(name,split="");
headName <- paste(headName[[1]][1],headName[[1]][2],headName[[1]][3],headName[[1]][4],headName[[1]][5],headName[[1]][6],sep="");
table[,1] <- headName;

line <-1;
year <- as.numeric(sYear);
if(sMonth=='01') day <- as.numeric(sDay)
else print("not taken into account yet");
while(line <= dim(table)[1]){
	table[line,2] <- year;
	table[line,3] <- day;
	line <- line+1;
	day <- day + 1;
	if (day>dayPerYear[dayPerYear[,1]==year,2]){
		year <- year+1;
		day <- 1;
	}  
}

return(table);
}

yearOf_365 <- function(table,name,sYear,sMonth,sDay,eYear,eMonth,eDay,dayPerYear)
{
headName <- strsplit(name,split="");
headName <- paste(headName[[1]][1],headName[[1]][2],headName[[1]][3],headName[[1]][4],headName[[1]][5],headName[[1]][6],sep="");
table[,1] <- headName;

# day difference
sLine <- row(dayPerYear(dayPerYear[,1]==as.numeric(sYear)));
eLine <- dayPerYear[,1]==as.numeric(eYear);
#missingDay <- sum(dayPerYear[dayPerYear[,1]==as.numeric(sYear):dayPerYear[,1]==as.numeric(eYear),2]);

line <-1;
year <- as.numeric(sYear);
if(sMonth=='01') day <- as.numeric(sDay)
else print("not taken into account yet");
while(line <= dim(table)[1]){
	table[line,2] <- year;
	table[line,3] <- day;
	line <- line+1;
	day <- day + 1;
	if (day>dayPerYear[dayPerYear[,1]==year,2]){
		year <- year+1;
		day <- 1;
	}  
}

return(table);
}

## WRITING
 ###############################################################################
addHeadFile <- function(path2data2write,model,fileName,gcm,period,station,lat,lon,alt,sYear,sMonth,sDay,eYear,eMonth,eDay)
{
write.table(
paste("!",fileName[station]," (",sYear,"-",sMonth,"-",sDay," to ",eYear,"-",eMonth,"-",eDay,")

[weather.met.weather]
Latitude=",lat,"
!Longitude=",lon,"
!Altitude=",alt,"

! TAV and AMP to be computed

site year day mint maxt rain radn
() () () (oC) (oC) (mm) (MJ/m^2)",sep=""),
file=paste(path2data2write,"/",model[gcm,period],"/",fileName[station],sep=""),append=FALSE,quote=FALSE,row.names=FALSE,col.names=FALSE,sep="");

}
