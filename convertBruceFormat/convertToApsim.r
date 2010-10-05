##
 # FILE convertToApsim.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/convertBruceFormat
 ###############################################################################
 #
 # functions required for APSIM format only
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
 # SOLAR RADIATION ESTIMATION
 ###############################################################################
 # > see for solar radiation
 # @article{ball2004evaluation,
 # 	title={{Evaluation of solar radiation prediction models in North America}},
 # 	author={Ball, R.A. and Purcell, L.C. and Carey, S.K.},
 # 	journal={Agronomy Journal},
 # 	volume={96},
 # 	number={2},
 # 	pages={391},
 # 	year={2004},
 # 	publisher={Am Soc Agronom}
 # }
 # > see for extraterrestrial radiation
 # @article{allen1998crop,
 # 	title={{Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56}},
 # 	author={Allen, R.G. and Pereira, L.S. and Raes, D. and Smith, M. and others},
 # 	journal={FAO, Rome},
 # 	volume={300},
 # 	year={1998}
 # }
 ###############################################################################
compute_radn <- function(data,station,inland=NULL)
{	
	if (is.null(inland)){
		print("missing parameter: (inland=TRUE) for inland station, inland=FALSE for coastal station");
		stop();
	}

	for (line in 1:dim(data$year)){
		Gsc <- 0.0820;					# solar constant = 0.0820 MJ.m^(-2).min^(-1)
		phi <- pi*station$lat/180;			# latitude [rad] (N.B. South lat shouls be negative)
		J <- data$julDay[line];				# julian day of the year

		delta <- 0.409*sin((2*pi*J/365)-1.39);		# solar decimation [rad]
		Dr <- 1+0.033*cos(2*pi*J/365);			# inverse relative distance Earth-Sun
		
		Ws <- acos(-tan(phi)*tan(delta)); 		# sunset hour angle [rad]
		
		# Extraterrestrial radiation for daily periods [MJ.m^(-2).day^(-1)]
		Ra <- (24*60/pi)*Gsc*Dr*(Ws*sin(phi)*sin(delta)+cos(phi)*cos(delta)*sin(Ws));
		
		Krs <- ifelse(inland,0.16,0.19);		# Krs in [0.1,1.2] for example 0.16 inland, 0.19 coastal
		# estimate of the atmospheric transmissivity
		Tt <- Krs *(1+2.7*10^(-5)*station$alt)*sqrt(data$tmx[line]-data$tmn[line]);
		
		if(line==1){
			radn <- array((Ra*Tt),dim=1);
		}else{
			radn <- array(c(radn,(Ra*Tt)),dim=dim(radn)+1);
		}
	}

	data <- list("tmn"=data$tmn,"tmx"=data$tmx,"ppt"=data$ppt,"year"=data$year,"julDay"=data$julDay,"radn"=radn);
return(data);
}

##
 # COMPUTE tav AND amp APSIM CONSTANTS
 # annual average ambient temperature (TAV)
 # annual amplitude in mean monthly temperature (AMP)
 ###############################################################################
 # > ref
 # http://www.apsim.info/apsim/Products/tav_amp.pdf
 # results confirmed in face of the tav_amp.exe dos application
 ###############################################################################
compute_tavNamp <- function(data)
{
	# daily mean
	dMean <- (data$tmn+data$tmx)/2;

	firstYear <- as.numeric(data$year[1]);
	yearlyAMP <- array(NA,dim=(as.numeric(data$year[dim(data$year)])-as.numeric(data$year[1])+1));
	monthlyMean <- array(0,dim=c(12,5));
	year <- firstYear;
	for (line in 1:dim(data$year)){
		# yearly AMP
		if (data$year[line]!=year){
			yearlyAMP[year-firstYear+1] <- max(monthlyMean[,3]/monthlyMean[,4])-min(monthlyMean[,3]/monthlyMean[,4]);
			year <- as.numeric(data$year[line]);
			monthlyMean[,3] <- 0;
			monthlyMean[,4] <- 0;
		}
		
		# monthly mean
		month <- as.numeric(format(as.Date(data$julDay[line]-1,origin=paste(data$year[line],"-01-01",sep="")),"%m"));
		monthlyMean[month,1] <- monthlyMean[month,1]+dMean[line];
		monthlyMean[month,2] <- monthlyMean[month,2]+1;
		monthlyMean[month,3] <- monthlyMean[month,3]+dMean[line];
		monthlyMean[month,4] <- monthlyMean[month,4]+1;
	}
	
	# complete
	yearlyAMP[year-firstYear+1] <- max(monthlyMean[,3]/monthlyMean[,4])-min(monthlyMean[,3]/monthlyMean[,4]);
	monthlyMean[,5] <- monthlyMean[,1]/monthlyMean[,2];

	amp <- format(mean(yearlyAMP),digits=4);
	tav <- format(mean(monthlyMean[,5]),digits=4);

	data<-list("tmn"=data$tmn,"tmx"=data$tmx,"ppt"=data$ppt,"year"=data$year,"julDay"=data$julDay,"radn"=data$radn,"amp"=amp,"tav"=tav);
return(data);
}

##
 # FORMAT AND WRITE DATA INTO OUTPUT .met APSIM FILE
 ###############################################################################
formatToMetFile <- function(data,fileHead,path)
{
# waiting for a nicer solution within apsim ( at least necessary until apsim 7.0 )
# make 'year' a fake, and 'realY' the actual year
# still one issue: 2100 is not a leap year, and any fake within the last century (allowed by APSIM so far) will be leap
# solution so far: do not simulate 2100 with apsim !!
	fake_year <- array(as.numeric(data$year[1])-ifelse(format(fileHead$period$end,"%Y")>=2065,100,0),dim=dim(data$year));

# make one table from all the data
	apsim_table <- as.numeric(fake_year);				# fake year titled	'year'
	apsim_table <- cbind(apsim_table,as.integer(data$year));	# real year titled	'realY'
	apsim_table <- cbind(apsim_table,as.integer(data$julDay));	# julian day titled	'day'
	apsim_table <- cbind(apsim_table,as.numeric(data$tmn));		# temp min titled	'mint'
	apsim_table <- cbind(apsim_table,as.numeric(data$tmx));		# temp max titled	'maxt'
	apsim_table <- cbind(apsim_table,as.numeric(data$ppt));		# precipitation titled	'rain'
	apsim_table <- cbind(apsim_table,as.numeric(data$radn));	# radiation titled	'mint'
	apsim_table[,4:7] <- format(apsim_table[,4:7],digits=2);

# write it into a .met file
	outName <- paste(path$output,strsplit(path$file$temp,"\\.")[[1]][1],".met",sep="");
	if(!file.exists(path$output)){	# create output dir if does not exists
		dir.create(path$output, showWarnings = FALSE, recursive = FALSE, mode = "0777");
	}else{				# remove output file if does exists
		if(file.exists(outName))	file.remove(outName);
	}
	# head
	file.copy(	"./apsimTemplates/apsimTemplate.met",	outName,overwrite=TRUE);
	changeVar(	"station_id",	fileHead$station$id,	outName,outName);
	changeVar(	"station_comm",	fileHead$comment,	outName,outName);
	changeVar(	"stat_lat",	fileHead$station$lat,	outName,outName);
	changeVar(	"stat_lon",	fileHead$station$lon,	outName,outName);
	changeVar(	"stat_alt",	fileHead$station$alt,	outName,outName);
	changeVar(	"inLand",	ifelse(path$inland,"0.16 (in land)","0.19 (coastal)"),	outName,outName);
	changeVar(	"period_tav",	data$tav,		outName,outName);
	changeVar(	"period_amp",	data$amp,		outName,outName);
	# body
	apsim_table <- format(apsim_table,justify="right",width=6);
	write.table(apsim_table,outName,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE);
}

