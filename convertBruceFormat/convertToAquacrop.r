##
 # FILE convertToAquaCrop.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/convertBruceFormat
 ###############################################################################
 #
 # functions required for AQUACROP format only
 #
 ###############################################################################

##
 # ETo ESTIMATION
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
 # > see for extraterrestrial radiation and ETo
 # @article{allen1998crop,
 # 	title={{Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56}},
 # 	author={Allen, R.G. and Pereira, L.S. and Raes, D. and Smith, M. and others},
 # 	journal={FAO, Rome},
 # 	volume={300},
 # 	year={1998}
 # }
 ###############################################################################
compute_ETo <- function(data,station,inland=NULL)
{	
################################################################################
	if (is.null(inland)){
		print("missing parameter: (inland=TRUE) for inland station, inland=FALSE for coastal station");
		stop();
	}

	for (line in 1:dim(data$year)){
		Gsc <- 0.0820;					# solar constant = 0.0820 MJ.m^(-2).min^(-1)
		phi <- pi*station$lat/180;			# latitude [rad] (N.B. South lat are negative)
		J <- data$julDay[line];				# julian day of the year

		delta <- 0.409*sin((2*pi*J/365)-1.39);		# solar decimation [rad]
		Dr <- 1+0.033*cos(2*pi*J/365);			# inverse relative distance Earth-Sun
		
		Ws <- acos(-tan(phi)*tan(delta)); 		# sunset hour angle [rad]
		Krs <- ifelse(inland,0.16,0.19);		# Krs in [0.1,1.2] for example 0.16 inland, 0.19 coastal
		
		# Extraterrestrial radiation for daily periods [MJ.m^(-2).day^(-1)]
		Ra <- (24*60/pi)*Gsc*Dr*(Ws*sin(phi)*sin(delta)+cos(phi)*cos(delta)*sin(Ws));
		# Estimate of the atmospheric transmissivity
		Tt <- Krs *(1+2.7*10^(-5)*station$alt)*sqrt(data$tmx[line]-data$tmn[line]);

		# Solar radiation at earth's surface [MJ.m^(-2).day^(-1)]
		Rs_ball <- Ra*Tt;					# see {ball2004evaluation}

		# Solar radiation that actually reaches the earth's surface
		n <-;	# actual duration of sunshine [hour]
		N <-;	# maximum possible duration of sunshine or daylight hours [hour]
		Rs_allen <- (0.25+0.50*n/N)*Ra;				# see {allen1998crop}

		# ????
		Tave <- (data$tmn+data$tmx)/2;
		lh <- 2.501-(0.002361*Tave); # Latent Heat of Vaporization [MJ.kg^(-1)] / The value of the latent heat varies only slightly over normal temperature ranges. A single value may be taken.
		# ????

		G <- ;	# soil heat flux density [MJ.m^(-2).day^(-1)]
		windSpeed <- ;	# wind speed at 2 m height [m.s^(-1)] 
		T <- ;	# air temperature at 2 m height [°C]
		Tdew <- ;	# dew point temperature [°C]

		# box7
		eTmin <- 0.6108*exp(17.27*data$tmn[line]/(data$tmn[line]+237.3));	# saturation vapour pressure at the air temperature tmn
		eTmax <- 0.6108*exp(17.27*data$tmx[line]/(data$tmx[line]+237.3));	# saturation vapour pressure at the air temperature tmx
		es <- (eTmax+eTmin)/2;							# saturation vapour pressure [kPa]
		ea <- 0.6108*exp(17.27*Tdew/(Tdew+237.3));				# actual vapour pressure [kPa]

		slopeVap <- 4098*0.6108*exp(17.27*T/(T+237.3))/((T+237.3)^2);		# slope vapour pressure curve at air temperature T [kPa.°C^(-1)]
		psychCon <- ;	# psychrometric constant [kPa.°C^(-1)]

		# box10
		albedo <- 0.23;			# a green vegetation cover has an albedo of about 0.20-0.25
		Rns <- (1-albedo)*Rs_?;	# net shortwave radiation [MJ.m^(-2).day^(-1)]
		Rnl <-;	# net longwave radiation [MJ.m^(-2).day^(-1)]
		Rn <- Rns -Rnl;					# net radiation at the crop surface [MJ.m^(-2).day^(-1)]

		# FAO Penman-Monteith equation for reference evapotranspiration [mm.day^(-1)]
		ETo <- (0.408*slopeVap*(Rn-G)+900*psychCon*windSpeed*(es-ea) / (T+273))/(slopeVap+psychCon*(1+0.34*windSpeed));		
	
		if(line==1){
			radn <- array(Rs,dim=1);
		}else{
			radn <- array(c(radn,Rs),dim=dim(radn)+1);
		}
	}
################################################################################
     Tave <- (tmin+tmax)/2
     lh <- 2.501-(0.002361*Tave)

     Rsolar <- (118.08/pi) * (1 + (0.033*cos(0.0172*jds))) * ((ws*sin(latr)*sin(delta)) + (cos(latr)*cos(delta)*sin(ws))) 
     ks <- 0.16 * (1 + (2.7e-5 * alt)) * sqrt(tmax - tmin) # approx atmos transmissivity - 1st coeff can be in range 0.1 - 0.2
     Rs <-  (1-0.23) * ks * Rsolar # assumes albedo 0.23

     fh <- 0.34 - (0.14 * sqrt(ea))
     fc <- (1.35*(ks * Rsolar/(0.75*Rsolar))) - 0.35
     Rl <- -1 * fc * fh * 4.903e-9 * ((tmax+273.15)^4 + (tmin+273.15)^4) / 2

     Rn <- (Rs + Rl) / lh
################################################################################
 
# in mm/day
	data <- list("tmn"=data$tmn,"tmx"=data$tmx,"ppt"=data$ppt,"year"=data$year,"julDay"=data$julDay,"radn"=radn);
return(data);
}

##
 # FORMAT AND WRITE DATA INTO OUTPUT .TMP AQUACROP FILE
 ###############################################################################
formatToTMPFile <- function(data,fileHead,path)
{
# I do not know yet if aquacrop is dealing with future date
# I hope so :)

# make one table from all the data
	apsim_table <- array(as.numeric(data$tmn),dim=dim(data$tmn));	# temp min titled	'Tmin'
	apsim_table <- cbind(apsim_table,as.numeric(data$tmx));		# temp max titled	'Tmax'

# write it into a .TMP file
	outName <- paste(path$output,strsplit(path$file$temp,"\\.")[[1]][1],".TMP",sep="");
	if(!file.exists(path$output)){	# create output dir if does not exists
		dir.create(path$output, showWarnings = FALSE, recursive = FALSE, mode = "0777");
	}else{				# remove output file if does exists
		if(file.exists(outName))	file.remove(outName);
	}
	# head
	file.copy(	"./aquaCropTemplates/aquaCropTemplate.TMP",	outName,overwrite=TRUE);
	changeVar(	"DESCRIPTION",	fileHead$station$id,		outName,outName);
	changeVar(	"FD",		format(fileHead$period$start,"%d"),	outName,outName);
	changeVar(	"FM",		format(fileHead$period$start,"%m"),	outName,outName);
	changeVar(	"FY",		format(fileHead$period$start,"%Y"),	outName,outName);
	# body
	apsim_table <- format(apsim_table,justify="right",width=6);
	write.table(apsim_table,outName,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE);
}

##
 # FORMAT AND WRITE DATA INTO OUTPUT .PLU AQUACROP FILE
 ###############################################################################
formatToPLUFile <- function(data,fileHead,path)
{
# I do not know yet if aquacrop is dealing with future date
# I hope so :)

# make one table from all the data
	apsim_table <- array(as.numeric(data$ppt),dim=dim(data$ppt));	# precip titled		'Rain'

# write it into a .TMP file
	outName <- paste(path$output,strsplit(path$file$temp,"\\.")[[1]][1],".PLU",sep="");
	if(!file.exists(path$output)){	# create output dir if does not exists
		dir.create(path$output, showWarnings = FALSE, recursive = FALSE, mode = "0777");
	}else{				# remove output file if does exists
		if(file.exists(outName))	file.remove(outName);
	}
	# head
	file.copy(	"./aquaCropTemplates/aquaCropTemplate.PLU",	outName,overwrite=TRUE);
	changeVar(	"DESCRIPTION",	fileHead$station$id,		outName,outName);
	changeVar(	"FD",		format(fileHead$period$start,"%d"),	outName,outName);
	changeVar(	"FM",		format(fileHead$period$start,"%m"),	outName,outName);
	changeVar(	"FY",		format(fileHead$period$start,"%Y"),	outName,outName);
	# body
	apsim_table <- format(apsim_table,justify="right",width=6);
	write.table(apsim_table,outName,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE);
}
