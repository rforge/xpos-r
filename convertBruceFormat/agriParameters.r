##
 # FILE agriParameters.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/convertBruceFormat
 ###############################################################################
 #
 # functions required for agricultural parameters
 #
 ###############################################################################

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
		
		# Solar radiation at earth's surface [MJ.m^(-2).day^(-1)]
		Rs <- Ra*Tt;

		if(line==1){
			sRad <- array(Rs,dim=1);
			eRad <- array(Ra,dim=1);
		}else{
			sRad <- array(c(sRad,Rs),dim=dim(sRad)+1);
			eRad <- array(c(eRad,Ra),dim=dim(eRad)+1);
		}
	}

	data <- list("tmn"=data$tmn,"tmx"=data$tmx,"ppt"=data$ppt,"year"=data$year,"julDay"=data$julDay,"sRad"=sRad,"eRad"=eRad);
return(data);
}

##
 # ETo ESTIMATION
 ###############################################################################
 # > see for extraterrestrial radiation
 # @article{allen1998crop,
 # 	title={{Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56}},
 # 	author={Allen, R.G. and Pereira, L.S. and Raes, D. and Smith, M. and others},
 # 	journal={FAO, Rome},
 # 	volume={300},
 # 	year={1998}
 # }
 # > see for PE
 # @article{priestley1972assessment,
 # title={{On the assessment of surface heat flux and evaporation using large-scale parameters}},
 # author={Priestley, CHB and Taylor, RJ},
 # journal={Monthly weather review},
 # volume={100},
 # number={2},
 # pages={81--92},
 # year={1972}
 # }
 ###############################################################################
compute_ETo <- function(data,station,inland=NULL)
{	
	if (is.null(inland)){
		print("missing parameter: (inland=TRUE) for inland station, inland=FALSE for coastal station");
		stop();
	}

	# compute Rs
	data <- compute_radn(data,station,inland);

	for (line in 1:dim(data$year)){
		# calculation of Slope of saturation vapour pressure curve
		Tmean <- (data$tmx[line]+data$tmn[line])/2;
		slopeVap <- 4098*0.6108*exp(17.27*Tmean/(Tmean+237.3))/((Tmean+237.3)^2);	# slope vapour pressure curve at air temperature T [kPa.°C^(-1)] - In the FAO Penman-Monteith equation, where ∆ occurs in the numerator and denominator, the slope of the vapour pressure curve is calculated using mean air temperature

		# calculation of psychometric constant
		Po <- 101.3; alto <- 0; g <- 9.807; R <- 287; a1 <- 0.0065;
		Tko <- 273.16+Tmean;
		P <- Po*((Tko-a1*(station$alt-alto))/Tko)^(g/(a1*R));		# atmospheric pressure
		Cp <- 0.001013;							# specific heat at constant pressure, 1.013.10^(-3) [MJ.kg^(-1).°C^(-1)]
		epsilon <- 0.622;						# ratio molecular weight of water vapour/dry air = 0.622
		lambda <- 2.501-(0.002361*Tmean);				# latent heat of vaporization, 2.45 [MJ.kg^(-1)]
		psychCon <- Cp*P/(epsilon*lambda);				# psychrometric constant [kPa.°C^(-1)]
		# N.B:	according to allen1998crop, and based on lambda = 2.45 MJ.kg^(-1) at 20°C
		#	psychCon should vary from 0.067 at altitude 0m to 0.060 at altitude 1000m
		#	looks like mine is lower ...

		# calculation of vapour pressures
# mark - where does this come from?
		Tdew <- (data$tmn[line]*0.52) + (0.6*data$tmx[line]) - (0.009*(data$tmx[line]^2)) - 2;
# ???		Tdew <- data$tmn[line];							# dew point temperature [°C] - assuming that Tdew is near Tmin
# ???		Tdew <- data$tmn[line] - 2 or -3;					# arid regions
		eTmin <- 0.6108*exp(17.27*data$tmn[line]/(data$tmn[line]+237.3));	# saturation vapour pressure at the air temperature tmn
		eTmax <- 0.6108*exp(17.27*data$tmx[line]/(data$tmx[line]+237.3));	# saturation vapour pressure at the air temperature tmx
		es <- (eTmax+eTmin)/2;							# saturation vapour pressure [kPa]
		ea <- 0.6108*exp(17.27*Tdew/(Tdew+237.3));				# actual vapour pressure [kPa]

		# calculation of Rn
		albedo <- 0.23;					# a green vegetation cover has an albedo of about 0.20-0.25
		Rns <- (1-albedo)*data$sRad[line];		# net shortwave radiation [MJ.m^(-2).day^(-1)]
		SteBolCon <- 4.903*10^(-9);			# Stefan-Boltzmann constant [4.903.10^(-9) MJ.K^(-4).m^(-2).day^(-1)]
		Rso <- (0.75+2*10^(-5)*station$alt)*data$eRad[line];		# clear-sky solar radiation [MJ.m^(-2).day^(-1)]
		Rnl <- ((SteBolCon*(data$tmx[line]+273.16)^4+SteBolCon*(data$tmn[line]+273.16)^4)/2)*(0.34-0.14*sqrt(ea))*((1.35*data$sRad[line]/Rso)-0.35);	# net longwave radiation [MJ.m^(-2).day^(-1)]
		Rn <- Rns -Rnl;					# net radiation at the crop surface [MJ.m^(-2).day^(-1)]

## =>		# Priestley-Taylor Potential Evapotranspiration
		G <- 0;		# soil heat flux density [MJ.m^(-2).day^(-1)] - As the magnitude of the day is relatively small, it may be ignored
		alpha<- 1.30;	# it seems that the best estimate of alpha is the overall mean (land and water) of 1.26 (over land though alpha seems rather greater than lower ~1.30)
		PE <- alpha*(Rn-G)*slopeVap/(slopeVap+psychCon);
	
## =>		# FAO Penman-Monteith equation for reference evapotranspiration [mm.day^(-1)]
		windSpeed <- 2;	# wind speed at 2 m height [m.s^(-1)]  - 2 m/s is used as a temporary estimate - Due to the appearance of windSpeed in both the nominator and denominator of the FAO Penman-Monteith equation, ETo is not highly sensitive to normal ranges of wind speed - N.B. taller is the ground vegetation considered, greater is the sensitivity
		ET <- ((0.408*slopeVap*(Rn-G))+(900*psychCon*windSpeed*(es-ea)/(Tmean+273)))/(slopeVap+psychCon*(1+0.34*windSpeed));		

## => 		# mark uses VPD ...data$tmx[line]
		gamma <- psychCon;
		s <- 4098*es/((Tmean+237.3)^2)		# is my slopeVap except es instead of ea ???
		a <- 0.04  # should vary between 0 (humid) and 0.08 (very arid)
		VPDmax <- (eTmax - eTmin) / (1 - (a * (eTmax - eTmin)))
		VPD <- 0.475 * VPDmax
		PTc <- 1.30 # should be 1.26-1.30
		pet <- (1+((PTc-1)*VPD))*s*Rn/(s+gamma) # second part looks a lot like PT

		if(line==1){
			dpT <- array(Tdew,dim=1);
			ETo_PT <- array(PE,dim=1);
			ETo_PM <- array(ET,dim=1);
			ETo_ma <- array(pet,dim=1);
		}else{
			dpT <- array(c(dpT,Tdew),dim=dim(dpT)+1);
			ETo_PT <- array(c(ETo_PT,PE),dim=dim(ETo_PT)+1);
			ETo_PM <- array(c(ETo_PM,ET),dim=dim(ETo_PM)+1);
			ETo_ma <- array(c(ETo_ma,pet),dim=dim(ETo_ma)+1);
		}
	}

	data <- list("tmn"=data$tmn,"tmx"=data$tmx,"ppt"=data$ppt,"ETo_PT"=ETo_PT,"ETo_PM"=ETo_PM,"ETo_ma"=ETo_ma,"dpT"=dpT);
return(data);
}

