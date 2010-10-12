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
 # > see for most of the calculations
 # @article{richard_g._allen_crop_1998,
 #	title = {Crop evapotranspiration - Guidelines for computing crop water requirements},
 #	volume = {56},
 #	url = {http://www.fao.org/docrep/x0490e/x0490e00.htm#Contents},
 #	journal = {{FAO} Irrigation and drainage papers},
 #	author = {Richard G. Allen and Luis S. Pereira and Dirk Raes and Martin Smith},
 #	year = {1998}
 # },
 # > see for solar radiation
 # @article{rosalind_a._ball_evaluation_2004,
 # 	title = {Evaluation of Solar Radiation Prediction Models in North America},
 #	volume = {96},
 #	url = {https://www.agronomy.org/publications/aj/abstracts/96/2/391},
 #	doi = {10.2134/agronj2004.0391},
 #	number = {2},
 #	journal = {Agronomy Journal},
 #	author = {Rosalind A. Ball and Larry C. Purcell and Sean K. Carey},
 #	year = {2004},
 #	pages = {391--397}
 # },
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
			aTra <- array(Tt,dim=1);
		}else{
			sRad <- array(c(sRad,Rs),dim=dim(sRad)+1);
			eRad <- array(c(eRad,Ra),dim=dim(eRad)+1);
			aTra <- array(c(aTra,Tt),dim=dim(aTra)+1);
		}
	}

	data <- list("tmn"=data$tmn,"tmx"=data$tmx,"ppt"=data$ppt,"year"=data$year,"julDay"=data$julDay,"sRad"=sRad,"eRad"=eRad,"aTra"=aTra);
return(data);
}

##
 # ETo ESTIMATION
 ###############################################################################
 # > see for most of the calculations
 # @article{richard_g._allen_crop_1998,
 # 	title = {Crop evapotranspiration - Guidelines for computing crop water requirements},
 #	volume = {56},
 #	url = {http://www.fao.org/docrep/x0490e/x0490e00.htm#Contents},
 #	journal = {{FAO} Irrigation and drainage papers},
 #	author = {Richard G. Allen and Luis S. Pereira and Dirk Raes and Martin Smith},
 #	year = {1998}
 # },
 # @techreport{delobel_review_2009,
 #	type = {{FAO} technical report},
 #	title = {Review of {ETo} calculation methods and software},
 #	author = {François Delobel},
 #	year = {2009}
 # },
 # @article{c._h._b._priestley_assessment_1972,
 # 	title = {On the Assessment of Surface Heat Flux and Evaporation Using {Large-Scale} Parameters},
 #	volume = {100},
 #	number = {2},
 #	journal = {Monthly Weather Review},
 #	author = {C. H. B. {PRIESTLEY} and R. J. {TAYLOR}},
 #	year = {1972},
 #	pages = {81--92}
 # }
 # @article{j._l._steiner_lysimetric_1991,
 #	title = {Lysimetric Evaluation of Daily Potential Evapotranspiration Models for Grain Sorghum},
 #	volume = {83},
 #	doi = {agronj1991.00021962008300010055x},
 #	number = {1},
 #	journal = {Agronomy Journal},
 #	author = {J. L. Steiner and T. A. Howell and A. D. Schneider},
 #	year = {1991},
 #	pages = {240--247}
 # }
 # @article{f._castellv_methods_1997,
 #	title = {Methods for estimating vapor pressure deficit at a regional scale depending on data availability},
 #	volume = {87},
 #	doi = {10.1016/S0168-1923(97)00034-8},
 #	number = {4},
 #	journal = {Agricultural and Forest Meteorology},
 #	author = {F. Castellví and P. J. Perez and C. O. Stockle and M. Ibañez},
 #	year = {1997},
 #	pages = {243--252}
 # }
 # good biblio in here: http://www.apesimulator.it/help/models/evapotranspiration/
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
		# calculation of psychometric constant
		Tmean <- (data$tmx[line]+data$tmn[line])/2;
		Po <- 101.3; alto <- 0; g <- 9.807; R <- 287; a1 <- 0.0065;
		Tko <- 273.15+Tmean;
		P <- Po*((Tko-a1*(station$alt-alto))/Tko)^(g/(a1*R));		# atmospheric pressure
		Cp <- 0.001013;							# specific heat at constant pressure, 1.013.10^(-3) [MJ.kg^(-1).°C^(-1)]
		epsilon <- 0.622;						# ratio molecular weight of water vapour/dry air = 0.622
		lambda <- 2.501-(0.002361*Tmean);				# latent heat of vaporization, 2.45 [MJ.kg^(-1)]
		psychCon <- Cp*P/(epsilon*lambda);				# psychrometric constant [kPa.°C^(-1)]
		# N.B:	according to allen1998crop, and based on lambda = 2.45 MJ.kg^(-1) at 20°C
		#	psychCon should vary from 0.067 at altitude 0m to 0.060 at altitude 1000m
		#	looks like mine is lower ...

		# calculation of vapour pressures
		Tdew <- (data$tmn[line]*0.52) + (0.6*data$tmx[line]) - (0.009*(data$tmx[line]^2)) - 2;	# see {delobel_review_2009}
		eTmin <- 0.6108*exp(17.27*data$tmn[line]/(data$tmn[line]+237.3));	# min saturation vapour pressure [kPa]
		eTmax <- 0.6108*exp(17.27*data$tmx[line]/(data$tmx[line]+237.3));	# max saturation vapour pressure [kPa]
		eTmean <- es <- (eTmax+eTmin)/2;					# mean saturation vapour pressure [kPa] - Using mean air temperature instead of daily minimum and maximum temperatures results in lower estimates for the mean saturation vapour pressure. The corresponding vapour pressure deficit (a parameter expressing the evaporating power of the atmosphere) will also be smaller and the result will be some underestimation of the reference crop evapotranspiration. Therefore, the mean saturation vapour pressure should be calculated as the mean between the saturation vapour pressure at both the daily maximum and minimum air temperature.
		ea <- 0.6108*exp(17.27*Tdew/(Tdew+237.3));				# actual vapour pressure [kPa]

		# calculation of Slope of saturation vapour pressure curve
#		slopeVap <- 4098*eTmean/((Tmean+237.3)^2);							# slope vapour pressure curve at air temperature T [kPa.°C^(-1)] - In the FAO Penman-Monteith equation, where ∆ occurs in the numerator and denominator, the slope of the vapour pressure curve is calculated using mean air temperature - according to {richard_g._allen_crop_1998}
		slopeVap <- 2049*((eTmin/((data$tmn[line]+237.3)^2))+(eTmax/((data$tmx[line]+237.3)^2)));	# to be favoured according to {delobel_review_2009}

		# calculation of Rn
		albedo <- 0.23;					# a green vegetation cover has an albedo of about 0.20-0.25
		Rns <- (1-albedo)*data$sRad[line];		# net shortwave radiation [MJ.m^(-2).day^(-1)]
		SteBolCon <- 4.903*10^(-9);			# Stefan-Boltzmann constant [4.903.10^(-9) MJ.K^(-4).m^(-2).day^(-1)]
		Rso <- (0.75+2*10^(-5)*station$alt)*data$eRad[line];		# clear-sky solar radiation [MJ.m^(-2).day^(-1)]
		Rnl_1 <- SteBolCon*((data$tmx[line]+273.15)^4+(data$tmn[line]+273.15)^4)/2;
		Rnl_2 <- 0.34 - 0.14*sqrt(ea);
		Rnl_3 <- (1.35*ifelse((data$sRad[line]/Rso)>1,1,data$sRad[line]/Rso))-0.35;
		Rnl <- Rnl_1 * Rnl_2 * Rnl_3;			# net longwave radiation [MJ.m^(-2).day^(-1)]
		Rn <- Rns -Rnl;					# net radiation at the crop surface [MJ.m^(-2).day^(-1)]

		# Priestley-Taylor coefficient see ...
		# According to its original formulation (Priestley and Taylor, 1972), alpha is a constant term (alpha=PTc, where PTc is the dimensionless Priestley-Taylor constant). An average value of PTc=1.26 was found by the authors and theoretically explained by Lhomme (1996) for "the evapotranspiration from a horizontally uniform saturated surface", that closely resembles a surface of well-watered short grasses under humid conditions. The literature shows that PTc can vary from 1.08 to more than 1.60 as a function of the advectivity of the environment (Villalobos et al., 2002). The constant should be increased for arid and semi-arid climates up to PTc=1.70-1.75, according to ASCE (1990). Lower values are expected for wetlands.
		# PTC, a and VPD coefficients (at least) VARY ACCORDING TO HUMID OR ARID
		Tc <- 2.24+0.49*(data$tmx[line]+data$tmn[line]);# see {f._castellv_methods_1997}
		eTc <- 0.6108*exp(17.27*Tc/(Tc+237.3));
		PTc <- 1.26;					# alpha overall average see {c._h._b._priestley_assessment_1972}
		a <- 0.04;					# ranges from 0 to 0.1 (humid to arid)
		VPDmax_1 <- eTmax - ea;				# see {f._castellv_methods_1997}
		VPDmax_2 <- (eTmax-eTmin)/(1-a*(eTmax-eTmin));	# see {f._castellv_methods_1997}
		VPD_1 <- es-ea;
		VPD_2 <- eTc - eTmin;				# see {f._castellv_methods_1997}
		VPD_31 <- 0.475*VPDmax_1+0;			# c=0.475 here ranges from 0.45 to 0.50 from arid to humid
		VPD_32 <- 0.475*VPDmax_2+0;			# c=0.475 here ranges from 0.45 to 0.50 from arid to humid
		alpha_1 <- PTc;					# see {c._h._b._priestley_assessment_1972}
		alpha_2 <- (1+psychCon/slopeVap)/(1+0.6);	# see {c._h._b._priestley_assessment_1972}
		alpha_31 <- 1+(PTc-1)*1*VPD_1;			# see {j._l._steiner_lysimetric_1991}
		alpha_32 <- 1+(PTc-1)*1*VPD_2;
		alpha_33 <- 1+(PTc-1)*1*VPD_31;
		alpha_34 <- 1+(PTc-1)*1*VPD_32;

## =>		# Priestley-Taylor Potential Evapotranspiration
		G <- 0;		# soil heat flux density [MJ.m^(-2).day^(-1)] - As the magnitude of the day is relatively small, it may be ignored
		PT_1 <- alpha_1/lambda*slopeVap*(Rn-G)/(slopeVap+psychCon);
		PT_2 <- alpha_2/lambda*slopeVap*(Rn-G)/(slopeVap+psychCon);
		PT_3 <- alpha_31/lambda*slopeVap*(Rn-G)/(slopeVap+psychCon);
		PT_4 <- alpha_32/lambda*slopeVap*(Rn-G)/(slopeVap+psychCon);
		PT_5 <- alpha_33/lambda*slopeVap*(Rn-G)/(slopeVap+psychCon);
		PT_6 <- alpha_34/lambda*slopeVap*(Rn-G)/(slopeVap+psychCon);
	
## => 		# mark modified
# read that VPD = es -ea and VPD max = eTmin - eTmax
		a <- 0.04  # should vary between 0 (humid) and 0.08 (very arid)
		VPDmax <- (eTmax - eTmin) / (1 - (a * (eTmax - eTmin)))
		VPD <- 0.475 * VPDmax
		pet <- (1+((1.30-1)*VPD))/lambda*(Rn-G)*slopeVap/(slopeVap+psychCon)

## =>		# FAO Penman-Monteith equation for reference evapotranspiration [mm.day^(-1)]
		# significant sensitivity to arid/humid condition and vegetation height through windspeed
		# this relation has been produced for : sub-humid, low to moderate wind speed, short vegetation
		windSpeed <- 2;	# wind speed at 2 m height [m.s^(-1)]  - 2 m/s is used as a temporary estimate - Due to the appearance of windSpeed in both the nominator and denominator of the FAO Penman-Monteith equation, ETo is not highly sensitive to normal ranges of wind speed - N.B. taller is the ground vegetation considered, greater is the sensitivity
		PM_1 <- (slopeVap*(Rn-G)/lambda+(900*psychCon*windSpeed*VPD_1/(Tmean+273)))/(slopeVap+psychCon*(1+0.34*windSpeed));		
		PM_2 <- (slopeVap*(Rn-G)/lambda+(900*psychCon*windSpeed*VPD_2/(Tmean+273)))/(slopeVap+psychCon*(1+0.34*windSpeed));		
		PM_3 <- (slopeVap*(Rn-G)/lambda+(900*psychCon*windSpeed*VPD_31/(Tmean+273)))/(slopeVap+psychCon*(1+0.34*windSpeed));		
		PM_4 <- (slopeVap*(Rn-G)/lambda+(900*psychCon*windSpeed*VPD_32/(Tmean+273)))/(slopeVap+psychCon*(1+0.34*windSpeed));		

## =>		# Hargreaves and Samani
		a <- 0; b <- 1;		# unadjusted version
		HS <- a+b*0.0023/lambda*(((data$tmx[line]+data$tmn[line])/2)+17.8)*sqrt(data$tmx[line]-data$tmn[line])*data$eRad[line];

## =>		mark
		mark <- compute_mark(data$tmn[line],data$tmx[line],station$lat,station$alt,data$julDay[line]);

		if(line==1){
			ETo_PT1 <- array(PT_1,dim=1);
			ETo_PT2 <- array(PT_2,dim=1);
			ETo_PT3 <- array(PT_3,dim=1);
			ETo_PT4 <- array(PT_4,dim=1);
			ETo_PT5 <- array(PT_5,dim=1);
			ETo_PT6 <- array(PT_6,dim=1);
			ETo_PM1 <- array(PM_1,dim=1);
			ETo_PM2 <- array(PM_2,dim=1);
			ETo_PM3 <- array(PM_3,dim=1);
			ETo_PM4 <- array(PM_4,dim=1);
			ETo_HS <- array(HS,dim=1);
			ETo_mm <- array(pet,dim=1);
			ETo_ma <- array(mark,dim=1);
			extra_1 <- array(VPDmax,dim=1);
			extra_2 <- array(eTmax-eTmin,dim=1);
		}else{
			ETo_PT1 <- array(c(ETo_PT1,PT_1),dim=dim(ETo_PT1)+1);
			ETo_PT2 <- array(c(ETo_PT2,PT_2),dim=dim(ETo_PT2)+1);
			ETo_PT3 <- array(c(ETo_PT3,PT_3),dim=dim(ETo_PT3)+1);
			ETo_PT4 <- array(c(ETo_PT4,PT_4),dim=dim(ETo_PT4)+1);
			ETo_PT5 <- array(c(ETo_PT5,PT_5),dim=dim(ETo_PT5)+1);
			ETo_PT6 <- array(c(ETo_PT6,PT_6),dim=dim(ETo_PT6)+1);
			ETo_PM1 <- array(c(ETo_PM1,PM_1),dim=dim(ETo_PM1)+1);
			ETo_PM2 <- array(c(ETo_PM2,PM_2),dim=dim(ETo_PM2)+1);
			ETo_PM3 <- array(c(ETo_PM3,PM_3),dim=dim(ETo_PM3)+1);
			ETo_PM4 <- array(c(ETo_PM4,PM_4),dim=dim(ETo_PM4)+1);
			ETo_HS <- array(c(ETo_HS,HS),dim=dim(ETo_HS)+1);
			ETo_mm <- array(c(ETo_mm,pet),dim=dim(ETo_mm)+1);
			ETo_ma <- array(c(ETo_ma,mark),dim=dim(ETo_ma)+1);
			extra_1 <- array(c(extra_1,VPDmax),dim=dim(extra_1)+1);
			extra_2 <- array(c(extra_2,eTmax-eTmin),dim=dim(extra_2)+1);
		}
	}

	data <- list(	"tmn"=data$tmn,"tmx"=data$tmx,"ppt"=data$ppt,"julDay"=data$julDay,"year"=data$year,
			"ETo_PT1"=ETo_PT1,"ETo_PT2"=ETo_PT2,"ETo_PT3"=ETo_PT3,"ETo_PT4"=ETo_PT4,"ETo_PT5"=ETo_PT5,"ETo_PT6"=ETo_PT6,
			"ETo_PM1"=ETo_PM1,"ETo_PM2"=ETo_PM2,"ETo_PM3"=ETo_PM3,"ETo_PM4"=ETo_PM4,
			"ETo_HS"=ETo_HS,
			"ETo_mm"=ETo_mm,"ETo_ma"=ETo_ma,"ex_1"=extra_1,"ex_2"=extra_2);
return(data);
}

## for check only
compute_mark <- function(tmin,tmax,lat,alt,jds)
{
     latr <- lat*pi/180
     Tave <- (tmin+tmax)/2
     Tdew <- (tmin*0.52) + (0.6*tmax) - (0.009*(tmax^2)) - 2
     ea <- 0.6108*exp((17.27*Tdew)/(Tdew+237.3))
     lh <- 2.501-(0.002361*Tave)

     P <- ifelse(alt > -90, 101.3*(((293-(0.0065*alt))/293)^5.26), 96)  # approximation without valid alt

     delta <- 0.4093 * sin((2*pi*jds/365)-1.39)
     ws <- acos(-tan(latr)*tan(delta))
     Rsolar <- (118.08/pi) * (1 + (0.033*cos(0.0172*jds))) * ((ws*sin(latr)*sin(delta)) + (cos(latr)*cos(delta)*sin(ws))) 

     alt <- ifelse(alt > -90, alt, 0)
     ks <- 0.16 * (1 + (2.7e-5 * alt)) * sqrt(tmax - tmin) # approx atmos transmissivity - 1st coeff can be in range 0.1 - 0.2

     Rs <-  (1-0.23) * ks * Rsolar # assumes albedo 0.23

     fh <- 0.34 - (0.14 * sqrt(ea))
     fc <- (1.35*(ks * Rsolar/(0.75*Rsolar))) - 0.35
     Rl <- -1 * fc * fh * 4.903e-9 * ((tmax+273.15)^4 + (tmin+273.15)^4) / 2

     Rn <- (Rs + Rl) / lh

     gamma <- (0.001013 * P)/(0.622*lh)
     eTmax <- 0.6108*exp((17.27*tmax)/(tmax+237.3))
     eTmin <- 0.6108*exp((17.27*tmin)/(tmin+237.3))
     es <- (eTmax + eTmin)/2
     s <- 4098*es/((Tave+237.3)^2)

# Definition of VPD ???
#     VPD <- es - ea

# VPD calculated via a critical temperature (from Italian web site)
#     Tc <- 2.24 + (0.49 * (tmax + tmin))
#     eTc <- 0.6108*exp((17.27*Tc)/(Tc+237.3))
#     VPD <- eTc - eTmin

# Recommended VPD from agrometshell
     a <- 0.04  # should vary between 0 (humid) and 0.08 (very arid)
     VPDmax <- (eTmax - eTmin) / (1 - (a * (eTmax - eTmin)))
     VPD <- 0.475 * VPDmax

     PTc <- 1.30 # should be 1.26-1.30

     pet <- (1+((PTc-1)*VPD))*s*Rn/(s+gamma)
     fPTpet <- pet
return(pet)
}
