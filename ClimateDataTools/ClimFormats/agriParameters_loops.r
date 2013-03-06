##
 # FILE agriParameters_loops.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/convertBruceFormat
 ###############################################################################
 #
 # years in one hand, day in the other
 # make the link
 #
 ###############################################################################

##
 # from agriParameters.r
 # around compute_radn_1day
 ###############################################################################
compute_radn <- function(data,station,inland=NULL)
{	
	if (is.null(inland)){
		print("missing parameter: (inland=TRUE) for inland station, inland=FALSE for coastal station",quote=FALSE);
		stop();
	}

	for (line in 1:dim(data$year)){

		radn<-compute_radn_1day(data$julDay[line],data$tmx[line],data$tmn[line],station$lat,station$alt,inland);
		
		if(line==1){
			sRad <- array(radn$solarRad[1],dim=1);
			eRad <- array(radn$extraRad[1],dim=1);
			aTra <- array(radn$atmosTra[1],dim=1);
		}else{
			sRad <- array(c(sRad,radn$solarRad[1]),dim=dim(sRad)+1);
			eRad <- array(c(eRad,radn$extraRad[1]),dim=dim(eRad)+1);
			aTra <- array(c(aTra,radn$atmosTra[1]),dim=dim(aTra)+1);
		}
	}

	data <- list("tmn"=data$tmn,"tmx"=data$tmx,"ppt"=data$ppt,"year"=data$year,"julDay"=data$julDay,"sRad"=sRad,"eRad"=eRad,"aTra"=aTra);
return(data);
}

##
 # from agriParameters.r
 # around compute_ETo_1Day
 ###############################################################################
compute_ETo <- function(data,fileHead,inland=NULL,arid=NULL)
{	station <- fileHead$station;

	# compute Rs, Ra, Tt
	data <- compute_radn(data,station,inland);

	# compute everything else
	for (line in 1:dim(data$year)){
		ET <- compute_ETo_1Day(data$tmx[line],data$tmn[line],station$alt,data$sRad[line],data$eRad[line],arid);

		if(line==1){
			RHs <- array(ET$RelHum[1],dim=1);
			ETo <- array(ET$ETo[1],dim=1);
		}else{
			RHs <- array(c(RHs,ET$RelHum[1]),dim=dim(RHs)+1);
			ETo <- array(c(ETo,ET$ETo[1]),dim=dim(ETo)+1);
		}
	}
	
## a and c aridity parameters routine
	meanRH <- sum(RHs,na.rm=TRUE)/(length(RHs)-length(RHs[is.na(RHs)]));
#	posteriori_a <- 4.5 * exp(-0.1 * meanRH);
	posteriori_c <- 0.724 - 0.004 * meanRH;
	doItAgain <- 0;
	if(arid>1 && arid <5){
		if (posteriori_c <= ET$cArray[arid-1])	doItAgain <- -1;
		if (posteriori_c >= ET$cArray[arid+1])	doItAgain <- +1;
	}

## Aridity Index (AI)
	# according to the definition provided by the United Nations Convention to Combat Desertification (UNCCD)
	# AI = ratio of mean annual precipitation to mean annual potential evapotranspiration
	meanAnnRain <- sum(data$ppt)/(as.numeric(format(fileHead$period$end,"%Y"))-as.numeric(format(fileHead$period$start,"%Y")));
	meanAnnETo <- sum(ETo)/(as.numeric(format(fileHead$period$end,"%Y"))-as.numeric(format(fileHead$period$start,"%Y")));
	AI <- meanAnnRain/meanAnnETo;
	if(is.na(AI)){
		AI <- "NA";
	}else{
		if (0<=AI && AI<0.05)	AI <- "Hyper-Arid";
		if (0.05<=AI && AI<0.2)	AI <- "Arid";
		if (0.2<=AI && AI<0.5)	AI <- "Semi-Arid";
		if (0.5<=AI && AI<0.65)	AI <- "Dry Sub-Humid";
		if (0.65<=AI)		AI <- "Humid";
	}
	
	data <- list(	"tmn"=data$tmn,
			"tmx"=data$tmx,
			"ppt"=data$ppt,
			"julDay"=data$julDay,
			"year"=data$year,
			"sRad"=data$sRad,
			"ETo"=ETo,
			"AI"=AI, "doItAgain"=doItAgain, "arid"=arid
		);

return(data);
}

