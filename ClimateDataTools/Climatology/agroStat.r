##
 # FILE agroStat.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/convertBruceFormat
 ###############################################################################

##
 # COMPUTE tav AND amp APSIM CONSTANTS
 # annual average ambient temperature (TAV)
 # annual amplitude in mean monthly temperature (AMP)
 ###############################################################################
 # > ref
 # http://www.apsim.info/Wiki/public/Upload/OtherProducts/tav_amp.pdf
 # results confirmed in face of the tav_amp.exe dos application
 ###############################################################################
agro_tavNamp <- function(metD)
{
### AMP is limited to complete year only since for uncomplete ones yearlyAMP will have NA values, then mean is processed with na.rm=T
###
browser()
	# daily mean
	dMean <- (metD$data$tmin+metD$data$tmax)/2
	yearlyAMP <- array(NA,dim=(metD$data$yyyy[length(metD$data$yyyy)]-metD$data$yyyy[1]+1))
	monthlyMean <- array(0,dim=c(12,5))
	year <- metD$data$yyyy[1]
	for (line in 1:length(metD$data$yyyy)){
		# yearly AMP
		if (metD$data$yyyy[line]!=year){
			yearlyAMP[year-firstYear+1] <- max(monthlyMean[,3]/monthlyMean[,4])-min(monthlyMean[,3]/monthlyMean[,4])
			year <- metD$data$yyyy[line]
			monthlyMean[,3] <- 0
			monthlyMean[,4] <- 0
		}
		# monthly mean
		month <- metD$data$mm[line]
		if (!is.na(dMean[line])){ # avoid NAs
			monthlyMean[month,1] <- monthlyMean[month,1]+dMean[line]
			monthlyMean[month,2] <- monthlyMean[month,2]+1
			monthlyMean[month,3] <- monthlyMean[month,3]+dMean[line]
			monthlyMean[month,4] <- monthlyMean[month,4]+1
		}
	}
	
	# complete
	yearlyAMP[year-firstYear+1] <- max(monthlyMean[,3]/monthlyMean[,4])-min(monthlyMean[,3]/monthlyMean[,4])
	monthlyMean[,5] <- monthlyMean[,1]/monthlyMean[,2]

	amp <- ifelse(all(is.na(yearlyAMP)),NA,format(mean(yearlyAMP,na.rm=T),digits=3))
	tav <- ifelse(all(is.na(monthlyMean[,5])),NA,format(mean(monthlyMean[,5]),digits=3))

	if(amp!="NA" && (as.numeric(amp)<0 || as.numeric(amp)>50))	{
		print("# WARNING: amp is not in the APSIM range allowed (0>amp>50)",quote=FALSE)
		browser()
	}
	if(tav!="NA" && (as.numeric(tav)<0 || as.numeric(tav)>50))	{
		print("# WARNING: tav is not in the APSIM range allowed (0>tav>50)",quote=FALSE)
		browser()
	}

return(metD)
rm(dMean,yearlyAMP,monthlyMean,year,line,month,amp,tav)
}

