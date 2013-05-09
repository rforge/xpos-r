##
 # FILE climStat.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
 ###############################################################################

### MEANS
########################################################################

########################################################################
# compute the annual mean
# over years with no more than maxMV% (maxMV: maximum missing value percentage) missing data per year
# data format defined in readFormats.r
stat_annualMeans <- function(data,maxMV=10)
{
	aMeans <- NULL
	# I just compute it all, even if computing the date does not make any sense
	for (v in 1:length(data)){
		tmp <- array(NA,dim=(1+data$yyyy[length(data$yyyy)]-data$yyyy[1]))
		y<-data$yyyy[1]
		while(y<=data$yyyy[length(data$yyyy)]){
			fullY <- array(NA,dim=as.numeric(julian(as.Date(paste(y,"12","31",sep="-")),origin=as.Date(paste(y,"01","01",sep="-"))))+1)
			fullY[1:length(data[[v]][data$yyyy==y])] <- data[[v]][data$yyyy==y]
			mMV <- maxMV*length(fullY)/100
			## only if less than maxMV% missing data per set
			if(length(fullY[is.na(fullY)])<=mMV){
				c <- 1+y-data$yyyy[1]
				if(v<=5){ # date, yyyy, mm, dd and juld
					tmp[c] <- median(fullY,na.rm=T)	
				}else{	tmp[c] <- mean(fullY,na.rm=T)
				}
			}
		y<-y+1
		}
		aMeans <- c(aMeans,list(tmp))
		names(aMeans)[length(aMeans)] <- names(data)[v]
	}

return(aMeans)
# rm tmp objects
rm(tmp,v,y,maxMV,mMV,c,fullY)
}

########################################################################
# compute the monthly mean
# over months with no more than maxMV% (maxMV: maximum missing value percentage) missing data per month
# data format defined in readFormats.r
source('/home/crespo/Desktop/Optimisation/xpos-r/ClimateDataTools/Climatology/climTools.r')
stat_monthlyMeans <- function(data,maxMV=10)
{
	mMeans <- NULL
	# I just compute it all, even if computing the date does not make any sense
	for (v in 1:length(data)){
		# monthNo <- months of first year + 12*no of fullY + months of last year
		monthNo <- (12-data$mm[1]+1) + (12*(data$yyyy[length(data$yyyy)]-data$yyyy[1]+1-2)) + (data$mm[length(data$mm)])
		tmp <- array(NA,dim=monthNo)
		c<-1
		y<-data$yyyy[1]
		while(y<=data$yyyy[length(data$yyyy)]){
			m<-data$mm[data$yyyy==y][1]
			while(m<=data$mm[data$yyyy==y][length(data$mm[data$yyyy==y])]){
				fullM <- array(NA,dim=as.numeric(julian(as.Date(paste(y,m,maxNo_days(y,m),sep="-")),origin=as.Date(paste(y,m,"01",sep="-"))))+1)
				fullM[1:length(data[[v]][data$mm==m & data$yyyy==y])] <- data[[v]][data$mm==m & data$yyyy==y]
				mMV <- maxMV*length(fullM)/100
				## only if less than maxMV% missing data per set
				if(length(fullM[is.na(fullM)])<=mMV){
					if(v<=5){ # date, yyyy, mm, dd and juld
						tmp[c] <- median(fullM,na.rm=T)	
					}else{	tmp[c] <- mean(fullM,na.rm=T)
					}
				}
			c<-c+1
			m<-m+1
			}
		y<-y+1
		}
		mMeans <- c(mMeans,list(tmp))
		names(mMeans)[length(mMeans)] <- names(data)[v]
	}

return(mMeans)
# rm tmp objects
rm(tmp,v,y,m,maxMV,mMV,c,monthNo,fullM)
}

########################################################################
# compute the winWD time-window mean (winWidth: window width)
# over months with no more than maxMV% (maxMV: maximum missing value percentage) missing data per time-window
# data format defined in readFormats.r
stat_windowMeans <- function(data,maxMV=10,winWidth=31)
{
	wMeans <- NULL
	# winWidth-1 because I'm gonna look at one day -aHalfWidth and +pHalfWidth, hence a aHalfWidth+1+pHalfWidth wide window
	aHalfWidth <- floor((winWidth-1)/2)
	pHalfWidth <- (winWidth-1) - aHalfWidth

	# I just compute it all, even if computing the date does not make any sense
	for (v in 1:length(data)){
		tmp <- array(NA,dim=length(data$date))
		for(l in 1:length(data$date)){
			mMV <- maxMV*winWidth/100
			## only if less than maxMV% missing data per set
			windowA <- array(NA,dim=aHalfWidth+1)
			windowP <- array(NA,dim=pHalfWidth+1)
			ifelse(aHalfWidth>=l,windowA[(length(windowA)-length(data[[v]][1:l])+1):length(windowA)]<-data[[v]][1:l],windowA<-data[[v]][(l-aHalfWidth):l])
			ifelse(pHalfWidth>=(length(data$date)-l),windowP[1:(length(data$date)-l+1)]<-data[[v]][l:length(data$date)],windowP<-data[[v]][l:(l+pHalfWidth)])
			window <- c(windowA,windowP[2:length(windowP)])
			if(length(window[is.na(window)])<=mMV){
				if(v<=5){ # date, yyyy, mm, dd and juld
					tmp[l] <- window[floor(winWidth/2)]	
				}else{	tmp[l] <- mean(window,na.rm=T)
				}
			}
		}
		wMeans <- c(wMeans,list(tmp))
		names(wMeans)[length(wMeans)] <- names(data)[v]
	}

return(wMeans)
# rm tmp objects
rm(tmp,aHalfWidth,pHalfWidth,v,l,maxMV,mMV,windowA,windowP,window)
}

### TOTALS
########################################################################

########################################################################
# compute the annual totals
# over years with no more than maxMV% (maxMV: percentage upper limit) missing data per year
# data format defined in readFormats.r
stat_annualTotals <- function(data,maxMV=5)
{
	aTotals <- NULL
	# I just compute it all, even if computing the date does not make any sense
	for (v in 1:length(data)){
		tmp <- array(NA,dim=(1+data$yyyy[length(data$yyyy)]-data$yyyy[1]))
		y<-data$yyyy[1]
		while(y<=data$yyyy[length(data$yyyy)]){
			fullY <- array(NA,dim=as.numeric(julian(as.Date(paste(y,"12","31",sep="-")),origin=as.Date(paste(y,"01","01",sep="-"))))+1)
			fullY[1:length(data[[v]][data$yyyy==y])] <- data[[v]][data$yyyy==y]
			mMV <- maxMV*length(fullY)/100
			## only if less than maxMV% missing data per set
			if(length(fullY[is.na(fullY)])<=mMV){
				c <- 1+y-data$yyyy[1]
				if(v<=5){ # date, yyyy, mm, dd and juld
					tmp[c] <- median(fullY,na.rm=T)	
				}else{	tmp[c] <- sum(fullY,na.rm=T)
				}
			}
		y<-y+1
		}
		aTotals <- c(aTotals,list(tmp))
		names(aTotals)[length(aTotals)] <- names(data)[v]
	}

return(aTotals)
# rm tmp objects
rm(tmp,v,y,maxMV,mMV,c,fullY)
}

########################################################################
# compute the monthly totals
# over months with no more than maxMV% (maxMV: maximum missing value percentage) missing data per month
# data format defined in readFormats.r
source('/home/crespo/Desktop/Optimisation/xpos-r/ClimateDataTools/Climatology/climTools.r')
stat_monthlyTotals <- function(data,maxMV=5)
{
	mTotals <- NULL
	# I just compute it all, even if computing the date does not make any sense
	for (v in 1:length(data)){
		# monthNo <- months of first year + 12*no of fullY + months of last year
		monthNo <- (12-data$mm[1]+1) + (12*(data$yyyy[length(data$yyyy)]-data$yyyy[1]+1-2)) + (data$mm[length(data$mm)])
		tmp <- array(NA,dim=monthNo)
		c<-1
		y<-data$yyyy[1]
		while(y<=data$yyyy[length(data$yyyy)]){
			m<-data$mm[data$yyyy==y][1]
			while(m<=data$mm[data$yyyy==y][length(data$mm[data$yyyy==y])]){
				fullM <- array(NA,dim=as.numeric(julian(as.Date(paste(y,m,maxNo_days(y,m),sep="-")),origin=as.Date(paste(y,m,"01",sep="-"))))+1)
				fullM[1:length(data[[v]][data$mm==m & data$yyyy==y])] <- data[[v]][data$mm==m & data$yyyy==y]
				mMV <- maxMV*length(fullM)/100
				## only if less than maxMV% missing data per set
				if(length(fullM[is.na(fullM)])<=mMV){
					if(v<=5){ # date, yyyy, mm, dd and juld
						tmp[c] <- median(fullM,na.rm=T)	
					}else{	tmp[c] <- sum(fullM,na.rm=T)
					}
				}
			c<-c+1
			m<-m+1
			}
		y<-y+1
		}
		mTotals <- c(mTotals,list(tmp))
		names(mTotals)[length(mTotals)] <- names(data)[v]
	}

return(mTotals)
# rm tmp objects
rm(tmp,v,y,m,maxMV,mMV,c,monthNo,fullM)
}

########################################################################
# compute the winWD time-window totals (winWidth: window width)
# over months with no more than maxMV% (maxMV: maximum missing value percentage) missing data per time-window
# data format defined in readFormats.r
stat_windowTotals <- function(data,maxMV=5,winWidth=31)
{
	wTotals <- NULL
	# winWidth-1 because I'm gonna look at one day -aHalfWidth and +pHalfWidth, hence a aHalfWidth+1+pHalfWidth wide window
	aHalfWidth <- floor((winWidth-1)/2)
	pHalfWidth <- (winWidth-1) - aHalfWidth

	# I just compute it all, even if computing the date does not make any sense
	for (v in 1:length(data)){
		tmp <- array(NA,dim=length(data$date))
		for(l in 1:length(data$date)){
			mMV <- maxMV*winWidth/100
			## only if less than maxMV% missing data per set
			windowA <- array(NA,dim=aHalfWidth+1)
			windowP <- array(NA,dim=pHalfWidth+1)
			ifelse(aHalfWidth>=l,windowA[(length(windowA)-length(data[[v]][1:l])+1):length(windowA)]<-data[[v]][1:l],windowA<-data[[v]][(l-aHalfWidth):l])
			ifelse(pHalfWidth>=(length(data$date)-l),windowP[1:(length(data$date)-l+1)]<-data[[v]][l:length(data$date)],windowP<-data[[v]][l:(l+pHalfWidth)])
			window <- c(windowA,windowP[2:length(windowP)])
			if(length(window[is.na(window)])<=mMV){
				if(v<=5){ # date, yyyy, mm, dd and juld
					tmp[l] <- window[floor(winWidth/2)]	
				}else{	tmp[l] <- sum(window,na.rm=T)
				}
			}
		}
		wTotals <- c(wTotals,list(tmp))
		names(wTotals)[length(wTotals)] <- names(data)[v]
	}

return(wTotals)
# rm tmp objects
rm(tmp,aHalfWidth,pHalfWidth,v,l,maxMV,mMV,windowA,windowP,window)
}

### QUANTILES
########################################################################

########################################################################
# compute the annual quantiles (0,0.1,0.25,0.5,0.75,0.9,1)
# over years with no more than maxMV% (maxMV: percentage upper limit) missing data per year
# data format defined in readFormats.r
stat_annualQuantiles <- function(data,maxMV=5)
{
	aQuantiles <- NULL
	# I just compute it all, even if computing the date does not make any sense
	for (v in 1:length(data)){
		tmp <- array(NA,dim=c((1+data$yyyy[length(data$yyyy)]-data$yyyy[1]),7))
		colnames(tmp)<-c('0%','10%','25%','50%','75%','90%','100%')
		y<-data$yyyy[1]
		while(y<=data$yyyy[length(data$yyyy)]){
			fullY <- array(NA,dim=as.numeric(julian(as.Date(paste(y,"12","31",sep="-")),origin=as.Date(paste(y,"01","01",sep="-"))))+1)
			fullY[1:length(data[[v]][data$yyyy==y])] <- data[[v]][data$yyyy==y]
			mMV <- maxMV*length(fullY)/100
			## only if less than maxMV% missing data per set
			if(length(fullY[is.na(fullY)])<=mMV){
				c <- 1+y-data$yyyy[1]
				tmp[c,] <- array(quantile(fullY,probs=c(0,0.1,0.25,0.5,0.75,0.9,1),na.rm=T),dim=7)
			}
		y<-y+1
		}
		aQuantiles <- c(aQuantiles,list(tmp))
		names(aQuantiles)[length(aQuantiles)] <- names(data)[v]
	}

return(aQuantiles)
# rm tmp objects
rm(tmp,v,y,maxMV,mMV,c,fullY)
}

########################################################################
# compute the monthly quantiles (0,0.1,0.25,0.5,0.75,0.9,1)
# over months with no more than maxMV% (maxMV: maximum missing value percentage) missing data per month
# data format defined in readFormats.r
source('/home/crespo/Desktop/Optimisation/xpos-r/ClimateDataTools/Climatology/climTools.r')
stat_monthlyQuantiles <- function(data,maxMV=5)
{
	mQuantiles <- NULL
	# I just compute it all, even if computing the date does not make any sense
	for (v in 1:length(data)){
		# monthNo <- months of first year + 12*no of fullY + months of last year
		monthNo <- (12-data$mm[1]+1) + (12*(data$yyyy[length(data$yyyy)]-data$yyyy[1]+1-2)) + (data$mm[length(data$mm)])
		tmp <- array(NA,dim=c(monthNo,7))
		colnames(tmp)<-c('0%','10%','25%','50%','75%','90%','100%')
		c<-1
		y<-data$yyyy[1]
		while(y<=data$yyyy[length(data$yyyy)]){
			m<-data$mm[data$yyyy==y][1]
			while(m<=data$mm[data$yyyy==y][length(data$mm[data$yyyy==y])]){
				fullM <- array(NA,dim=as.numeric(julian(as.Date(paste(y,m,maxNo_days(y,m),sep="-")),origin=as.Date(paste(y,m,"01",sep="-"))))+1)
				fullM[1:length(data[[v]][data$mm==m & data$yyyy==y])] <- data[[v]][data$mm==m & data$yyyy==y]
				mMV <- maxMV*length(fullM)/100
				## only if less than maxMV% missing data per set
				if(length(fullM[is.na(fullM)])<=mMV){
					tmp[c,] <- array(quantile(fullM,probs=c(0,0.1,0.25,0.5,0.75,0.9,1),na.rm=T),dim=7)
				}
			c<-c+1
			m<-m+1
			}
		y<-y+1
		}
		mQuantiles <- c(mQuantiles,list(tmp))
		names(mQuantiles)[length(mQuantiles)] <- names(data)[v]
	}

return(mQuantiles)
# rm tmp objects
rm(tmp,v,y,m,maxMV,mMV,c,monthNo,fullM)
}

########################################################################
# compute the winWD time-window quantiles (0,0.1,0.25,0.5,0.75,0.9,1) (winWidth: window width)
# over months with no more than maxMV% (maxMV: maximum missing value percentage) missing data per time-window
# data format defined in readFormats.r
stat_windowQuantiles <- function(data,maxMV=5,winWidth=31)
{
	wQuantiles <- NULL
	# winWidth-1 because I'm gonna look at one day -aHalfWidth and +pHalfWidth, hence a aHalfWidth+1+pHalfWidth wide window
	aHalfWidth <- floor((winWidth-1)/2)
	pHalfWidth <- (winWidth-1) - aHalfWidth

	# I just compute it all, even if computing the date does not make any sense
	for (v in 1:length(data)){
		tmp <- array(NA,dim=c(length(data$date),7))
		colnames(tmp)<-c('0%','10%','25%','50%','75%','90%','100%')
		for(l in 1:length(data$date)){
			mMV <- maxMV*winWidth/100
			## only if less than maxMV% missing data per set
			windowA <- array(NA,dim=aHalfWidth+1)
			windowP <- array(NA,dim=pHalfWidth+1)
			ifelse(aHalfWidth>=l,windowA[(length(windowA)-length(data[[v]][1:l])+1):length(windowA)]<-data[[v]][1:l],windowA<-data[[v]][(l-aHalfWidth):l])
			ifelse(pHalfWidth>=(length(data$date)-l),windowP[1:(length(data$date)-l+1)]<-data[[v]][l:length(data$date)],windowP<-data[[v]][l:(l+pHalfWidth)])
			window <- c(windowA,windowP[2:length(windowP)])
			if(length(window[is.na(window)])<=mMV){
				tmp[l,] <- array(quantile(window,probs=c(0,0.1,0.25,0.5,0.75,0.9,1),na.rm=T),dim=7)
			}
		}
		wQuantiles <- c(wQuantiles,list(tmp))
		names(wQuantiles)[length(wQuantiles)] <- names(data)[v]
	}

return(wQuantiles)
# rm tmp objects
rm(tmp,aHalfWidth,pHalfWidth,v,l,maxMV,mMV,windowA,windowP,window)
}


