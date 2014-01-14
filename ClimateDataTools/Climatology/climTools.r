##
 # FILE climTools.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
 ###############################################################################

### MAKE VIRTUAL always 365 days long years
########################################################################
# tmin and tmax: average 28-29-FEB (when exist)
# rain: sum 28-29-FEB (when exist)
########################################################################
make_virtual365 <- function(data)
{
	virt_data <- data

	i<-j<-1
	while(i<=length(data$yyyy)){
		if(data$mm[i]==2 && data$dd[i]==29){
			j<-j-1
			virt_data$tmin[j]<-	mean(data$tmin[i],data$tmin[i-1],na.rm=TRUE)
			virt_data$tmax[j]<-	mean(data$tmax[i],data$tmax[i-1],na.rm=TRUE)
			virt_data$rain[j]<-	sum(data$rain[i],data$rain[i-1],na.rm=TRUE)
		}else{
			virt_data$date[j]<-	data$date[i]
			virt_data$yyyy[j]<-	data$yyyy[i]
			virt_data$mm[j]<-	data$mm[i]
			virt_data$dd[j]<-	data$dd[i]
			virt_data$tmin[j]<-	data$tmin[i]
			virt_data$tmax[j]<-	data$tmax[i]
			virt_data$rain[j]<-	data$rain[i]
		}
		i<-i+1
		j<-j+1
	}
	virt_data$juld <- rep(1:365,length.out=length(virt_data$juld))

	#rm somehow NA at the end
	for(i in 1:length(virt_data)){
		virt_data[[i]]<-virt_data[[i]][1:(j-1)]			
	}

return(virt_data)
rm(data,virt_data,i,j)
}

##
 # is that year a leap year?
 ###############################################################################
is.leapYear <- function(year)
{
	## trusting R date class
	## otherwise there would be a conflict sooner or later anyway
	start <- as.Date(paste("01","01",year,sep="-"),"%d-%m-%Y");
	end <- as.Date(paste("31","12",year,sep="-"),"%d-%m-%Y");

	dayNo <- end-start +1;
	switch(dayNo-364,
		leap <- FALSE,
		leap <- TRUE
	);
return(leap);
}

##
 # how many days in a specific month-year
 ###############################################################################
maxNo_days <- function(year,month)
{
	switch(month,
		{	# 1: JAN
		dayNo <- 31
		},{	# 2: FEB
		dayNo <- ifelse(is.leapYear(year),29,28)
		},{	# 3: MAR
		dayNo <- 31
		},{	# 4: APR
		dayNo <- 30
		},{	# 5: MAY
		dayNo <- 31
		},{	# 6: JUN
		dayNo <- 30
		},{	# 7: JUL
		dayNo <- 31
		},{	# 8: AUG
		dayNo <- 31
		},{	# 9: SEP
		dayNo <- 30
		},{	# 10: OCT
		dayNo <- 31
		},{	# 11: NOV
		dayNo <- 30
		},{	# 12: DEC
		dayNo <- 31
		}
	)
return(dayNo)
}

