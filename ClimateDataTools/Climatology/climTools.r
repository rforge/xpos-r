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

