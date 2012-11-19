##
 # FILE plotFct.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/convertBruceFormat
 ###############################################################################
 #
 # useless for conversion, but helpfull to work it out
 #
 ###############################################################################


dif2 <- function(data)
{
rm(list = ls())     # clear objects  
graphics.off()      # close graphics windows   

	# means for all
	ET6_mean <- mean(data$ETo_PT6[data$julDay==1]);
	ET7_mean <- mean(data$ETo_PM1[data$julDay==1]);
	ET12_mean <- mean(data$ETo_ma[data$julDay==1]);
	ET6_min <- min(data$ETo_PT6[data$julDay==1]);
	ET7_min <- min(data$ETo_PM1[data$julDay==1]);
	ET12_min <- min(data$ETo_ma[data$julDay==1]);
	ET6_max <- max(data$ETo_PT6[data$julDay==1]);
	ET7_max <- max(data$ETo_PM1[data$julDay==1]);
	ET12_max <- max(data$ETo_ma[data$julDay==1]);
	for(day in 2:365){
		ET6_mean<- rbind(ET6_mean,mean(data$ETo_PT6[data$julDay==day]));
		ET7_mean<- rbind(ET7_mean,mean(data$ETo_PM1[data$julDay==day]));
		ET12_mean<- rbind(ET12_mean,mean(data$ETo_ma[data$julDay==day]));
		ET6_min<- rbind(ET6_min,min(data$ETo_PT6[data$julDay==day]));
		ET7_min<- rbind(ET7_min,min(data$ETo_PM1[data$julDay==day]));
		ET12_min<- rbind(ET12_min,min(data$ETo_ma[data$julDay==day]));
		ET6_max<- rbind(ET6_max,max(data$ETo_PT6[data$julDay==day]));
		ET7_max<- rbind(ET7_max,max(data$ETo_PM1[data$julDay==day]));
		ET12_max<- rbind(ET12_max,max(data$ETo_ma[data$julDay==day]));
	}

#	plot(ET6_mean-ET7_mean,type="l",col=1,ylim=c(-.5,3),xlab="julian Day",ylab="ETo (PT6-PM1)",main="SUSSUNDENGA");
#	lines(ET6_min-ET7_min,type="l",col=2);
#	lines(ET6_max-ET7_max,type="l",col=3);


	plot(ET6_mean-ET12_mean,type="l",col=1,ylim=c(-.02,.04),xlab="julian Day",ylab="ETo (PT6-mark)",main="SUSSUNDENGA");
	lines(ET6_min-ET12_min,type="l",col=2);
	lines(ET6_max-ET12_max,type="l",col=3);

}

bp <- function(data)
{
rm(list = ls())     # clear objects  
graphics.off()      # close graphics windows   

	# boxplot for one
#	d<-array(data$ETo_PM1[data$julDay==1],dim=length(data$ETo_PM1[data$julDay==1]));
#	for(day in 2:365){
#		d<-cbind(d,data$ETo_PM1[data$julDay==day]);
#	}

	d<-array(data$ETo_PT6[data$julDay==1],dim=length(data$ETo_PT6[data$julDay==1]));
	for(day in 2:365){
		d<-cbind(d,data$ETo_PT6[data$julDay==day]);
	}

	boxplot(d,col="yellow",xlab="julian Day",ylab="ETo",main="SUSSUNDENGA - (PT6)");

}

pdm <- function(data)
{
rm(list = ls())     # clear objects  
graphics.off()      # close graphics windows   

	# means for all
#	ET1 <- mean(data$ETo_PT1[data$julDay==1]);
#	ET2 <- mean(data$ETo_PT2[data$julDay==1]);
#	ET3 <- mean(data$ETo_PT3[data$julDay==1]);
#	ET4 <- mean(data$ETo_PT4[data$julDay==1]);
#	ET5 <- mean(data$ETo_PT5[data$julDay==1]);
	ET61 <- mean(data$ETo_PT61[data$julDay==1]);
	ET62 <- mean(data$ETo_PT62[data$julDay==1]);
	ET63 <- mean(data$ETo_PT63[data$julDay==1]);
	ET64 <- mean(data$ETo_PT64[data$julDay==1]);
#	ET7 <- mean(data$ETo_PM1[data$julDay==1]);
#	ET8 <- mean(data$ETo_PM2[data$julDay==1]);
#	ET9 <- mean(data$ETo_PM3[data$julDay==1]);
#	ET10 <- mean(data$ETo_PM4[data$julDay==1]);
#	ET11 <- mean(data$ETo_HS[data$julDay==1]);
#	ET12 <- mean(data$ETo_ma[data$julDay==1]);
	for(day in 2:365){
#		ET1<- rbind(ET1,mean(data$ETo_PT1[data$julDay==day]));
#		ET2<- rbind(ET2,mean(data$ETo_PT2[data$julDay==day]));
#		ET3<- rbind(ET3,mean(data$ETo_PT3[data$julDay==day]));
#		ET4<- rbind(ET4,mean(data$ETo_PT4[data$julDay==day]));
#		ET5<- rbind(ET5,mean(data$ETo_PT5[data$julDay==day]));
		ET61<- rbind(ET61,mean(data$ETo_PT61[data$julDay==day]));
		ET62<- rbind(ET62,mean(data$ETo_PT62[data$julDay==day]));
		ET63<- rbind(ET63,mean(data$ETo_PT63[data$julDay==day]));
		ET64<- rbind(ET64,mean(data$ETo_PT64[data$julDay==day]));
#		ET7<- rbind(ET7,mean(data$ETo_PM1[data$julDay==day]));
#		ET8<- rbind(ET8,mean(data$ETo_PM2[data$julDay==day]));
#		ET9<- rbind(ET9,mean(data$ETo_PM3[data$julDay==day]));
#		ET10<- rbind(ET10,mean(data$ETo_PM4[data$julDay==day]));
#		ET11<- rbind(ET11,mean(data$ETo_HS[data$julDay==day]));
#		ET12<- rbind(ET12,mean(data$ETo_ma[data$julDay==day]));
	}
#	plot(ET1,type="l",col=1,ylim=c(2,7),xlab="julian Day",ylab="mean ETo",main="SUSSUNDENGA");
#	lines(ET2,type="l",col=2);
#	lines(ET3,type="l",col=3);
#	lines(ET4,type="l",col=4);
#	lines(ET5,type="l",col=5);
	plot(ET61,type="l",col=1,ylim=c(2,10),xlab="julian Day",ylab="mean ETo_PT6",main="SUSSUNDENGA");
	lines(ET62,type="l",col=2);
	lines(ET63,type="l",col=3);
	lines(ET64,type="l",col=4);
#	lines(ET7,type="l",col=7,lty=2);
#	lines(ET8,type="l",col=8,lty=2);
#	lines(ET9,type="l",col=9,lty=2);
#	lines(ET10,type="l",col=10,lty=2);
#	lines(ET11,type="l",col=11,lty=3);
#	lines(ET12,type="l",col=12,lty=3);

	legend("topleft",	legend=c(	"1.26 (overall mean)","1.30 (approx. mean on land)","1.08 (extremely humid)","1.70 (extremely arid)"),
				fill=c(		1,2,3,4)
	);
}

alphaVar <- function(data)
{

rm(list = ls())     # clear objects  
graphics.off()      # close graphics windows 

	a <- 0;			# ranges from 0 to 0.1 (humid to arid)
	c <- 0.45;		# c=0.475 here ranges from 0.45 to 0.50 from arid to humid

	PTc <- 1.26;		# Priestley-Taylor coefficient

	for (line in 1:dim(data$year)){
		eTmin <- 0.6108*exp(17.27*data$tmn[line]/(data$tmn[line]+237.3));	# min saturation vapour pressure [kPa]
		eTmax <- 0.6108*exp(17.27*data$tmx[line]/(data$tmx[line]+237.3));	# max saturation vapour pressure [kPa]
		VPDmax <- (eTmax-eTmin)/(1-a*(eTmax-eTmin));
		VPD <- c*VPDmax+0;

		alpha <- 1+(PTc-1)*1*VPD;
		if(line==1){
			var <- array(alpha,dim=1);
		}else{
			var <- array(c(var,alpha),dim=dim(var)+1);

		}
	}	
	
	var_boxP<-array(var[data$julDay==1],dim=length(var[data$julDay==1]));
	var_qu <- quantile(var[data$julDay==1],  probs = c(0, 10, 33, 50, 66, 90, 100)/100);
	for(day in 2:365){
		var_boxP<-cbind(var_boxP,var[data$julDay==day]);
		var_qu <- cbind(var_qu,quantile(var[data$julDay==day],  probs = c(0, 10, 33, 50, 66, 90, 100)/100));
	}

#	boxplot(var_boxP,boxwex=0.01);

	var_mean <- mean(var[data$julDay==1]);
	for(day in 2:365){
		var_mean<- rbind(var_mean,mean(var[data$julDay==day]));
	}

	plot (var_mean,type="l",col=1, ylim=c(1.1,2.4), main="SUSSUNDENGA - alpha variation (a and c set for humid conditions, PTc = 1.26)", xlab="Day",ylab="0-10-33-50-66-90-100 percentiles and mean (black)");
	lines(var_qu[1,],type="l",col=3);
	lines(var_qu[2,],type="l",col=4);
	lines(var_qu[3,],type="l",col=5);
	lines(var_qu[4,],type="l",col=6);
	lines(var_qu[5,],type="l",col=5);
	lines(var_qu[6,],type="l",col=4);
	lines(var_qu[7,],type="l",col=3);

	
}
