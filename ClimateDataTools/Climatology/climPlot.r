##
 # FILE climPlot.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
 ###############################################################################

################################################################################
# TOOLS
################################################################################

## EPS
copyDev2eps <- function(title="title",file)
{
	dev.print(device=postscript,title=title,paper="special",horizontal=FALSE,file=file);
}

## JPG
copyDev2jpg <- function(file)
{
	savePlot(filename=file,type="jpeg",device=dev.cur());
}

## FIG
## HAS TO BE SMALLER THAN AN A4 FORMAT
copyDev2fig <- function(file)
{
	dev.print(device=xfig,file=file);
}


##############################################################################
#### FUNCTIONS
##############################################################################

plot_temp <- function(bas1,per1,bas2=NULL,per2=NULL)
{
	graphics.off()
	plot(seq(min(bas1,bas2,na.rm=T),max(bas1,bas2,na.rm=T),length.out=100),seq(min(per1,per2,na.rm=T),max(per1,per2,na.rm=T),length.out=100),type="n",xlab="base",ylab="perturbed",main="tmin and tmax")

	points(bas1,per1,pch="+",col="blue")
	if(!is.null(bas2))	points(bas2,per2,pch="+",col="red")
	abline(0,1)
	
	# stat
	minM <- lm(per1~bas1)
	abline(minM,lw=2,lt=2)
	if(!is.null(bas2)){
		maxM <- lm(per2~bas2)
		abline(maxM,lw=2,lt=2)
	}
}

plot_rain <- function(bas,per)
{
	graphics.off()
	plot(seq(min(bas,na.rm=T),max(bas,na.rm=T),length.out=100),seq(min(per,na.rm=T),max(per,na.rm=T),length.out=100),type="n",xlab="base",ylab="perturbed",main="rainfall")

	points(bas,per,pch="+",col="blue")
	abline(0,1)

	# stat
	rainM <- lm(per~bas)
	abline(rainM,lw=2,lt=2)
}

# var : 1:date 2:year 3:month 4:day 5:julianDay 6:tmin 7:tmax 8:rain
compute_radar <- function(data,var=6)
{
	graphics.off()

	data_virt <- make_virtual365(data)
	data_swm<-stat_windowMeans(data_virt,maxMV=10,winWidth=31)

	tmp_vec <- array(NA,dim=(ceiling(length(data_virt[[1]])/365)*365))
	for(i in 1:(length(data_virt[[1]]))){
#		tmp_vec[i] <- data_virt[[var]][i] 
		tmp_vec[i] <- data_swm[[var]][i] 
	}

return(tmp_vec)
}

plot_radar <- function(radar)
{
	graphics.off()
	full_mat<-matrix(radar,ncol=365,byrow=TRUE)
	period_ave<-mean(full_mat,na.rm=TRUE)
	daily_ave<-daily_1_20<-daily_31_50<-daily_61_80<-array(NA,dim=365)
	for(i in 1:365){
		daily_ave[i]<-mean(full_mat[,i],na.rm=TRUE)
		daily_1_20[i]<-mean(full_mat[1:20,i],na.rm=TRUE)
		daily_31_50[i]<-mean(full_mat[31:50,i],na.rm=TRUE)
		daily_61_80[i]<-mean(full_mat[61:80,i],na.rm=TRUE)
	}
	
#	stars(matrix(radar,ncol=365,byrow=TRUE),locations=c(0,0),scale=FALSE,radius=FALSE,frame.plot=TRUE,col.lines=heat.colors(length(radar)/365))
	stars(rbind(period_ave,daily_1_20,daily_31_50,daily_61_80),locations=c(0,0),scale=FALSE,radius=FALSE,frame.plot=TRUE,col.lines=c("black","green","orange","red"))
	abline(h=0)
	abline(v=0)

}


