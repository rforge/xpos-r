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

