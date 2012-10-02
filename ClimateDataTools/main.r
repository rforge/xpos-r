source('plotFct.r')

path<-"../../../12_AgMIP/2012-07_Sentinel/InLandIsT/ZIM_pat/obs/Bulawayo_51_2002.met"
title<-"Bulawayo - Zimbabwe (1951-2002)"
fil<-"../../../12_AgMIP/2012-10_globalWorkshop/Climatology/ZIMbulawayo"

sta<-load_obs(path)


temp_quantiles(sta,title)
copyDev2eps(file=paste(fil,"temp.eps",sep="_"))
copyDev2jpg(file=paste(fil,"temp.jpg",sep="_"))

source('plotFct.r')

prec_runMonth(metDat=sta,figTit=title,rDayFac=10)
copyDev2eps(file=paste(fil,"prec.eps",sep="_"))
copyDev2jpg(file=paste(fil,"prec.jpg",sep="_"))

