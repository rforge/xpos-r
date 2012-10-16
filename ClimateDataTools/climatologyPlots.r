source('plotFct.r')

path<-"../../../../Link\ to\ WinShared/12_AgMIP/2012-07_Sentinel/InLandIsT/RZA/obs/0261516.1.met"
title<-"Bloemfontein - South Africa (1962-2010)"
fil<-"../../../../Link\ to\ WinShared/12_AgMIP/2012-10_globalWorkshop/Climatology/RZAbloemfontein"

sta<-load_obs(path)


temp_quantiles(sta,title)
copyDev2eps(file=paste(fil,"temp.eps",sep="_"))
copyDev2jpg(file=paste(fil,"temp.jpg",sep="_"))

source('plotFct.r')

prec_runMonth(metDat=sta,figTit=title,rDayFac=5)
copyDev2eps(file=paste(fil,"prec.eps",sep="_"))
copyDev2jpg(file=paste(fil,"prec.jpg",sep="_"))

