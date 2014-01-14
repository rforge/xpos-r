##
 # FILE climFuture.r
 # AUTHOR olivier olivier
 # https://r-forge.r-project.org/projects/xpos-r/
 ###############################################################################
# EVENTUALLY I WANT TO MEASURE SOME CHANGE INDICATORS

###############################################################################
###############################################################################
source('/home/olivier/Desktop/Optimisation/xpos-r/ClimateDataTools/Climatology/climStat.r')
# compute the mean/total monthly change
future_mtChange <- function(met1,met2)
{
	# compute stats
	s_mm_met1 <- stat_monthlyMeans(met1$data)
	s_mm_met2 <- stat_monthlyMeans(met2$data)
	s_mt_met1 <- stat_monthlyTotals(met1$data)
	s_mt_met2 <- stat_monthlyTotals(met2$data)
	#
	# It appears in some cases that a simple shift ends up creating tmin > tmax
	# So I go for a linearly proportinal shift (related to the tmax-tmin difference)
	# Same for rain
	# N.B. mathematically it would not prevent tmin > tmax, but in practice, it should
	#
	# addedd ms_diff to return

	# monthly stats
	ms_met1 <- list("tmin"=array(NA,dim=12),"tmax"=array(NA,dim=12),"rain"=array(NA,dim=12))
	ms_met2 <- list("tmin"=array(NA,dim=12),"tmax"=array(NA,dim=12),"rain"=array(NA,dim=12))
	ms_diffT <- array(NA,dim=12)
	for (m in 1:12){
		ms_met1$tmin[m] <- mean(s_mm_met1$tmin[s_mm_met1$mm==m],na.rm=T)
		ms_met2$tmin[m] <- mean(s_mm_met2$tmin[s_mm_met2$mm==m],na.rm=T)
		ms_met1$tmax[m] <- mean(s_mm_met1$tmax[s_mm_met1$mm==m],na.rm=T)
		ms_met2$tmax[m] <- mean(s_mm_met2$tmax[s_mm_met2$mm==m],na.rm=T)
		ms_met1$rain[m] <- mean(s_mt_met1$rain[s_mt_met1$mm==m],na.rm=T)
		ms_met2$rain[m] <- mean(s_mt_met2$rain[s_mt_met2$mm==m],na.rm=T)
		ms_diffT[m] <- ms_met1$tmax[m]-ms_met1$tmin[m]
	}

	# change (tmep shift and rain percentage)
	change <- matrix(	c(	(ms_met2$tmin-ms_met1$tmin),
					(ms_met2$tmax-ms_met1$tmax),
					(ms_met2$rain/ms_met1$rain),
					ms_diffT
				),
				ncol=12,byrow=T
			)

	colnames(change) <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
	rownames(change) <- c('tmin','tmax','rain','tDif')

return(change)
rm(s_mm_met1,s_mm_met2,s_mt_met1,s_mt_met2,ms_met1,ms_met2,change)
}

###############################################################################
###############################################################################
source('/home/olivier/Desktop/Optimisation/xpos-r/ClimateDataTools/Climatology/climStat.r')
# compute the quantiles monthly change
future_qChange <- function(met1,met2)
{
	# do I need to normalise as above?

	# compute stats
	s_mq_met1 <- stat_monthlyQuantiles(met1$data)
	s_mq_met2 <- stat_monthlyQuantiles(met2$data)

	# monthly stats
	l_qu <- dim(s_mq_met1[[1]])[2]
	m_qu <- median(1:l_qu)
	qs_met1 <- list("tmin"=array(NA,dim=c(12,l_qu)),"tmax"=array(NA,dim=c(12,l_qu)),"rain"=array(NA,dim=c(12,l_qu)))
	qs_met2 <- list("tmin"=array(NA,dim=c(12,l_qu)),"tmax"=array(NA,dim=c(12,l_qu)),"rain"=array(NA,dim=c(12,l_qu)))
	for (m in 1:12){
		qs_met1$tmin[m,] <- apply(s_mq_met1$tmin[s_mq_met1$mm[,m_qu]==m,],2,mean) # ,na.rm=T ??
		qs_met2$tmin[m,] <- apply(s_mq_met2$tmin[s_mq_met2$mm[,m_qu]==m,],2,mean)
		qs_met1$tmax[m,] <- apply(s_mq_met1$tmax[s_mq_met1$mm[,m_qu]==m,],2,mean)
		qs_met2$tmax[m,] <- apply(s_mq_met2$tmax[s_mq_met2$mm[,m_qu]==m,],2,mean)
		qs_met1$rain[m,] <- apply(s_mq_met1$rain[s_mq_met1$mm[,m_qu]==m,],2,mean)
		qs_met2$rain[m,] <- apply(s_mq_met2$rain[s_mq_met2$mm[,m_qu]==m,],2,mean)
	}

	# change
	change <- array(NA,dim=c(3,12,l_qu))
	# shift
	change[1,,] <- qs_met2$tmin-qs_met1$tmin
	change[2,,] <- qs_met2$tmax-qs_met1$tmax
	# percentage
	print("rewrite, see climFuture.r"); browser();
	change[3,,] <- qs_met2$rain-qs_met1$rain
	colnames(change) <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
	rownames(change) <- c('tmin','tmax','rain')

return(change)
rm(s_mq_met1,s_mq_met2,l_qu,m_qu,qs_met1,qs_met2,change)
}
























