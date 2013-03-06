##
 # FILE matTools.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/ClimateDataTools/AgMIP
 #####################################################################
 # Here we assume we have a data map stored as a matrix
 # where left to right columns are west to east longitudes
 # and top to bottom rows are north to south latitudes
 # I'LL CALL THIS MATRIX THE top-north left-west matrix
 #####################################################################

## read *.mat matlab formatted matrices
####################################################################
# read and save and clear objects, as they are quite big
# NB.	you need R.matlab library
# 	assumes there is ONLY matlab formatted *.mat matrices, if not just make another folder with ONLY those
#	remember than when you load the rData file, the object name is 'wdc'
####################################################################
# TAKES A LOOOOOOOOOOOT OF MEM
####################################################################
library('R.matlab')
mat_translate <- function(inFolder,outFolder)
{	
	file <- list.files(inFolder)

	for (f in 1:length(file)){
		fName <- strsplit(file[f],split=".");
		tmp <- readMat(paste(inFolder,file[f],sep=""))
		wdc <- tmp[[1]]
		save(wdc,file=paste(outFolder,paste(fName[1:(length(fName)-1)],"rData",sep="."),sep=""))
		rm(tmp,wdc)
	}
print("### > process completed",quote=F)
}

# rotate matrix by 45 degrees
####################################################################
# problem is that a basic plot read row a X and col as Y
# so that a visually oriented matrix (north at the top, west on the left)
# is plotted north on the left and west at bottom
# RETURN a matrix which can be ploted with image(mat) for instance 
####################################################################
mat_rotate45 <- function(mat)
{
return<-(t(mat[dim(mat)[1]:1,]))
}

# transform lat,lon coordinates into the closest suitable matrix indices
####################################################################
# INPUTS	mLat top-north left-west Latitudes matrix
# 		mLon top-north left-west Longitudes matrix
#		coo = c(latitude,longitude)
# RETURNS	index = c(latitude,longitude) in a top-north left-west matrix
####################################################################
mat_latLon2matIndex <- function(coo,mLat,mLon)
{
	index<-c(NA,NA)
	dLat <- mLat-coo[1]
	dLon <- mLon-coo[2]
	index[1] <- which(abs(dLat[,1])==min(abs(dLat[,1])))
	index[2] <- which(abs(dLon[1,])==min(abs(dLon[1,])))

return(index)
}


