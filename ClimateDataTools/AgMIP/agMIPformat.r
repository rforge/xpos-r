inF<-'../../../../12_AgMIP/2012-10-01_fastTrack/AMIP/MerraData_CM/SABA0QXX.AgMIP'

readAgMIPformat <- function(inFile)
{
	# read AgMIP format data
	data <-	scan(inFile,skip=5)
	data <- matrix(data,ncol=12,byrow=T)
	colnames(data) <- scan(inFile,what='raw',skip=4,nlines=1)

	# read header
	head <- scan(inFile,what='raw',skip=3,nlines=1)
	head <- matrix(head,ncol=8,byrow=T)
	nam <- scan(inFile,what='raw',skip=2,nlines=1)
	colnames(head) <- nam[2:length(nam)]

	agmip <- list("name"=inFile,"data"=data,"head"=head)

return(agmip)
}

biaseCol <- function(data,col,much,version)
{
	data[,col] <- data[,col]+much;
}


