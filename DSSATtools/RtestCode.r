read_oneSOLfile <- function(path2file)
{
	## find out first part of soil file name
	temp <- strsplit(path2file,split="/",fixed=TRUE);
	temp <- strsplit(temp[[1]][length(temp[[1]])],split=".",fixed=TRUE);
	first <- temp[[1]][1];
	
	## read all file, line by line
	temp <- readLines(path2file);
	soils <- NULL;
	for(i in 1:length(temp)){
		test <- try(strsplit(temp[i],split=first),silent=TRUE);
		if(length(test[[1]])>1 && test[[1]][1]=="*"){	## this is a soil
			soils <- rbind(soils,temp[i]);
		}
	}

return(soils);
}

read_allSOLfiles <- function(path2Dir)
{
	SOLfile <- list.files(path2Dir,pattern="*.SOL");

	soils <- NULL;
	for (i in 1:length(SOLfile)){
		soils <- rbind(soils,read_oneSOLfile(paste(path2Dir,SOLfile[i],sep="")));
	}

	write(soils,file=paste(path2Dir,"allSoilstype.tmp",sep=""))
return(soils);
}
