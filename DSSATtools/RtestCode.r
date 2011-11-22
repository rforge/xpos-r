read_soilFile <- function(path2file)
{
	## find out first part of soil file name
	temp <- strsplit(path2file,split=".",fixed=TRUE);
	first <- temp[[1]][1];
	
	## read all file
	temp <- scan(path2file,what="character",sep=" ", blank.lines.skip = FALSE);
	for(i in 1:length(temp)){
		test <- strsplit(temp[i],split=first);
print(test);
print(!is.character(test))	;
browser();
		# if empty character, skip to nect step
		if(!is.character(test))	next;

		if(test[[1]][1]=="*"){	## this is a soil
			print(paste(test[[1]][1],first,test[[1]][2]," cmp ",temp[i]));
		}
	}

	## i'm looking for "*firstxxxxxxx"

return(temp)
}

