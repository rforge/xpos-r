##
 # FILE convertDscl2Apsim
 # AUTHOR olivier crespo
 #########################################################################

source('convertD2Afct.r');

## read user settings
data <- init_data();
stationNames <- init_stationName();
gcmNames <- init_gcmNames();

## loop initialization
station <- stationNames[1];

tp <- convert_OneStation4OnePeriod(data,station);

##########################################################################
## REWRITE
##########################################################################

for(data in 1:dim(dataDscled)[1]){
		temp <- read.table(paste(path2data2read,"/",model[gcm,period],"/",dataDscled[data],"/",fileName[station],sep=""),skip=3);
		if(!exists("redTable")){redTable <- array(NA,dim=c(dim(temp)[1],dim(dataDscled)[1]));}
		redTable[,data] <- temp[,1];
	}

	

			## adapt table
			table <- array(NA,dim=c(dim(redTable)[1],3));
			switch(fDate+1,
				table <- realDay(table,fileName[station],sYear,sMonth,sDay,eYear,eMonth,eDay,dayPerYear),
				table <- 365Day(table,fileName[station],sYear,sMonth,sDay,eYear,eMonth,eDay,dayPerYear),
				print("360 day")
				#table <- 360Day(table2write)
			);
			table2write <- cbind(table,redTable);

			## write apsim format file
			addHeadFile(path2data2write,model,fileName,gcm,period,station,lat,lon,alt,sYear,sMonth,sDay,eYear,eMonth,eDay);
			write.table(table2write,file=paste(path2data2write,"/",model[gcm,period],"/",fileName[station],sep=""),append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE,sep=" ");
		}
	}
}
