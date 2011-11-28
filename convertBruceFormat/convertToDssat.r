##
 # FILE convertToDssat.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/convertBruceFormat
 ###############################################################################
 #
 # functions required for DSSAT format only
 #
 ###############################################################################

##
 # FORMAT AND WRITE DATA INTO OUTPUT .WTH DSSAT FILE
 ###############################################################################
formatToWTHfile <- function(data,fileHead,path)
{

# make one table from all the data
	dssat_table <- cbind(dssat_table,paste(as.integer(data$year),as.integer(data$julDay),sep=""));	# year and jul day titled 'DATE'
	dssat_table <- cbind(dssat_table,as.numeric(data$sRad));	# radiation titled	'SRAD'
	dssat_table <- cbind(dssat_table,as.numeric(data$tmx));		# temp max titled	'TMAX'
	dssat_table <- cbind(dssat_table,as.numeric(data$tmn));		# temp min titled	'TMIN'
	dssat_table <- cbind(dssat_table,as.numeric(data$ppt));		# precipitation titled	'RAIN'
	dssat_table[,4:7] <- format(dssat_table[,4:7],digits=3);

# write it into a .WTH file
	outName <- paste(path$output,strsplit(path$file$temp,"\\.")[[1]][1],".WTH",sep="");
	if(!file.exists(path$output)){	# create output dir if does not exists
		dir.create(path$output, showWarnings = FALSE, recursive = FALSE, mode = "0777");
	}else{				# remove output file if does exists
		if(file.exists(outName))	file.remove(outName);
	}
	# head
	file.copy(	"./dssatTemplates/dssatTemplate.WTH",	outName,overwrite=TRUE);
	changeVar(	"station_id",	fileHead$station$id,	outName,outName);
	changeVar(	"station_comm",	fileHead$comment,	outName,outName);
	changeVar(	"stat_lat",	fileHead$station$lat,	outName,outName);
	changeVar(	"stat_lon",	fileHead$station$lon,	outName,outName);
	changeVar(	"stat_alt",	fileHead$station$alt,	outName,outName);
	changeVar(	"inLand",	ifelse(path$inland,"0.16 (in land)","0.19 (coastal)"),	outName,outName);
	changeVar(	"period_tav",	data$tav,		outName,outName);
	changeVar(	"period_amp",	data$amp,		outName,outName);
	# body
	dssat_table <- format(dssat_table,justify="right",width=7);
	write.table(dssat_table,outName,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE);

	## to handle the linux/windows format issue
	if(Sys.info()["sysname"]=="Linux"){
		tempFile <- readLines(outName,n=-1,warn=FALSE);
		writeLines(tempFile,outName,sep="\r\n");
	}
}

