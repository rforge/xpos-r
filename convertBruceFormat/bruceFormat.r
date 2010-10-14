##
 # FILE bruceFormat.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/convertBruceFormat
 ###############################################################################
 #
 # functions required to read, and work on bruce data format
 #
 ###############################################################################

## 
 # HEAD FILES FORMAT
 ###############################################################################
read_bruceHeadFile <- function(path2file)
{
	## first line
	temp <- scan(path2file,what="character",sep=" ",nlines=1,quiet=TRUE);
	temp <- temp[temp!=""];
	station <- list("id"=temp[1],"lat"=as.numeric(temp[2]),"lon"=as.numeric(temp[3]),"alt"=as.numeric(temp[4]));

	## second line
	temp <- scan(path2file,what="character",sep=" ",skip=1,nlines=1,quiet=TRUE);
	temp <- temp[temp!=""];
	period <- list("start"=as.Date(temp[1],"%Y%m%d"),"end"=as.Date(temp[2],"%Y%m%d"),"type"=as.numeric(temp[3]));
	## for info
	# year	=	format(date,"%Y")
	# month	=	format(date,"%m")
	# day	=	format(date,"%d")

	## third line
	temp <- scan(path2file,what="character",sep=" ",skip=2,nlines=1,quiet=TRUE);
	temp <- temp[temp!=""];
	comm <- temp[1];

return(list("station"=station,"period"=period,"comment"=comm));
}

## 
 # IMPORT DATA according to common period
 ###############################################################################
importData <- function(path,fileHead)
{
# import all
	# tmn
	path2file <- paste(path$input,path$folder$tmn,path$file$temp,sep="");
	head_tmn <- read_bruceHeadFile(path2file);
	tmn <- as.array(as.numeric(scan(path2file,what="numeric",sep="\n",skip=3,nlines=-1,quiet=TRUE)));
	# tmx
	path2file <- paste(path$input,path$folder$tmx,path$file$temp,sep="");
	head_tmx <- read_bruceHeadFile(path2file);
	tmx <- as.array(as.numeric(scan(path2file,what="numeric",sep="\n",skip=3,nlines=-1,quiet=TRUE)));
	# ppt
	path2file <- paste(path$input,path$folder$ppt,path$file$prec,sep="");
	head_ppt <- read_bruceHeadFile(path2file);
	ppt <- as.array(as.numeric(scan(path2file,what="numeric",sep="\n",skip=3,nlines=-1,quiet=TRUE)));

# cut the tail of files that do not belong into the common period
	# tmn
	tail <- 0; tail <- head_tmn$period$end-fileHead$period$end;
	if(tail>0) tmn <- tmn[1:(dim(tmn)[1]-tail)];
	# tmx
	tail <- 0; tail <- head_tmx$period$end-fileHead$period$end;
	if(tail>0) tmx <- tmx[1:(dim(tmx)[1]-tail)];
	# ppt
	tail <- 0; tail <- head_ppt$period$end-fileHead$period$end;
	if(tail>0) ppt <- ppt[1:(dim(ppt)[1]-tail)];

# cut the head of files that do not belong into the common period
	# tmn
	head <- 0; head <- fileHead$period$start-head_tmn$period$start;
	if(head>0) tmn <- tmn[(head+1):dim(tmn)[1]];
	# tmx
	head <- 0; head <- fileHead$period$start-head_tmx$period$start;
	if(head>0) tmx <- tmx[(head+1):dim(tmx)[1]];
	# ppt
	head <- 0; head <- fileHead$period$start-head_ppt$period$start;
	if(head>0) ppt <- ppt[(head+1):dim(ppt)[1]];

# check period consistencies
	if( dim(tmn)!=dim(tmx) || dim(tmn)!=dim(ppt) || dim(tmx)!=dim(ppt)){
		print("# ERROR: not your fault, but the importData function (in bruceFormat.r) is failing producing files of same length :(",quote=FALSE);
		stop();
	}

return(list("tmn"=tmn,"tmx"=tmx,"ppt"=ppt));
}


