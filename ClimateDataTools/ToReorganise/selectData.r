##
 # FILE selectData.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
 ####################################################################
 # attempt to build functions that will help selecting data within a huge file
 ####################################################################

library("XML");


##
 # lisa gives me a csv file for africa, how do I select the station I want ..
 ####################################################################

# @param latBondaries <- c(minLat,maxLat)
# @param lonBondaries <- c(minLon,maxLon)
stationID_inRectangle <- function(csvFile,latBoundaries,lonBoundaries,IDcol=1,latcol=2,loncol=3)
{
	latBoundaries<-sort(latBoundaries);
	lonBoundaries<-sort(lonBoundaries);
	mincol<-1;	maxcol<-2;

	temp <- read.csv(csvFile);

	temp<-temp[(temp[,latcol]>=latBoundaries[mincol]),1:4];	# >= minLat
	temp<-temp[(temp[,latcol]<=latBoundaries[maxcol]),1:4];	# <= maxLat
	temp<-temp[(temp[,loncol]>=lonBoundaries[mincol]),1:4];	# >= minLon
	temp<-temp[(temp[,loncol]<=lonBoundaries[maxcol]),1:4];	# <= maxLon

return(temp);
}

# @param rad in decimal degrees
stationID_aroundPoint <- function(csvFile,lat,lon,rad,IDcol=1,latcol=2,loncol=3)
{
	temp <- stationID_inRectangle(csvFile,c((lat-rad),(lat+rad)),c((lon-rad),(lon+rad)),IDcol=1,latcol=2,loncol=3);

return(temp);
}

# most of the time I simply need a raw file: no col names, no rows name, no quotes ...
write.rawCSV <- function(x,file,append=FALSE)
{
	write.table(x,file,row.names=FALSE,col.names=FALSE,quote=FALSE,sep=",",append=append);
}

##
 # KML files (attempt) - requires pakage("XML"())
 ####################################################################

##
 # get node into a sublevel named "name"
 ####################################################################
 # @name : leaf name at sublevel
node.sublevel <- function(node,name)
{	
	for (l in 1:length(node)){
		if( xmlName(node[[l]]) == name){	break;}
	}
return(node[[l]]);
}

##
 # create a placemark list
 ####################################################################
KMLdoc.byPlacemark <- function(doc)
{
	placemark <- list("pm"=NULL);
	for (pm in 1:length(doc)){
print(paste("placemark:",pm,"/",length(doc),sep=" "));
		name <- xmlValue(node.sublevel(doc[[pm]],"name"));
		description <- xmlValue(node.sublevel(doc[[pm]],"description"));
		sen <- strsplit(description,split="\n ")[[1]];
		point <- xmlValue(node.sublevel(doc[[pm]],"point"));
		lla <- strsplit(point,split=",")[[1]];

		placemark[[pm]] <- list("nam"=name,"des"=sen,"coo"=lla);
	}

return(placemark);
}

##
 # subselection of placemark within rectangle (lon,lat)
 ####################################################################
pm.inRect <- function(allPM,latR,lonR)
{
	latR <- sort(latR);
	lonR <- sort(lonR);
	mincol<-1;
	maxcol<-2;

	new_pm <- list("pm"=NULL);
	new_it <- 1;
	for (pm in 1:length(allPM)){
		if(	as.numeric(allPM[[pm]]$coo[1])>=lonR[mincol] &&
			as.numeric(allPM[[pm]]$coo[1])<=lonR[maxcol] &&
			as.numeric(allPM[[pm]]$coo[2])>=latR[mincol] &&
			as.numeric(allPM[[pm]]$coo[2])<=latR[maxcol] ){
				new_pm[[new_it]] <- allPM[[pm]];
				new_it <- new_it+1;
		}
	}

return(new_pm);
}

writeIt_oldSchool <- function(placemark,file)
{
	# file head
	write(paste(	"<?xml version=\"1.0\" encoding=\"utf-8\"?>",
			" <kml xmlns=\"http://earth.google.com/kml/2.2\">",
			"  <Document>",
			sep="\n"),file,append=FALSE);
	
	# file body
	for(pm in 1:length(placemark)){
		write(paste(	"   <Placemark>",
				"    <name>",
				placemark[[pm]]$nam,
				"    </name>",
				"    <description>",
				placemark[[pm]]$des[1],
				placemark[[pm]]$des[2],
				placemark[[pm]]$des[3],
				"    </description>",
				"    <Point>",
				"     <coordinates>",
				paste(placemark[[pm]]$coo[1],placemark[[pm]]$coo[2],placemark[[pm]]$coo[3],sep=","),
				"     </coordinates>",
				"    </Point>",
				"   </Placemark>",
				sep="\n"),file,append=TRUE);
	}

	# file tail
	write(paste(	"  </Document>",
			" </kml>",
			sep="\n"),file,append=TRUE);

}

##
 # something
 ####################################################################
runItAll <- function(kmlFile)
{
	x <- xmlTreeParse(kmlFile);
	kml <- x$doc$children$kml;
	doc <- node.sublevel(kml,"Document");
	old_pm <- KMLdoc.byPlacemark(doc);

	# lesotho - all
	# latR = (-30.675,-28.570)
	# lonR = (27.011,29.455)
#+/-1 deg
#	lesotho_pm <- pm.inRect(old_pm,c((-30.675-1),(-28.570+1)),c((27.011-1),(29.455+1)));
#+/-.1 deg
	lesotho_pm <- pm.inRect(old_pm,c((-30.675-.1),(-28.570+.1)),c((27.011-.1),(29.455+.1)));
	# maphutseng -30.213,27.517
#	maphutseng <- pm.inRect(old_pm,c((-30.213-1),(-30.213+1)),c((27.517-1),(27.517+1)));

	# swaziland - all
	# latR = (-27.317,-25.718)
	# lonR = (30.790,32.134)
#+/-1 deg
#	swaziland_pm <- pm.inRect(old_pm,c((-27.317-1),(-25.718+1)),c((30.790-1),(32.134+1)));
#+/-.1 deg
	swaziland_pm <- pm.inRect(old_pm,c((-27.317-.1),(-25.718+.1)),c((30.790-.1),(32.134+.1)));
	# mpolonjeni_1? -26.313,31.135
#	mpolonjeni_1 <- pm.inRect(old_pm,c((-26.313-1),(-26.313+1)),c((31.135-1),(31.135+1)));
	# mpolonjeni_2? -26.589,31.830
#	mpolonjeni_2 <- pm.inRect(old_pm,c((-26.589-1),(-26.589+1)),c((31.830-1),(31.830+1)));

	# malawi - all
	# latR = (-17.136,-9.368)
	# lonR = (32.678,35.923)
#+/-1 deg
#	malawi_pm <- pm.inRect(old_pm,c((-17.136-1),(-9.368+1)),c((32.678-1),(35.923+1)));
#+/-.1 deg
	malawi_pm <- pm.inRect(old_pm,c((-17.136-.1),(-9.368+.1)),c((32.678-.1),(35.923+.1)));
	# lilongwe -13.983,33.783
#	lilongwe <- pm.inRect(old_pm,c((-13.983-1),(-13.983+1)),c((33.783-1),(33.783+1)));

	# print files
# all_ppt
#	writeIt_oldSchool(lesotho_pm,file="/home/crespo/Desktop/11_SECCAP/ClimateData/Rselection_afterFANRPAN/lesotho_all_ppt.kml");
#	writeIt_oldSchool(swaziland_pm,file="/home/crespo/Desktop/11_SECCAP/ClimateData/Rselection_afterFANRPAN/swaziland_all_ppt.kml");
#	writeIt_oldSchool(malawi_pm,file="/home/crespo/Desktop/11_SECCAP/ClimateData/Rselection_afterFANRPAN/malawi_all_ppt.kml");
# all_tmn
	writeIt_oldSchool(lesotho_pm,file="/home/crespo/Desktop/11_SECCAP/ClimateData/Rselection_afterFANRPAN/lesotho_all_tmn.kml");
	writeIt_oldSchool(swaziland_pm,file="/home/crespo/Desktop/11_SECCAP/ClimateData/Rselection_afterFANRPAN/swaziland_all_tmn.kml");
	writeIt_oldSchool(malawi_pm,file="/home/crespo/Desktop/11_SECCAP/ClimateData/Rselection_afterFANRPAN/malawi_all_tmn.kml");

#	writeIt_oldSchool(maphutseng,file="../../../11_IDRC/ClimateData/Rtest/maphutseng.kml");
#	writeIt_oldSchool(mpolonjeni_1,file="../../../11_IDRC/ClimateData/Rtest/mpolonjeni_1.kml");
#	writeIt_oldSchool(mpolonjeni_2,file="../../../11_IDRC/ClimateData/Rtest/mpolonjeni_2.kml");
#	writeIt_oldSchool(lilongwe,file="../../../11_IDRC/ClimateData/Rtest/lilongwe.kml");

}
