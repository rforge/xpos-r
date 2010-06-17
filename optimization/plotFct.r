##
 # FILE plotFct.r
 # AUTHOR olivier crespo
 # DATE july 2009 - july 2009, 13
 # adapted plot function for decision and criteria space
 ####################################################################


##
 # POP OUT THE FIGURE WITH AXES
 ####################################################################
plotAxes <- function(regDef,varX,varY,labX,labY)
{
	plot(	seq(regDef[1,varX],regDef[2,varX],(regDef[2,varX]-regDef[1,varX])/10),	# non plotted
		seq(regDef[1,varY],regDef[2,varY],(regDef[2,varY]-regDef[1,varY])/10),	# non plotted
		type="n",				# do not plot
		xlab=labX,				# X label
		ylab=labY,				# Y label
		xlim=c(regDef[1,varX],regDef[2,varX]),	# X limit
		ylim=c(regDef[1,varY],regDef[2,varY])	# Y limit
	);
}

##
 # DRAW A FILLED RECTANGLE
 ####################################################################
plotRectangle <- function(regDef,varX,varY,filCol,borCol,title)
{
	rect(	regDef[1,varX],	# x left
		regDef[1,varY],	# y bottom
		regDef[2,varX],	# x right
		regDef[2,varY],	# y top
		density=NA,
		col=filCol,
		border=borCol,
		asp=1
	);
	if (!is.null(title)){title(title);}
}

##
 # PLOT DECISION DEFINITION VECTORS FROM A LIST
 ####################################################################
plotDecDef <- function(decNo,decDef,varX,varY,pch,color)
{
	for (d in 1:decNo){
		points(decDef[[d]][varX],decDef[[d]][varY],pch=pch,asp=1,col=color);
	}	
}

##
 # PLOT DECISION EVALUATION MATRICES FROM A LIST
 ####################################################################
plotDecEva <- function(decNo,decEva,criX,criY,pch,color)
{
	perNo <- dim(decEva[[1]])[1];

	for (d in 1:decNo){
	for (p in 1:perNo){
		points(decEva[[d]][p,criX],decEva[[d]][p,criY],pch=pch,asp=1,col=color);
	}}
}

##
 # PLOTTING TO 'SEE' THE DECISION SPACE TROUGH THE PROCESS
 ####################################################################
watchDecSpace <- function(uneList,varX,varY,bgCol)
{
	## decision space figure
	for (r in 1:uneList$itemNo){
		plotRectangle(uneList$regEva[[r]]$regDef,varX,varY,bgCol,"black",NULL);
		plotDecDef(uneList$regEva[[r]]$itemNo,uneList$regEva[[r]]$decDef,varX,varY,"+","black");
	}
}

##
 # PLOTTING TO 'SEE' THE CRITERIA SPACE TROUGH THE PROCESS
 ####################################################################
watchCriSpace <- function(uneList,criX,criY,pch,pCol)
{
	## decision space figure
	for (r in 1:uneList$itemNo){
		plotDecEva(uneList$regEva[[r]]$itemNo,uneList$regEva[[r]]$decEva,criX,criY,pch,pCol);
	}
}

##
 # INITIALISE GRAPHIC DISPLAY
 ####################################################################
init_visualisation <- function(seeItThrough,decS,criS)
{

	graphics.off();
	
	if (seeItThrough=="g"){	# one graph including pro, pen and unb
	decDev <- NULL;
	criDev <- NULL;

		##### decision space
		if(dim(decS)[2]==2){
			if(Sys.info()["sysname"]=="Linux"){
				x11(title=" *** xPos-a : decision space visulalisation ***");
			}else{	windows(title=" *** xPos-a : decision space visulalisation ***");
			}
			decDev <- 2;
			plot.new();
			plotAxes(decS,1,2,"decision 1","decision 2");
			plotRectangle(decS,1,2,"white","white","decision space decomposition");
		}

		##### criteria space
		if(dim(criS)[2]==2){
			if(Sys.info()["sysname"]=="Linux"){
				x11(title=" *** xPos-a : criteria space visulalisation ***");
			}else{				windows(title=" *** xPos-a : criteria space visulalisation ***");
			}
			criDev <- 3;
			plot.new();
			plotAxes(criS,1,2,"criterion 1","criterion 2");
			plotRectangle(criS,1,2,"white","white","criteria space evaluation");
		}

		return(list("dDev"=decDev,"cDev"=criDev,"criS"=criS));
	}

	if (seeItThrough=="d"){	# one graph per list pro, pen and unb
	decDev <- NULL;
	criDev <- NULL;

		mfcol=c(2,2);
		penScreen <- c(1,1);
		unbScreen <- c(2,1);
		proScreen <- c(1,2);
		besScreen <- c(2,2);

		##### decision space
		if(dim(decS)[2]==2){
			if(Sys.info()["sysname"]=="Linux"){
				x11(title=" *** xPos-a : decision space visulalisation ***");
			}else{	windows(title=" *** xPos-a : decision space visulalisation ***");
			}
			decDev <- 2;
			plot.new();
			par(mfcol=mfcol);
			par(mfg=penScreen);
				plotRectangle(decS,1,2,"white","white","pending regions");
			par(mfg=proScreen);
				plotRectangle(decS,1,2,"white","white","promising region(s)");
			par(mfg=besScreen);
				plotRectangle(decS,1,2,"white","white","current best region(s)");
			par(mfg=unbScreen);
				plotAxes(decS,1,2,"decision 1","decision 2");
				plotRectangle(decS,1,2,"white","white","unbreakable regions");
		}

		##### criteria space
		if(dim(criS)[2]==2){
			if(Sys.info()["sysname"]=="Linux"){
					x11(title=" *** xPos-a : criteria space visulalisation ***");
			}else{	windows(title=" *** xPos-a : criteria space visulalisation ***");
			}
			criDev <- 3;
			plot.new();
				par(mfcol=mfcol);
			par(mfg=unbScreen);
				plotAxes(criS,1,2,"criterion 1","criterion 2");
				plotRectangle(criS,1,2,"white","white","unbreakable region evaluations");
			par(mfg=penScreen);
				plotRectangle(criS,1,2,"white","white","pending region evaluations");
			par(mfg=proScreen);
				plotRectangle(criS,1,2,"white","white","promising region evaluations");
			par(mfg=besScreen);
				plotRectangle(criS,1,2,"white","white","current best region evaluations");
		}

		return(list("pro"=proScreen,"pen"=penScreen,"unb"=unbScreen,"bes"=besScreen,"dDev"=decDev,"cDev"=criDev,"criS"=criS));
	}
}

##
 # UPDATE GRAPHIC DISPLAY WITHIN A LOOP
 ####################################################################
update_visualisation <- function(seeItThrough,scrList,proList,penList,unbList,besList)
{
	
	if (seeItThrough=="g"){
		decDev <- scrList$dDev;
		criDev <- scrList$cDev;
		criS <- scrList$criS;
		
		## decision space
		if(!is.null(decDev)){
			dev.set(decDev);
			if(unbList$item>0){watchDecSpace(unbList,1,2,"red");}
			if(penList$item>0){watchDecSpace(penList,1,2,"blue");}
			if(proList$item>0){watchDecSpace(proList,1,2,"gold");}
			#if(besList$item>0){watchDecSpace(besList,1,2,"green");}
		}

		## criteria space
		if(!is.null(criDev)){
			dev.set(criDev);
			plotRectangle(criS,1,2,"white","white","criteria space evaluation");
			if(unbList$item>0){watchCriSpace(unbList,1,2,".","red");}
			if(penList$item>0){watchCriSpace(penList,1,2,"+","blue");}
			if(proList$item>0){watchCriSpace(proList,1,2,"+","black");}
			if(besList$item>0){watchCriSpace(besList,1,2,"o","green");}
		}
	}

	if (seeItThrough=="d"){
		proScreen <- scrList$pro;
		penScreen <- scrList$pen;
		unbScreen <- scrList$unb;
		besScreen <- scrList$bes;
		decDev <- scrList$dDev;
		criDev <- scrList$cDev;
		criS <- scrList$criS;

		## decision space
		if(!is.null(decDev)){
			dev.set(decDev);
			if(proList$itemNo>0){
				par(mfg=proScreen);
				if(unbList$item>0){watchDecSpace(unbList,1,2,"white");}
				if(penList$item>0){watchDecSpace(penList,1,2,"white");}
				watchDecSpace(proList,1,2,"gold");
			}
			if(penList$itemNo>0){
				par(mfg=penScreen);
				if(unbList$item>0){watchDecSpace(unbList,1,2,"white");}
				if(proList$item>0){watchDecSpace(proList,1,2,"white");}
				watchDecSpace(penList,1,2,"blue");
			}
			if(besList$itemNo>0){
				par(mfg=besScreen);
				if(unbList$item>0){watchDecSpace(unbList,1,2,"white");}
				if(proList$item>0){watchDecSpace(proList,1,2,"white");}
				if(penList$item>0){watchDecSpace(penList,1,2,"white");}
				watchDecSpace(besList,1,2,"green");
			}	
			if(unbList$item>0){
				par(mfg=unbScreen);
				watchDecSpace(unbList,1,2,"red");
			}
		}
	
		## criteria space
		if(!is.null(criDev)){
			dev.set(criDev);
			if(proList$itemNo>0){
				par(mfg=proScreen);
				plotRectangle(criS,1,2,"white","white","promising region evaluations");
				watchCriSpace(proList,1,2,"+","black");
			}
			if(penList$itemNo>0){
				par(mfg=penScreen);
				plotRectangle(criS,1,2,"white","white","pending region evaluations");
				watchCriSpace(penList,1,2,"+","blue");
			}	
			if(besList$itemNo>0){
				par(mfg=besScreen);
				plotRectangle(criS,1,2,"white","white","current best region evaluations");
				watchCriSpace(besList,1,2,"o","green");
			}	
			if(unbList$item>0){
				par(mfg=unbScreen);
				watchCriSpace(unbList,1,2,".","red");
			}
		}
	}

#readline();#update		
}

##
 # LAST GRAPHIC DISPLAY WITHIN A LOOP
 ####################################################################
last_visualisation <- function(seeItThrough,scrList,proList,penList,unbList,besList)
{
	if (seeItThrough=="g"){
		decDev <- scrList$dDev;
		criDev <- scrList$cDev;
		criS <- scrList$criS;
		
		## decision space
		if(!is.null(decDev)){
			dev.set(decDev);
			if(unbList$item>0){watchDecSpace(unbList,1,2,"red");}
			if(penList$item>0){watchDecSpace(penList,1,2,"blue");}
			if(proList$item>0){watchDecSpace(proList,1,2,"gold");}
			if(besList$item>0){watchDecSpace(besList,1,2,"green");}
		}

		## criteria space
		if(!is.null(criDev)){
			dev.set(criDev);
			plotRectangle(criS,1,2,"white","white","criteria space evaluation");
			if(unbList$item>0){watchCriSpace(unbList,1,2,".","red");}
			if(penList$item>0){watchCriSpace(penList,1,2,"+","blue");}
			if(proList$item>0){watchCriSpace(proList,1,2,"+","black");}
			if(besList$item>0){watchCriSpace(besList,1,2,"o","green");}
		}
	}

	if (seeItThrough=="d"){
		proScreen <- scrList$pro;
		penScreen <- scrList$pen;
		unbScreen <- scrList$unb;
		besScreen <- scrList$bes;
		decDev <- scrList$dDev;
		criDev <- scrList$cDev;
		criS <- scrList$criS;

		## decision space
		if(!is.null(decDev)){
			dev.set(decDev);
			if(proList$itemNo>0){
				par(mfg=proScreen);
				if(unbList$item>0){watchDecSpace(unbList,1,2,"white");}
				if(penList$item>0){watchDecSpace(penList,1,2,"white");}
				watchDecSpace(proList,1,2,"gold");
			}
			if(penList$itemNo>0){
				par(mfg=penScreen);
				if(unbList$item>0){watchDecSpace(unbList,1,2,"white");}
				if(proList$item>0){watchDecSpace(proList,1,2,"white");}
				watchDecSpace(penList,1,2,"blue");
			}
			if(besList$itemNo>0){
				par(mfg=besScreen);
				if(unbList$item>0){watchDecSpace(unbList,1,2,"white");}
				if(proList$item>0){watchDecSpace(proList,1,2,"white");}
				if(penList$item>0){watchDecSpace(penList,1,2,"white");}
				watchDecSpace(besList,1,2,"green");
			}	
			if(unbList$item>0){
				par(mfg=unbScreen);
				watchDecSpace(unbList,1,2,"red");
			}
		}
	
		## criteria space
		if(!is.null(criDev)){
			dev.set(criDev);
			if(proList$itemNo>0){
				par(mfg=proScreen);
				plotRectangle(criS,1,2,"white","white","promising region evaluations");
				watchCriSpace(proList,1,2,"+","black");
			}
			if(penList$itemNo>0){
				par(mfg=penScreen);
				plotRectangle(criS,1,2,"white","white","pending region evaluations");
				watchCriSpace(penList,1,2,"+","blue");
			}
			if(besList$itemNo>0){
				par(mfg=besScreen);
				plotRectangle(criS,1,2,"white","white","current best region evaluations");
				watchCriSpace(besList,1,2,"o","green");
			}	
			if(unbList$item>0){
				par(mfg=unbScreen);
				watchCriSpace(unbList,1,2,".","red");
			}
		}
	}
}

################################################################################
################################################################################
##
## AFTER OPTIMIZATION PLOTTING FUNCTIONS
##
################################################################################
################################################################################

################################################################################
##### DECISION SPACE FUNCTIONS (start)
#
#

## IN DECISION SPACE
 # show a list of regions in the 2D decision space
 ###############################################################################
showListInDecisionSpace <- function(proList,penList,unbList,besList,decS,varX,varY,varH,bgCol)
{
	graphics.off();
	mfcol=c(1,floor((decS[2,varH]-decS[1,varH])/decS[3,varH]));
	screen <- c(1,1);
	if(.Platform$OS.type=="unix"){
		## LINUX
		x11(title=" *** xPos-a : decision space visulalisation (2D) ***",width=17,height=2.5);
	}else{	## WINDOWS
		windows(title=" *** xPos-a : decision space visulalisation (2D) ***");
	}
	plot.new();
	par(mfcol=mfcol);

	for (layer in seq(decS[1,varH]+(decS[3,varH]/2),decS[2,varH]-(decS[3,varH]/2),decS[3,varH])){	
		par(mfg=screen);
		par(mar=c(4,1,1,1));
		plot(	seq(decS[1,varX],decS[2,varX],(decS[2,varX]-decS[1,varX])/10),	# non plotted
			seq(decS[1,varY],decS[2,varY],(decS[2,varY]-decS[1,varY])/10),	# non plotted
			type="n",				# do not plot
			ann=FALSE,
			xlim=c(decS[1,varX],decS[2,varX]),	# X limit
			ylim=c(decS[1,varY],decS[2,varY]),	# Y limit
			pty="s"
		);
		mtext(paste("dec ",varX," vs. dec ",varY,sep=""),side=1,line=2,cex=.8);
		mtext(paste("dec ",varH," = ",layer,sep=""),side=1,line=3,cex=.8);
		for (r in 1:max(proList$itemNo,penList$itemNo,unbList$itemNo,besList$itemNo)){
#			if(proList$item>=r && proList$regEva[[r]]$regDef[1,varH]<layer && proList$regEva[[r]]$regDef[2,varH]>layer){
#				plotRectangle(proList$regEva[[r]]$regDef,varX,varY,"gold","black",NULL);
#				plotDecDef(proList$regEva[[r]]$itemNo,proList$regEva[[r]]$decDef,varX,varY,"+","black");
#				watchDecSpace(proList,varX,varY,"gold");
#			}
#			if(penList$item>=r && penList$regEva[[r]]$regDef[1,varH]<layer && penList$regEva[[r]]$regDef[2,varH]>layer){
#				plotRectangle(penList$regEva[[r]]$regDef,varX,varY,"blue","black",NULL);
#				plotDecDef(penList$regEva[[r]]$itemNo,penList$regEva[[r]]$decDef,varX,varY,"+","black");
#				watchDecSpace(penList,varX,varY,"blue");
#			}
#			if(unbList$item>=r && unbList$regEva[[r]]$regDef[1,varH]<layer && unbList$regEva[[r]]$regDef[2,varH]>layer){
#				plotRectangle(unbList$regEva[[r]]$regDef,varX,varY,"red","black",NULL);
#				plotDecDef(unbList$regEva[[r]]$itemNo,unbList$regEva[[r]]$decDef,varX,varY,"+","black");
#				watchDecSpace(unbList,varX,varY,"red");
#			}
			if(besList$item>=r && besList$regEva[[r]]$regDef[1,varH]<layer && besList$regEva[[r]]$regDef[2,varH]>layer){
				plotRectangle(besList$regEva[[r]]$regDef,varX,varY,bgCol,"black",NULL);
				plotDecDef(besList$regEva[[r]]$itemNo,besList$regEva[[r]]$decDef,varX,varY,"+","black");
#				watchDecSpace(besList,varX,varY,bgCol);
			}
		}
		screen<-screen+c(0,1);
	}
}

## IN DECISION SPACE
 # show regions which min and max are respectively less than thrMin and thrMax 
 ###############################################################################
showRegInfInDecisionSpace <- function(proList,penList,unbList,besList,decS,varX,varY,bgCol,thrMin,thrMax)
{
	if(.Platform$OS.type=="unix"){
		## LINUX
		x11(title=" *** xPos-a : decision space visulalisation ***");##,width=11,height=11);
	}else{	## WINDOWS
		windows(title=" *** xPos-a : decision space visulalisation ***");
	}
	plot.new();
	plotAxes(decS,varX,varY,"decision X","decision Y");
	plotRectangle(decS,varX,varY,"white","white","list regions visualisation");

	if(proList$item>0){watchDecSpace(proList,varX,varY,"gold");}
	if(penList$item>0){watchDecSpace(penList,varX,varY,"blue");}
	if(unbList$item>0){watchDecSpace(unbList,varX,varY,"red");}
	if(besList$item>0){watchDecSpace(besList,varX,varY,"green");}

	for (r in 1:besList$itemNo){
	for (d in 1:besList$regEva[[r]]$itemNo){
		if(min(besList$regEva[[r]]$decEva[[d]][,varX])<thrMin
			&& max(besList$regEva[[r]]$decEva[[d]][,varX])<thrMax){
			plotRectangle(besList$regEva[[r]]$regDef,varX,varY,bgCol,"black",NULL);
		}
	}}
}

## NEEDED FOR BELOW DECISION SPACE FUNCTIONS
 # based on besList,
 # compute a front and body of efficient decisions achieved
 # NB. I did not embeded them into the graphical functions
 #	as the computation might be quite time consumming
 ###############################################################################
computeFrontierIn3dDecisionSpace <- function(besList,dec1,dec2,dec3)
{
	# compute the frontiere
	frontiere <- NULL;
	if(besList$item>0){
		uneList <- besList;
		for ( r in 1:uneList$itemNo){
			print(paste(r," / ",uneList$itemNo,sep=""));
			for(x in c(uneList$regEva[[r]]$regDef[1,dec1],uneList$regEva[[r]]$regDef[2,dec1])){
			for(y in c(uneList$regEva[[r]]$regDef[1,dec2],uneList$regEva[[r]]$regDef[2,dec2])){
			for(z in c(uneList$regEva[[r]]$regDef[1,dec3],uneList$regEva[[r]]$regDef[2,dec3])){
				done<-FALSE;
				point <- as.array(c(x,y,z));

				if (!is.null(frontiere)){
					for (l in 1:dim(frontiere)[1]){
						if (all(frontiere[l,1:3]==point)){
							frontiere[l,4]<-frontiere[l,4]+1;
							done<-TRUE;
							break;
						}						
					}
				}
				if(!done) frontiere <- rbind(frontiere,c(point,0));

			}}}
		}
	}

	front<-NULL;
	body<-NULL;
	for (l in 1:dim(frontiere)[1]){
		if(frontiere[l,4]==0){
			front <- rbind(front,frontiere[l,]);
		}else{
			body <- rbind(body,frontiere[l,]);
		}
	}

#write.table(front,file="front.txt",row.names=FALSE,col.names=FALSE);
#write.table(body,file="body.txt",row.names=FALSE,col.names=FALSE);
result<-list("allBest"=frontiere,"front"=front,"body"=body);
#save(result,file="frontiere.Rdata");

return(result);
}

## IN DECISION SPACE
 # show the body of efficient decisions
 # 	in 3D and dec1/dec2, dec1/dec3, dec2/dec3 projections
 # NB. requires "scatterplot3d" R package
 ###############################################################################
showBestBodyIn3dDecisionSpace <- function(best,decS,dec1,dec2,dec3,bgCol,angle)
{
library('scatterplot3d');

	mfcol=c(2,2);
	yzScreen <- c(1,1);
	xzScreen <- c(2,1);
	xyzScreen <- c(1,2);
	xyScreen <- c(2,2);

	if(.Platform$OS.type=="unix"){
			## LINUX
			x11(title=" *** xPos-a : 3D decision space visulalisation ***");#,width=11,height=11);
	}else{		## WINDOWS
			windows(title=" *** xPos-a : decision space visulalisation ***");
	}
	plot.new();
	par(mfcol=mfcol);

	# plot 3D
	par(mfg=xyzScreen)
	scatterplot3d(
		## should be dec1, dec2 ... insead of 1,2 ..
		## actually depends on compute function before ...?
		best$body[,1],xlab="decision 1",
		best$body[,2],ylab="decision 2",
		best$body[,3],zlab="decision 3",
		xlim=decS[1:2,dec1],
		ylim=decS[1:2,dec2],
		zlim=decS[1:2,dec3],
		type="p",highlight.3d=TRUE,tick=FALSE,
		pch=20,angle=angle
	);

	# plot XY
	par(mfg=xyScreen)
	plot(	best$body[,1],xlab="decision 1",
		best$body[,2],ylab="decision 2",
		xlim=decS[1:2,dec1],
		ylim=decS[1:2,dec2],
		type="p",pch="+",col=bgCol,
		main="dec1/dec2 projection"
	);

	# plot XZ
	par(mfg=xzScreen)
	plot(	best$body[,1],xlab="decision 1",
		best$body[,3],ylab="decision 3",
		xlim=decS[1:2,dec1],
		ylim=decS[1:2,dec3],
		type="p",pch="+",col=bgCol,
		main="dec1/dec3 projection"
	);

	# plot YZ
	par(mfg=yzScreen)
	plot(	best$body[,2],xlab="decision 2",
		best$body[,3],ylab="decision 3",
		xlim=decS[1:2,dec2],
		ylim=decS[1:2,dec3],
		type="p",pch="+",col=bgCol,
		main="dec2/dec3 projection"
	);
}

## IN DECISION SPACE
 # show the frontier of efficient decisions
 # 	in 3D and dec1/dec2, dec1/dec3, dec2/dec3 projections
 # NB. requires "scatterplot3d" R package
 ###############################################################################
showBestFrontIn3dDecisionSpace <- function(best,decS,dec1,dec2,dec3,bgCol,angle)
{
library('scatterplot3d');

	mfcol=c(2,2);
	yzScreen <- c(1,1);
	xzScreen <- c(2,1);
	xyzScreen <- c(1,2);
	xyScreen <- c(2,2);

	if(.Platform$OS.type=="unix"){
			## LINUX
			x11(title=" *** xPos-a : 3D decision space visulalisation ***");#,width=11,height=11);
	}else{		## WINDOWS
			windows(title=" *** xPos-a : decision space visulalisation ***");
	}
	plot.new();
	par(mfcol=mfcol);

	# plot 3D
	par(mfg=xyzScreen)
	scatterplot3d(
		best$front[,1],xlab="decision 1",
		best$front[,2],ylab="decision 2",
		best$front[,3],zlab="decision 3",
		xlim=decS[1:2,dec1],
		ylim=decS[1:2,dec2],
		zlim=decS[1:2,dec3],
		type="p",highlight.3d=TRUE,tick=FALSE,
		pch=20,angle=angle
	);

	# plot XY
	par(mfg=xyScreen)
	plot(	best$front[,1],xlab="decision 1",
		best$front[,2],ylab="decision 2",
		xlim=decS[1:2,dec1],
		ylim=decS[1:2,dec2],
		type="p",pch="+",col=bgCol,
		main="dec1/dec2 projection"
	);

	# plot XZ
	par(mfg=xzScreen)
	plot(	best$front[,1],xlab="decision 1",
		best$front[,3],ylab="decision 3",
		xlim=decS[1:2,dec1],
		ylim=decS[1:2,dec3],
		type="p",pch="+",col=bgCol,
		main="dec1/dec3 projection"
	);

	# plot YZ
	par(mfg=yzScreen)
	plot(	best$front[,2],xlab="decision 2",
		best$front[,3],ylab="decision 3",
		xlim=decS[1:2,dec2],
		ylim=decS[1:2,dec3],
		type="p",pch="+",col=bgCol,
		main="dec2/dec3 projection"
	);
}

#
#
##### DECISION SPACE FUNCTIONS (end)
################################################################################
##### CRITERIA SPACE FUNCTIONS (start)
#
#

## NEEDED FOR BELOW CRITERIA SPACE FUNCTIONS
 # based on besList,
 # compute a front and body of non dominated groups achieved
 # NB. I did not embeded them into the graphical functions
 #	as the computation might be quite time consumming
 # CONSTRUCTION
 ###############################################################################
computeFrontierIn3dCriteriaSpace <- function(besList,cri1,cri2,cri3)
{
	# compute the frontiere
	frontiere <- NULL;
	if(besList$item>0){
		uneList <- besList;
		for ( r in 1:uneList$itemNo){
			print(paste(r," / ",uneList$itemNo,sep=""));
			for ( d in 1:uneList$regEva[[r]]$itemNo){
				for ( p in 1:dim(uneList$regEva[[r]]$decEva[[d]])[1]){
					frontiere <- rbind(frontiere,as.array(c(	uneList$regEva[[r]]$decEva[[d]][p,cri1],
											uneList$regEva[[r]]$decEva[[d]][p,cri2],
											uneList$regEva[[r]]$decEva[[d]][p,cri3]
										)));
				}
			}
		}
	}

	## pareto front only
source('evaluation.r');
	temp <- frontiere;
	temp <- cbind(temp,array(0,dim=c(dim(temp)[1],1)));
	for (p1 in 1:(dim(temp)[1]-1)){
		for (p2 in (p1+1):dim(temp)[1]){
			print(paste(p1," / ",p2,sep=""));
			switch(paretoDomi_decPerVSdecPer(temp[p1,1:3],temp[p2,1:3]),
				## 1: p1 dominates p2
				{temp[p2,(dim(frontiere)[2]+1)]<-temp[p2,(dim(frontiere)[2]+1)]+1;},
				## 2: p2 domintes p1
				{temp[p1,(dim(frontiere)[2]+1)]<-temp[p1,(dim(frontiere)[2]+1)]+1;},
				{}
				);
		}
	}
	front<-NULL;
	for (l in 1:dim(temp)[1]){
		if(temp[l,4]==0){
			front <- rbind(front,temp[l,]);
		}
	}

## XY front
print("frontXY");
	temp<-front[,1:2];
	temp<-cbind(temp,array(0,dim=dim(temp)[1]));
	for (l1 in 1:(dim(temp)[1]-1)){
		for (l2 in (l1+1):dim(temp)[1]){
			print(paste(l1," / ",l2,sep=""));
			switch(paretoDomi_decPerVSdecPer(temp[l1,1:2],temp[l2,1:2]),
				## 1: p1 dominates p2
				{temp[l2,3]<-temp[l2,3]+1;},
				## 2: p2 domintes p1
				{temp[l1,3]<-temp[l1,3]+1;},
				{}
			);
		}
	}
	frontXY<-NULL;
	for (l in 1:dim(temp)[1]){
		if(temp[l,3]==0){
			frontXY <- rbind(frontXY,temp[l,]);
		}
	}

## YZ front
print("frontYZ");
	temp<-front[,2:3];
	temp<-cbind(temp,array(0,dim=dim(temp)[1]));
	for (l1 in 1:(dim(temp)[1]-1)){
		for (l2 in (l1+1):dim(temp)[1]){
			print(paste(l1," / ",l2,sep=""));
			switch(paretoDomi_decPerVSdecPer(temp[l1,1:2],temp[l2,1:2]),
				## 1: p1 dominates p2
				{temp[l2,3]<-temp[l2,3]+1;},
				## 2: p2 domintes p1
				{temp[l1,3]<-temp[l1,3]+1;},
				{}
			);
		}
	}
	frontYZ<-NULL;
	for (l in 1:dim(temp)[1]){
		if(temp[l,3]==0){
			frontYZ <- rbind(frontYZ,temp[l,]);
		}
	}
		
## XZ front
print("frontXZY");
	temp<-cbind(front[,1],front[,3],array(0,dim=dim(temp)[1]));
	for (l1 in 1:(dim(temp)[1]-1)){
		for (l2 in (l1+1):dim(temp)[1]){
			print(paste(l1," / ",l2,sep=""));
			switch(paretoDomi_decPerVSdecPer(temp[l1,1:2],temp[l2,1:2]),
				## 1: p1 dominates p2
				{temp[l2,3]<-temp[l2,3]+1;},
				## 2: p2 domintes p1
				{temp[l1,3]<-temp[l1,3]+1;},
				{}
			);
		}
	}
	frontXZ<-NULL;
	for (l in 1:dim(temp)[1]){
		if(temp[l,3]==0){
			frontXZ <- rbind(frontXZ,temp[l,]);
		}
	}

result<-list("all"=frontiere,"front"=front,"frontXY"=frontXY,"frontYZ"=frontYZ,"frontXZ"=frontXZ);
return(result);
}

## IN CRITERIA SPACE
 # show a list of region in the criteria space
 ####################################################################
showListInCriteriaSpace <- function(proList,penList,unbList,besList,criS,criX,criY,ptType,ptCol)
{
	if(.Platform$OS.type=="unix"){
		## LINUX
		x11(title=" *** xPos-a : criteria space visulalisation ***");##,width=11,height=11);
	}else{	## WINDOWS
		windows(title=" *** xPos-a : criteria space visulalisation ***");
	}
	plot.new();
	plotAxes(criS,criX,criY,"criterion A","criterion B");
	plotRectangle(criS,criX,criY,"white","white","criteria space evaluation");

	if(proList$item>0){watchCriSpace(proList,criX,criY,".","black");}
	if(penList$item>0){watchCriSpace(penList,criX,criY,".","blue");}
	if(unbList$item>0){watchCriSpace(unbList,criX,criY,".","red");}
	if(besList$item>0){watchCriSpace(besList,criX,criY,ptType,ptCol);}
}
## IN CRITERIA SPACE
 # show ???
 # NB. requires "scatterplot3d" R package
 ####################################################################
showListIn3dCriteriaSpace <- function(proList,penList,unbList,besList,criS,criX,criY,criZ,ptType,ptCol,angle)
{
	title<-" *** xPos-a : criteria space 3D visulalisation ***";
	if(.Platform$OS.type=="unix"){
		## LINUX
		x11(title=" *** xPos-a : 3D decision space visulalisation ***");#,width=11,height=11);
	}else{	## WINDOWS
		windows(title=" *** xPos-a : decision space visulalisation ***");
	}
	plot.new();

	myPlot <- scatterplot3d::scatterplot3d(
		mean(criS[1:2,criX]),xlab="criterion 1",
		mean(criS[1:2,criY]),ylab="criterion 2",
		mean(criS[1:2,criZ]),zlab="criterion 3",
		xlim=criS[1:2,criX],
		ylim=criS[1:2,criY],
		zlim=criS[1:2,criZ],
		pch="",angle=angle
	);

	if(proList$item>0){
		uneList <- proList;
		for ( r in 1:uneList$itemNo){
			for (d in 1:uneList$regEva[[r]]$itemNo){	
				myPlot$points3d(
					uneList$regEva[[r]]$decEva[[d]][,criX],
					uneList$regEva[[r]]$decEva[[d]][,criY],
					uneList$regEva[[r]]$decEva[[d]][,criZ],
					pch=".",col="gold"
				);
			}
		}
	}
	if(penList$item>0){
		uneList <- penList;
		for ( r in 1:uneList$itemNo){
			for (d in 1:uneList$regEva[[r]]$itemNo){	
				myPlot$points3d(
					uneList$regEva[[r]]$decEva[[d]][,criX],
					uneList$regEva[[r]]$decEva[[d]][,criY],
					uneList$regEva[[r]]$decEva[[d]][,criZ],
					pch=".",col="blue"
				);
			}
		}
	}
	if(unbList$item>0){
		uneList <- unbList;
		for ( r in 1:uneList$itemNo){
			for (d in 1:uneList$regEva[[r]]$itemNo){	
				myPlot$points3d(
					uneList$regEva[[r]]$decEva[[d]][,criX],
					uneList$regEva[[r]]$decEva[[d]][,criY],
					uneList$regEva[[r]]$decEva[[d]][,criZ],
					pch=".",col="red"
				);
			}
		}
	}

	if(besList$item>0){
		uneList <- besList;
		for ( r in 1:uneList$itemNo){
			for (d in 1:uneList$regEva[[r]]$itemNo){	
				myPlot$points3d(
					uneList$regEva[[r]]$decEva[[d]][,criX],
					uneList$regEva[[r]]$decEva[[d]][,criY],
					uneList$regEva[[r]]$decEva[[d]][,criZ],
					pch=ptType,col=ptCol
				);
			}
		}
	}
}

## IN CRITERIA SPACE
 # show ???
 # NB. requires "scatterplot3d" R package
 # IN CONSTRUCTION
 ###############################################################################
showBestIn3dCriteriaSpace <- function(best,criS,bgCol,angle)
{
library('scatterplot3d');

	mfcol=c(2,2);
	yzScreen <- c(1,1);
	xzScreen <- c(2,1);
	xyzScreen <- c(1,2);
	xyScreen <- c(2,2);

	if(.Platform$OS.type=="unix"){
			## LINUX
			x11(title=" *** xPos-a : 3D criteria space visulalisation ***",width=11,height=11);
	}else{		## WINDOWS
			windows(title=" *** xPos-a : 3D criteria space visulalisation ***");
	}
	plot.new();
	par(mfcol=mfcol);

	# plot 3D
	par(mfg=xyzScreen)
	scatterplot3d(
		best$all[,1],xlab="criterion 1",
		best$all[,2],ylab="criterion 2",
		best$all[,3],zlab="criterion 3",
		xlim=criS[1:2,1],
		ylim=criS[1:2,2],
		zlim=criS[1:2,3],
		type="p",highlight.3d=TRUE,tick=FALSE,
		pch=20,angle=angle
	);

	# plot XY
	par(mfg=xyScreen)
	plot(	best$all[,1],xlab="criterion 1",
		best$all[,2],ylab="criterion 2",
		xlim=criS[1:2,1],
		ylim=criS[1:2,2],
		type="p",pch=".",col=bgCol,
		main="dec1/dec2 projection"
	);
	lines(best$front,col="green")
	lines(best$front,col="green")

	# plot XZ
	par(mfg=xzScreen)
	plot(	best$all[,1],xlab="criterion 1",
		best$all[,3],ylab="criterion 3",
		xlim=criS[1:2,1],
		ylim=criS[1:2,3],
		type="p",pch=".",col=bgCol,
		main="dec1/dec3 projection"
	);

	# plot YZ
	par(mfg=yzScreen)
	plot(	best$all[,2],xlab="criterion 2",
		best$all[,3],ylab="criterion 3",
		xlim=criS[1:2,2],
		ylim=criS[1:2,3],
		type	="p",pch=".",col=bgCol,
		main="dec2/dec3 projection"
	);
}

#
#
##### CRITERIA SPACE FUNCTIONS (end)
################################################################################

