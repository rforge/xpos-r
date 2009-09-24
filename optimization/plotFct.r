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
watchDecSpace <- function(uneList,bgCol)
{
	## decision space figure
	for (r in 1:uneList$itemNo){
		plotRectangle(uneList$regEva[[r]]$regDef,1,2,bgCol,"black",NULL);
		plotDecDef(uneList$regEva[[r]]$itemNo,uneList$regEva[[r]]$decDef,1,2,"+","black");
	}
}

##
 # PLOTTING TO 'SEE' THE CRITERIA SPACE TROUGH THE PROCESS
 ####################################################################
watchCriSpace <- function(uneList,pch,pCol)
{
	## decision space figure
	for (r in 1:uneList$itemNo){
		plotDecEva(uneList$regEva[[r]]$itemNo,uneList$regEva[[r]]$decEva,1,2,pch,pCol);
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
			windows(title=" *** xPos-a : decision space visulalisation ***");
			decDev <- 2;
			plot.new();
			plotAxes(decS,1,2,"decision 1","decision 2");
			plotRectangle(decS,1,2,"white","white","decision space decomposition");
		}

		##### criteria space
		if(dim(criS)[2]==2){
			windows(title=" *** xPos-a : criteria space visulalisation ***");
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
			windows(title=" *** xPos-a : decision space visulalisation ***");
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
			windows(title=" *** xPos-a : criteria space visulalisation ***");
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
			if(unbList$item>0){watchDecSpace(unbList,"red");}
			if(penList$item>0){watchDecSpace(penList,"blue");}
			if(proList$item>0){watchDecSpace(proList,"gold");}
			#if(besList$item>0){watchDecSpace(besList,"green");}
		}

		## criteria space
		if(!is.null(criDev)){
			dev.set(criDev);
			plotRectangle(criS,1,2,"white","white","criteria space evaluation");
			if(unbList$item>0){watchCriSpace(unbList,".","red");}
			if(penList$item>0){watchCriSpace(penList,"+","blue");}
			if(proList$item>0){watchCriSpace(proList,"+","black");}
			if(besList$item>0){watchCriSpace(besList,"o","green");}
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
				if(unbList$item>0){watchDecSpace(unbList,"white");}
				if(penList$item>0){watchDecSpace(penList,"white");}
				watchDecSpace(proList,"gold");
			}
			if(penList$itemNo>0){
				par(mfg=penScreen);
				if(unbList$item>0){watchDecSpace(unbList,"white");}
				if(proList$item>0){watchDecSpace(proList,"white");}
				watchDecSpace(penList,"blue");
			}
			if(besList$itemNo>0){
				par(mfg=besScreen);
				if(unbList$item>0){watchDecSpace(unbList,"white");}
				if(proList$item>0){watchDecSpace(proList,"white");}
				if(penList$item>0){watchDecSpace(penList,"white");}
				watchDecSpace(besList,"green");
			}	
			if(unbList$item>0){
				par(mfg=unbScreen);
				watchDecSpace(unbList,"red");
			}
		}
	
		## criteria space
		if(!is.null(criDev)){
			dev.set(criDev);
			if(proList$itemNo>0){
				par(mfg=proScreen);
				plotRectangle(criS,1,2,"white","white","promising region evaluations");
				watchCriSpace(proList,"+","black");
			}
			if(penList$itemNo>0){
				par(mfg=penScreen);
				plotRectangle(criS,1,2,"white","white","pending region evaluations");
				watchCriSpace(penList,"+","blue");
			}	
			if(besList$itemNo>0){
				par(mfg=besScreen);
				plotRectangle(criS,1,2,"white","white","current best region evaluations");
				watchCriSpace(besList,"o","green");
			}	
			if(unbList$item>0){
				par(mfg=unbScreen);
				watchCriSpace(unbList,".","red");
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
			if(unbList$item>0){watchDecSpace(unbList,"red");}
			if(penList$item>0){watchDecSpace(penList,"blue");}
			if(proList$item>0){watchDecSpace(proList,"gold");}
			if(besList$item>0){watchDecSpace(besList,"green");}
		}

		## criteria space
		if(!is.null(criDev)){
			dev.set(criDev);
			plotRectangle(criS,1,2,"white","white","criteria space evaluation");
			if(unbList$item>0){watchCriSpace(unbList,".","red");}
			if(penList$item>0){watchCriSpace(penList,"+","blue");}
			if(proList$item>0){watchCriSpace(proList,"+","black");}
			if(besList$item>0){watchCriSpace(besList,"o","green");}
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
				if(unbList$item>0){watchDecSpace(unbList,"white");}
				if(penList$item>0){watchDecSpace(penList,"white");}
				watchDecSpace(proList,"gold");
			}
			if(penList$itemNo>0){
				par(mfg=penScreen);
				if(unbList$item>0){watchDecSpace(unbList,"white");}
				if(proList$item>0){watchDecSpace(proList,"white");}
				watchDecSpace(penList,"blue");
			}
			if(besList$itemNo>0){
				par(mfg=besScreen);
				if(unbList$item>0){watchDecSpace(unbList,"white");}
				if(proList$item>0){watchDecSpace(proList,"white");}
				if(penList$item>0){watchDecSpace(penList,"white");}
				watchDecSpace(besList,"green");
			}	
			if(unbList$item>0){
				par(mfg=unbScreen);
				watchDecSpace(unbList,"red");
			}
		}
	
		## criteria space
		if(!is.null(criDev)){
			dev.set(criDev);
			if(proList$itemNo>0){
				par(mfg=proScreen);
				plotRectangle(criS,1,2,"white","white","promising region evaluations");
				watchCriSpace(proList,"+","black");
			}
			if(penList$itemNo>0){
				par(mfg=penScreen);
				plotRectangle(criS,1,2,"white","white","pending region evaluations");
				watchCriSpace(penList,"+","blue");
			}
			if(besList$itemNo>0){
				par(mfg=besScreen);
				plotRectangle(criS,1,2,"white","white","current best region evaluations");
				watchCriSpace(besList,"o","green");
			}	
			if(unbList$item>0){
				par(mfg=unbScreen);
				watchCriSpace(unbList,".","red");
			}
		}
	}
}

## after optimization visualiation tools
 ####################################################################
showListInDecisionSpace <- function(uneList,decS,varX,varY,bgCol)
{
	if(uneList$itemNo==0){
		print("##",quote=FALSE);
		print("## -- empty list",quote=FALSE);
		print("##",quote=FALSE);
	}else{
		windows(title=" *** xPos-a : decision space visulalisation ***");
		plot.new();
		plotAxes(decS,varX,varY,"decision X","decision Y");
		plotRectangle(decS,varX,varY,"white","white","list regions visualisation");

		watchDecSpace(uneList,bgCol);
	}
	stop();
}