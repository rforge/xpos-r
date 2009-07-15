##
 # FILE evaluation.r
 # AUTHOR olivier crespo
 # DATE july 2009 - july 2009, 15
 ####################################################################

##
 #  REGION MEAN EVALUATION  
 ####################################################################
eval_mean <- function(regEva, criterion)
{
	regMean <- array(0,dim=dim(regEva$decEva[[1]])[2]);
	for (d in 1:regEva$itemNo){
		decMean <- apply(regEva$decEva[[d]],2,mean);
		regMean <- regMean + decMean;
	}
	regMean <- regMean / regEva$itemNo;
		
return(regMean[criterion]);
}

##
 #  REGION MIN DECMEAN EVALUATION  (min is the best)
 ####################################################################
eval_minMean <- function(regEva, criterion)
{
	regMinMean <- apply(regEva$decEva[[1]],2,mean);
	if(regEva$itemNo>1){
	for (d in 2:regEva$itemNo){
		decMean <- apply(regEva$decEva[[d]],2,mean);
		if(decMean[criterion]<regMinMean[criterion]){
			regMinMean <- decMean;
		}
	}}
		
return(regMinMean[criterion]);
}

##
 #  REGION MAX DECMEAN EVALUATION  (max is the worst)
 ####################################################################
eval_maxMean <- function(regEva, criterion)
{
	regMaxMean <- apply(regEva$decEva[[1]],2,mean);
	if(regEva$itemNo>1){
	for (d in 2:regEva$itemNo){
		decMean <- apply(regEva$decEva[[d]],2,mean);
		if(decMean[criterion]>regMinMean[criterion]){
			regMaxMean <- decMean;
		}
	}}
	
return(regMaxMean[criterion]);
}

#####################################################################
 #  MULTICRITERIA GROUP DOMINANCE FUNCTIONS
 ####################################################################

##
 #  (dec,per) vector Pareto dominance comparison
 ####################################################################
 # 1: v1 is dominating v2
 # 2: v2 is dominating v1
 # 3: v1 and v2 are non dominated
 ####################################################################
paretoDomi_decPerVSdecPer <- function(decPer1,decPer2)
{
	criNo <- length(decPer1);

	## is v1 dominating?
	atLeastAsGood_no <- 0;
	strictlyDominating_no <- 0;
  	for (i in 1:criNo){
    		if (decPer1[i] <= decPer2[i]){
			atLeastAsGood_no <- atLeastAsGood_no + 1;
		}else{break;}
    		if (decPer1[i] < decPer2[i]){
      		strictlyDominating_no <- strictlyDominating_no+1;
		}
	}
  	## all at least as good, and at least one better
	if((atLeastAsGood_no == criNo) && (strictlyDominating_no > 0)){
		return(1);
	}else{
		## is v2 dominating?
		atLeastAsGood_no <- 0;
		strictlyDominating_no <- 0;
		for (i in 1:criNo){
      		if (decPer2[i] <= decPer1[i]){
				atLeastAsGood_no <- atLeastAsGood_no + 1;
			}else{break;}
      		if (decPer2[i] < decPer1[i]){
	      		strictlyDominating_no <- strictlyDominating_no+1;
			}
    		}    
		## all at least as good, and at least one better
		if((atLeastAsGood_no == criNo) && (strictlyDominating_no > 0)){
			return(2);
		}else{
		## they are both non dominated
		return(3);
		}
	}
}

##
 #  reg: List of dec matrix (perNo,criNo) dominance comparison
 ####################################################################
 # 1: r1 is definitely dominating r2
 # 2: r1 is definitely dominated by r2
 # 3: r1 and r2 are definitely non dominated
 # 5: r1 is acceptably dominating r2
 # 6: r1 is acceptably dominated by r2
 # 7: r1 and r2 are acceptably non dominated
 # 9: otherwise
 ####################################################################
 #  MULTICRITERIA GROUP DOMINANCE EVALUATION
 ####################################################################
 # > REFERENCE:
 # O. Crespo, F. Garcia, J.E. Bergez,
 # Multiobjective optimization subject to uncertainty: application to agricultural resources management,
 # to be submitted
 ####################################################################
groupDomi_regVSreg <- function(reg1,reg2)
{
	decNo <- reg1$itemNo;
	if(decNo>1){
		perNo <- dim(reg1$decEva[[1]])[1];
		criNo <- dim(reg1$decEva[[1]])[2];
	}else{
		perNo <- dim(reg1$decEva)[1];
		criNo <- dim(reg1$decEva)[2];
	}
	
	#####
	# I want to avoid computing multicriteria demanding comparisons if i can
	# So I tried to eliminate alternatives as soon as possible
	# instead of computing all and finding out what domination it is
	#####
	# I start with all results possible=1 (always say r1 first, r2 last)
	# and plan to 'return' as soon as the sum of either r1 or r2 is =1 
	# potentially confirm with the other region which should have reached the according result
	# I do not know if it is worth the trouble, let us see ...
	#####
	initReg <- list("defDominating"=1,"defDominated"=1,"defNonDominated"=1,"accDominating"=1,"accDominated"=1,"accNonDominated"=1,"undecidable"=1);
	r1domi <- initReg;
	r2domi <- initReg;
	
	decPer <- array(0,dim=c(decNo,perNo));
	r1 <- list("worstTh"=decPer,"betterTh"=decPer,"nonDomi"=decPer);
	r2 <- list("worstTh"=decPer,"betterTh"=decPer,"nonDomi"=decPer);

	for (d1 in 1:decNo){
		for (p1 in 1:perNo){
			for (d2 in 1:decNo){
				for (p2 in 1:perNo){
					if (decNo>1){
						vect1 <- reg1$decEva[[d1]][p1,1:criNo];
						vect2 <- reg2$decEva[[d2]][p2,1:criNo];
					}else{
						vect1 <- reg1$decEva[p1,1:criNo];
						vect2 <- reg2$decEva[p2,1:criNo];
					}##### MULTICRITERIA DECPER COMPARISON
					switch(paretoDomi_decPerVSdecPer(vect1,vect2),
						## this response of reg1 is dominating this response of reg2
						{	r1$betterTh[d1,p1] <- r1$betterTh[d1,p1] +1;
							r2$worstTh[d2,p2] <- r2$worstTh[d2,p2] +1;
						},
						## this response of reg2 is dominating this response of reg1
						{	r1$worstTh[d1,p1] <- r1$worstTh[d1,p1] +1;
							r2$betterTh[d2,p2] <- r2$betterTh[d2,p2] +1;
						},
						## these responses of reg1 and reg2 are not dominating each other
						{	r1$nonDomi[d1,p1] <- r1$nonDomi[d1,p1] +1;
							r2$nonDomi[d2,p2] <- r2$nonDomi[d2,p2] +1;
						}
					);

					##### ELIMINATION CONDITIONS
					## not definitely non dominating as soon as
					#if(sum(r1$worstTh)>0 || sum(r2$worstTh)>0 || sum(r1$betterTh)>0 || sum(r2$betterTh)>0){
					#	r1domi$defNonDominated=0;
					#	r2domi$defNonDominated=0;
					#	# everything else is possible
					#}
					## not r1 definitely dominating r2 as soon as
					## not r2 definitely dominating r1 as soon as
					## not acceptably non dominating as soon as
					## not r1 acceptably dominating r2 as soon as
					## not r2 acceptably dominating r1 as soon as
	

					##### EARLIER STOPPING TESTS
					#if ( (r1domi[[1]]+r1domi[[2]]+r1domi[[3]]+r1domi[[4]]+r1domi[[5]]+r1domi[[6]]+r1domi[[7]]) == 1){
						# first confirm with r2domi
						# then return
					#	if(r1domi$defDominating==1){		return(1)};
					#	if(r1domi$defDominated==1){		return(2)};
					#	if(r1domi$defNonDominating==1){	return(3)};
					#	if(r1domi$accDominating==1){		return(5)};
					#	if(r1domi$accDominated==1){		return(6)};
					#	if(r1domi$accNonDominating==1){	return(7)};
					#	if(r1domi$undecidable==1){		return(9)};
					#}
				}	
			}
		}	
	}

	##### HERE EVERY DECPER PAIR COMPARISON HAS BEEN COMPUTED
	decPerNo <- decNo*perNo;
	anyOfThem <- decNo*perNo;
	allOfThem <- decNo*perNo * decNo*perNo;

	## definitive non domination
	if (length(r1$worstTh[r1$worstTh==0])==decPerNo && length(r2$worstTh[r2$worstTh==0])==decPerNo){
		return(3);										
	}
	## r1 definitively dominates r2
	if (length(r1$worstTh[r1$worstTh==0])==decPerNo && length(r2$worstTh[r2$worstTh==decPerNo])==decPerNo){
		return(1);										
	}
	## r2 definitively dominates r1
	if (length(r2$worstTh[r2$worstTh==0])==decPerNo && length(r1$worstTh[r1$worstTh==decPerNo])==decPerNo){
		return(2);										
	}
	## acceptable non domination
	if (length(r1$worstTh[r1$worstTh==0])>0 && length(r2$worstTh[r2$worstTh==0]) > 0){
		return(7);										
	}else{
	## r1 acceptably dominates r2
		if (length(r2$worstTh[r2$worstTh==0]) == 0){	## r1 is potentially dominating r2
			if(length(r1$betterTh[r1$betterTh==0]) == 0){
				return(5);					
			}else{	# length(r1$betterTh[r1$betterTh==0]) > 0
				if(length(r1$nonDomi[r1$nonDomi>0]) > 0){
					return(5);
				}
			}
		}
	## r2 acceptably dominates r1
		if (length(r1$worstTh[r1$worstTh==0]) == 0){	## r2 is potentially dominating r1
			if(length(r2$betterTh[r2$betterTh==0]) == 0){
				return(6);					
			}else{	# length(r2$betterTh[r2$betterTh==0]) > 0
				if(length(r2$nonDomi[r2$nonDomi>0]) > 0){
					return(6);
				}
			}
		}
	}
	## otherwise
	return(9);
}

##
 #  reg: List of dec matrix (perNo,criNo) dominance comparison
 ####################################################################
evaluate <- function(uneList,evalMeth,simNo)
{
	if(simNo>=850){
		browser();
	}
	for (reg in 1:uneList$itemNo){
		uneList$regEva[[reg]]$selCri <- array(0,dim=c(2,2));
	}

	##### remember selCri selection
	#  -------------
	# | min1 | min2 |
	# |------|------|
	# | max3 | max4 |
	#  -------------
	# min1 : def worstThan + acc worstThan
	# min2 : def worstThan
	# max3 : undecidable no
	# max4 : def betterThan + acc betterThan
	#####
	for (reg in 1:uneList$itemNo){
		switch(	evalMeth,
			# mean evaluation
			{	uneList$regEva[[reg]]$selCri[1,1] <- eval_mean(uneList$regEva[[reg]],1);
				uneList$regEva[[reg]]$selCri[1,2] <- eval_minMean(uneList$regEva[[reg]],1);	# why not!
			},
			# minMean evaluation
			{	uneList$regEva[[reg]]$selCri[1,1] <- eval_minMean(uneList$regEva[[reg]],1);
				uneList$regEva[[reg]]$selCri[1,2] <- eval_maxMean(uneList$regEva[[reg]],1);	# why not!
			},
			# maxMean evaluation
			{	uneList$regEva[[reg]]$selCri[1,1] <- eval_maxMean(uneList$regEva[[reg]],1);
				uneList$regEva[[reg]]$selCri[1,2] <- eval_mean(uneList$regEva[[reg]],1);	# why not!
			},
			{},
			## multicriteria evaluation
			{	if(reg<uneList$itemNo){
				for (r in (reg+1):uneList$itemNo){
					switch(	regVSreg <- groupDomi_regVSreg(uneList$regEva[[reg]],uneList$regEva[[r]]),
						# reg definitively dominates r
						{	uneList$regEva[[r]]$selCri[1,1] <- uneList$regEva[[r]]$selCri[1,1] +1;
							uneList$regEva[[r]]$selCri[1,2] <- uneList$regEva[[r]]$selCri[1,2] +1;
							uneList$regEva[[reg]]$selCri[2,2] <- uneList$regEva[[reg]]$selCri[2,2] +1;
						},
						# r definitively dominates reg
						{	uneList$regEva[[reg]]$selCri[1,1] <- uneList$regEva[[reg]]$selCri[1,1] +1;
							uneList$regEva[[reg]]$selCri[1,2] <- uneList$regEva[[reg]]$selCri[1,2] +1;
							uneList$regEva[[r]]$selCri[2,2] <- uneList$regEva[[r]]$selCri[2,2] +1;
						},
						{}, # any interests to optimize that?
						{},
						# reg acceptably dominates r
						{	uneList$regEva[[r]]$selCri[1,1] <- uneList$regEva[[r]]$selCri[1,1] +1;
							uneList$regEva[[reg]]$selCri[2,2] <- uneList$regEva[[reg]]$selCri[2,2] +1;
						},
						# r acceptably dominates reg
						{	uneList$regEva[[reg]]$selCri[1,1] <- uneList$regEva[[reg]]$selCri[1,1] +1;
							uneList$regEva[[r]]$selCri[2,2] <- uneList$regEva[[r]]$selCri[2,2] +1;
						},
						{}, # any interests to optimize that?
						{},
						# undecidable: might be good to cut it down before maximizing the no of dominated ones
						{	uneList$regEva[[reg]]$selCri[2,1] <- uneList$regEva[[reg]]$selCri[2,1] +1;
							uneList$regEva[[r]]$selCri[2,1] <- uneList$regEva[[r]]$selCri[2,1] +1;
						}
					);
				}}
			}
		);
	}

return(uneList);
}
