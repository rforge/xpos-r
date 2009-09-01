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
	## as soon as decEva is non NULL it has to be a list
	## otherwise perNo and criNo are going to be NULL
	decNo <- reg1$itemNo;
	perNo <- dim(reg1$decEva[[1]])[1];
	criNo <- dim(reg1$decEva[[1]])[2];
	decPerNo <- decNo*perNo;
	
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
#	initReg <- list("defDominating"=1,"defDominated"=1,"defNonDominated"=1,"accDominating"=1,"accDominated"=1,"accNonDominated"=1,"undecidable"=1);
#	r1domi <- initReg;
#	r2domi <- initReg;
#	##### to eliminate accNonDomination and undecidability
#	r1_dpWorstThanAllExists <- 0;
#	r1_dpBetterThanAllExists <- 0;
#	r1_dpNonDomExists <- 0;

	decPer <- array(0,dim=c(decNo,perNo));
	r1 <- list("worstTh"=decPer,"betterTh"=decPer,"nonDomi"=decPer);
	r2 <- list("worstTh"=decPer,"betterTh"=decPer,"nonDomi"=decPer);

	for (d1 in 1:decNo){
		for (p1 in 1:perNo){
#			dp1_worstTh <- 0;
#			dp1_betterTh <- 0;
#			dp1_nonDom <- 0;
			for (d2 in 1:decNo){
				for (p2 in 1:perNo){
					##### MULTICRITERIA DECPER COMPARISON
					switch(regVSreg <- paretoDomi_decPerVSdecPer(reg1$decEva[[d1]][p1,1:criNo],reg2$decEva[[d2]][p2,1:criNo]),
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
# WORKING ON THE			if( regVSreg !=1 ){
# REDUCTION OF THE			r1domi$defDominating=0;
# MULTICRITERIA				r2domi$defDominated=0;
# COMPARISON			}
#					if( regVSreg !=2 ){
#						r2domi$defDominating=0;
#						r1domi$defDominated=0;
#					}
#					if( regVSreg != 3){
#						r1domi$defNonDominated=0;
#						r2domi$defNonDominated=0;
#					}
#					if( regVSreg == 1){
#						dp1_betterTh <- dp1_betterTh +1;
#					}
#					if( regVSreg == 2){
#						dp1_worstTh <- dp1_worstTh +1;
#					}
#					if( regVSreg == 3){
#						dp1_nonDom <- dp1_nonDom +1;
#					}

					##### EARLIER STOPPING TESTS
#					if ( (r1domi[[1]]+r1domi[[2]]+r1domi[[3]]+r1domi[[4]]+r1domi[[5]]+r1domi[[6]]+r1domi[[7]]) == 1){
#						# confirmation with r2domi should be useless...
#						# then return
#						if(r1domi$defDominating==1){		return(1)};
#						if(r1domi$defDominated==1){		return(2)};
#						if(r1domi$defNonDominating==1){	return(3)};
#						if(r1domi$accDominating==1){		return(5)};
#						if(r1domi$accDominated==1){		return(6)};
#						if(r1domi$accNonDominating==1){	return(7)};
#						if(r1domi$undecidable==1){		return(9)};
#					}
				}	
			}
#			if( dp1_worstTh == decPerNo){
#				r1_dpWorstThanAllExists <- 1;
#			}
#			if( dp1_betterTh == decPerNo){
#				r1_dpBetterThanAllExists <- 1;
#				r1domi$accNonDominated <- 0;
#			}
#			if( dp1_nonDom == decPerNo){
#				r1_dpNonDomExists <- 1;
#			}
		}	
	}

#print(r1domi);
#print(r2domi);
#print("earlier elimination rules are useless");

	##### HERE EVERY DECPER PAIR COMPARISON HAS BEEN COMPUTED
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
 #  reg: List of dec matrix (perNo,criNo)
 ####################################################################
 # return the no of non dominated (dec,per) combinations
 ####################################################################
 #  MULTICRITERIA GROUP DOMINANCE EVALUATION
 ####################################################################
 # > REFERENCE:
 # O. Crespo, F. Garcia, J.E. Bergez,
 # Multiobjective optimization subject to uncertainty: application to agricultural resources management,
 # to be submitted
 ####################################################################
groupDomi_regIntern <- function(reg)
{
	## as soon as decEva is non NULL it has to be a list
	## otherwise perNo and criNo are going to be NULL
	decNo <- reg$itemNo;
	perNo <- dim(reg$decEva[[1]])[1];
	criNo <- dim(reg$decEva[[1]])[2];
	decPerNo <- decNo*perNo;
	
	decPer <- array(0,dim=c(decNo,perNo));
	r <- list("worstTh"=decPer,"betterTh"=decPer,"nonDomi"=decPer);

	d1 <- 0;
	repeat{	d1 <- d1+1;	if(d1>decNo) break;
		
		p1 <- 0;
		repeat{	p1 <- p1+1;	if(p1>perNo) break;
			
			ifelse(p1<perNo,d2<-d1-1,d2<-d1);
			repeat{	d2 <- d2+1;	if(d2>decNo) break;

				ifelse(p1<perNo,p2<-p1,p2<-0);
				repeat{	p2 <- p2+1;	if(p2>perNo) break;

					##### MULTICRITERIA DECPER COMPARISON
					switch(paretoDomi_decPerVSdecPer(reg$decEva[[d1]][p1,1:criNo],reg$decEva[[d2]][p2,1:criNo]),
						## this response of reg is dominating this response of reg
						{	r$betterTh[d1,p1] <- r$betterTh[d1,p1] +1;
							r$worstTh[d2,p2] <- r$worstTh[d2,p2] +1;
						},
						## this response of reg is dominating this response of reg
						{	r$worstTh[d1,p1] <- r$worstTh[d1,p1] +1;
							r$betterTh[d2,p2] <- r$betterTh[d2,p2] +1;
						},
						## these responses of reg are not dominating each other
						{	r$nonDomi[d1,p1] <- r$nonDomi[d1,p1] +1;
							r$nonDomi[d2,p2] <- r$nonDomi[d2,p2] +1;
						}
					);
				}
			}
		}
	}

return(length(r$worstTh[r$worstTh==0]));
}

##	
 # EVALUATE PROMISING REGIONS (SELCRI) FROM PROLIST
 ####################################################################
 # !!!
 # UNSIFFICIANT FOR MULTICRITERIA COMPARISON
 # you have to update comparison with the pending regions in penList
 # see evaluate_penPLUSproList and evaluate_penMINUSproList
 ####################################################################
evaluate_proList <- function(uneList,evalMeth,criterion)
{
	# probably useless... but
	if(uneList$itemNo==0){
		return(uneList);
	}

	# either do not exist or has to be reevaluated here
	for (reg in 1:uneList$itemNo){
		uneList$regEva[[reg]]$selCri <- array(0,dim=c(2,2));
	}

	for (reg in 1:uneList$itemNo){
		switch(	evalMeth,
			# mean evaluation
			{	uneList$regEva[[reg]]$selCri[1,1] <- eval_mean(uneList$regEva[[reg]],criterion);
			},
			# minMean evaluation
			{	uneList$regEva[[reg]]$selCri[1,1] <- eval_minMean(uneList$regEva[[reg]],criterion);
			},
			# maxMean evaluation
			{	uneList$regEva[[reg]]$selCri[1,1] <- eval_maxMean(uneList$regEva[[reg]],criterion);
			},
			{},
			## multicriteria evaluation
			{	## intra region
				if(reg<uneList$itemNo){
				for (r in (reg+1):uneList$itemNo){
					switch(	regVSreg <- groupDomi_regVSreg(uneList$regEva[[reg]],uneList$regEva[[r]]),
						# reg definitively dominates r
						{	uneList$regEva[[r]]$selCri[1,1] <- uneList$regEva[[r]]$selCri[1,1] +1;
							uneList$regEva[[reg]]$selCri[2,1] <- uneList$regEva[[reg]]$selCri[2,1] +1;
						},
						# r definitively dominates reg
						{	uneList$regEva[[reg]]$selCri[1,1] <- uneList$regEva[[reg]]$selCri[1,1] +1;
							uneList$regEva[[r]]$selCri[2,1] <- uneList$regEva[[r]]$selCri[2,1] +1;
						},
						{},
						{},
						# reg acceptably dominates r
						{	uneList$regEva[[r]]$selCri[1,1] <- uneList$regEva[[r]]$selCri[1,1] +1;
							uneList$regEva[[reg]]$selCri[2,1] <- uneList$regEva[[reg]]$selCri[2,1] +1;
						},
						# r acceptably dominates reg
						{	uneList$regEva[[reg]]$selCri[1,1] <- uneList$regEva[[reg]]$selCri[1,1] +1;
							uneList$regEva[[r]]$selCri[2,1] <- uneList$regEva[[r]]$selCri[2,1] +1;
						},
						{},
						{},
						{}
					);
				}}
			}
		);
	}

return(uneList);
}

##
 # UPDATE EVALUATION COMPARISON OF PROLIST REGIONS AND PENLIST REGIONS
 ####################################################################
 # > proList regions have been created and evaluate before (evalute_proList)
 # > original proList has to be removed before (evaluate_penMINUSproList)
 # > new proList (offspring) regions can now be evaluated in front of penList
 ####################################################################
evaluate_penPLUSproList <- function(proList,penList,evalMeth)
{
	# probably useless... but
	if(proList$itemNo==0 || penList$itemNo==0){
		return(list("pro"=proList,"pen"=penList));
	}

	for (reg in 1:proList$itemNo){
		switch(	evalMeth,
			# mean evaluation
			{	print("should not come through here, look at \"evaluation.r\" function evaluate_penPLUSproList!");
			},
			# minMean evaluation
			{	print("should not come through here, look at \"evaluation.r\" function evaluate_penPLUSproList!");
			},
			# maxMean evaluation
			{	print("should not come through here, look at \"evaluation.r\" function evaluate_penPLUSproList!");
			},
			{},
			## multicriteria evaluation
			{	for (r in 1:penList$itemNo){
					switch(	regVSreg <- groupDomi_regVSreg(proList$regEva[[reg]],penList$regEva[[r]]),
						# reg definitively dominates r
						{	penList$regEva[[r]]$selCri[1,1] <- penList$regEva[[r]]$selCri[1,1] +1;
							proList$regEva[[reg]]$selCri[2,1] <- proList$regEva[[reg]]$selCri[2,1] +1;
						},
						# r definitively dominates reg
						{	proList$regEva[[reg]]$selCri[1,1] <- proList$regEva[[reg]]$selCri[1,1] +1;
							penList$regEva[[r]]$selCri[2,1] <- penList$regEva[[r]]$selCri[2,1] +1;
						},
						{},
						{},
						# reg acceptably dominates r
						{	penList$regEva[[r]]$selCri[1,1] <- penList$regEva[[r]]$selCri[1,1] +1;
							proList$regEva[[reg]]$selCri[2,1] <- proList$regEva[[reg]]$selCri[2,1] +1;
						},
						# r acceptably dominates reg
						{	proList$regEva[[reg]]$selCri[1,1] <- proList$regEva[[reg]]$selCri[1,1] +1;
							penList$regEva[[r]]$selCri[2,1] <- penList$regEva[[r]]$selCri[2,1] +1;
						},
						{},
						{},
						{}
					);
				}
			}
		);
	}

return(list("pro"=proList,"pen"=penList));
}

##
 # UPDATE EVALUATION COMPARISON OF PROLIST REGIONS MINUS PENLIST REGIONS
 ####################################################################
 # > proList regions have been created and evaluate before (evalute_proList)
 # > original proList has to be removed 
 # > later new proList (offspring) regions would be evaluated (evaluate_penPLUSproList)
 ####################################################################
evaluate_penMINUSproList <- function(proList,penList,evalMeth)
{
	# probably useless... but
	if(proList$itemNo==0 || penList$itemNo==0){
		return(list("pro"=proList,"pen"=penList));
	}

	for (reg in 1:proList$itemNo){
		switch(	evalMeth,
			# mean evaluation
			{	print("should not come through here, look at \"evaluation.r\" function evaluate_penMINUSproList!");
			},
			# minMean evaluation
			{	print("should not come through here, look at \"evaluation.r\" function evaluate_penMINUSproList!");
			},
			# maxMean evaluation
			{	print("should not come through here, look at \"evaluation.r\" function evaluate_penMINUSproList!");
			},
			{},
			## multicriteria evaluation
			{	for (r in 1:penList$itemNo){
					#
					#	REMOVING PROLISTS REGION (reg) COMPARISON FROM PENLIST REGIONS (r) EVALUATIONS
					#
					switch(	regVSreg <- groupDomi_regVSreg(proList$regEva[[reg]],penList$regEva[[r]]),
						# reg definitively dominates r
						{	penList$regEva[[r]]$selCri[1,1] <- penList$regEva[[r]]$selCri[1,1] -1;
							proList$regEva[[reg]]$selCri[2,1] <- proList$regEva[[reg]]$selCri[2,1] -1;
						},
						# r definitively dominates reg
						{	proList$regEva[[reg]]$selCri[1,1] <- proList$regEva[[reg]]$selCri[1,1] -1;
							penList$regEva[[r]]$selCri[2,1] <- penList$regEva[[r]]$selCri[2,1] -1;
						},
						{},
						{},
						# reg acceptably dominates r
						{	penList$regEva[[r]]$selCri[1,1] <- penList$regEva[[r]]$selCri[1,1] -1;
							proList$regEva[[reg]]$selCri[2,1] <- proList$regEva[[reg]]$selCri[2,1] -1;
						},
						# r acceptably dominates reg
						{	proList$regEva[[reg]]$selCri[1,1] <- proList$regEva[[reg]]$selCri[1,1] -1;
							penList$regEva[[r]]$selCri[2,1] <- penList$regEva[[r]]$selCri[2,1] -1;
						},
						{},
						{},
						{}
					);
				}
			}
		);
	}

return(list("pro"=proList,"pen"=penList));
}
