##
 # FILE evaluation.r
 # AUTHOR olivier crespo
 # DATE july 2009 - july 2009, 13
 ####################################################################

##
 #  REGION MEAN EVALUATION  
 ####################################################################
eval_mean <- function(regEva, criterion)
{
	mulDom <- array(0,dim=c(2,2));

	regMean <- array(0,dim=dim(regEva$decEva[[1]])[2]);
	for (d in 1:regEva$itemNo){
		decMean <- apply(regEva$decEva[[d]],2,mean);
		regMean <- regMean + decMean;
	}
	regMean <- regMean / regEva$itemNo;
	mulDom[1,1] <- regMean[criterion];
	
return(mulDom);
}

##
 #  REGION MIN DECMEAN EVALUATION  (min is the best)
 ####################################################################
eval_minMean <- function(regEva, criterion)
{
	mulDom <- array(0,dim=c(2,2));

	regMinMean <- apply(regEva$decEva[[1]],2,mean);
	if(regEva$itemNo>1){
	for (d in 2:regEva$itemNo){
		decMean <- apply(regEva$decEva[[d]],2,mean);
		if(decMean[criterion]<regMinMean[criterion]){
			regMinMean <- decMean;
		}
	}}
	mulDom[1,1] <- regMinMean[criterion];
	
return(mulDom);
}

##
 #  REGION MAX DECMEAN EVALUATION  (max is the worst)
 ####################################################################
eval_maxMean <- function(regEva, criterion)
{
	mulDom <- array(0,dim=c(2,2));

	regMaxMean <- apply(regEva$decEva[[1]],2,mean);
	if(regEva$itemNo>1){
	for (d in 2:regEva$itemNo){
		decMean <- apply(regEva$decEva[[d]],2,mean);
		if(decMean[criterion]>regMinMean[criterion]){
			regMaxMean <- decMean;
		}
	}}
	mulDom[2,1] <- regMaxMean[criterion];
	
return(mulDom);
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
	criNo <- dim(decPer1)[1];
	
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
groupDomi_regVSreg <- function(reg1,reg2)
{
	decNo <- reg1$itemNo;
	perNo <- dim(reg1$decEva[[1]])[1];
	criNo <- dim(reg1$decEva[[1]])[2];
	
	#####
	# I want to avoid computing multicriteria demanding comparisons if i can
	# So I tried to eliminate alternatives as soon as possible
	# instead of computing all and finding out what domination it is
	#####
	# I start with all results possible=1 (always say r1 first, r2 last)
	initReg <- list("defDominating"=1,"defDominated"=1,"defNonDominated"=1,"accDominating"=1,"accDominated"=1,"accNonDominated"=1,"undecidable"=1);
	# and plan to 'return' as soon as the sum of either r1 or r2 is =1 
	# potentially confirm with the other region which should have reached the according result
	r1domi <- initReg;
	r2domi <- initReg;
	# I do not know if it is worth the trouble, let us see ...
	#####

	decPer <- array(0,dim=c(decNo,perNo));
	r1 <- list("worstTh"=decPer,"betterTh"=decPer,"nonDomi"=decPer);
	r2 <- list("worstTh"=decPer,"betterTh"=decPer,"nonDomi"=decPer);
	allOfThem <- decNo*perNo * decNo*perNo;
	
	for (d1 in 1:decNo){
		for (p1 in 1:perNo){
			for (d2 in 1:decNo){
				for (p2 in 1:perNo){
					##### MULTICRITERIA DECPER COMPARISON
					switch(paretoDomi_decPerVSdecPer(reg1$decEva[[d1]][p1,1:criNo],reg2$decEva[[d2]][p2,1:criNo]),
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
					if(sum(r1$worstTh)>0 || sum(r2$worstTh)>0 || sum(r1$betterTh)>0 || sum(r2$betterTh)>0){
						r1domi$defNonDominated=0;
						r2domi$defNonDominated=0;
						# everything else is possible
					}
					## not r1 definitely dominating r2 as soon as
					## not r2 definitely dominating r1 as soon as
					## not acceptably non dominating as soon as
					## not r1 acceptably dominating r2 as soon as
					## not r2 acceptably dominating r1 as soon as
	

					##### EARLIER STOPPING TESTS
					if ( (r1domi[1]+r1domi[2]+r1domi[3]+r1domi[4]+r1domi[5]+r1domi[6]+r1domi[7]) == 1){
						# first confirm with r2domi
						# then return
						if(r1domi$defDominating==1){		return(1)};
						if(r1domi$defDominated==1){		return(2)};
						if(r1domi$defNonDominating==1){	return(3)};
						if(r1domi$accDominating==1){		return(5)};
						if(r1domi$accDominated==1){		return(6)};
						if(r1domi$accNonDominating==1){	return(7)};
						if(r1domi$undecidable==1){		return(9)};
					}
				}	
			}
		}	
	}
	##### HERE EVERY DECPER PAIR COMPARISON HAS BEEN COMPUTED
	if (sum(r1$worstTh)==0 && sum(r2$worstTh)==0 && sum(r1$betterTh)==0 && sum(r2$betterTh)==0){
		return(3);	## definitive non domination
	}
	if (sum(r1$worstTh)==0 && sum(r2$worstTh)==allOfThem && sum(r1$betterTh)==allOfThem && sum(r2$betterTh)==0){
		return(1);	## r1 definitively dominates r2
	}
	if (sum(r1$worstTh)==allOfThem && sum(r2$worstTh)==0 && sum(r1$betterTh)==0 && sum(r2$betterTh)==allOfThem){
		return(2);	## r2 definitively dominates r1
	}
	if (sum(r1$worstTh) <- allOfThem && sum(r2$worstTh) < allOfThem){
		return(7);	## acceptable non domination
	}else{
		if (sum(r1$worstTh)==0 && sum(r2$worstTh)==allOfThem && sum(r1$betterTh)==allOfThem && sum(r2$betterTh)==0){
			return(5);	## r1 acceptably dominates r2
		}
		if (sum(r1$worstTh)==allOfThem && sum(r2$worstTh)==0 && sum(r1$betterTh)==0 && sum(r2$betterTh)==allOfThem){
			return(6);	## r2 acceptably dominates r1
		}
	}
	## otherwise
	return(9);
}

##
 #  MULTICRITERIA GROUP DOMINANCE EVALUATION
 ####################################################################
 # > REFERENCE:
 # O. Crespo, F. Garcia, J.E. Bergez,
 # Multiobjective optimization subject to uncertainty: application to agricultural resources management,
 # to be submitted
 ####################################################################
 # 1 if s1 is dominating s2
 # -1 if s1 is dominated by s2
 # 0 otherwise
 ####################################################################
eval_multiGroupDomi <- function()
{
	
}
