##
 # FILE multiobjective.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
 ####################################################################
 # multiobjective domiance rules definition
 ####################################################################

##
 # Pareto dominance comparison of criteria vectors v1 and v2
 ####################################################################
 # > inputs
 # v1 is a criteria vector
 # v2 is a criteria vector
 # min=TRUE (default) for a minimization, min=FALSE for a maximization
 ####################################################################
 # > outputs
 # 1: v1 is Pareto dominating v2
 # 2: v2 is Pareto dominating v1
 # 3: v1 and v2 are Pareto non dominated
 ####################################################################
paretoDomi_v1VSv2 <- function(v1,v2,min=TRUE)
{
	if( length(v1)!=length(v2) ){
		print("v1 and v2 are not the same length where they should be");
		print("have a look by yourself (quit with 'Q')");
		browser();
	}

	if(min){
		v1_atLeastAsGood_no <- length(v2)-(v2_strictlyDominating_no <- length(v2[v2<v1]));
		v2_atLeastAsGood_no <- length(v1)-(v1_strictlyDominating_no <- length(v1[v1<v2]));
	}else{
		v1_atLeastAsGood_no <- length(v2)-(v2_strictlyDominating_no <- length(v2[v2>v1]));
		v2_atLeastAsGood_no <- length(v1)-(v1_strictlyDominating_no <- length(v1[v1>v2]));
	}

	if( v1_atLeastAsGood_no==length(v1) && v1_strictlyDominating_no>0 ){
		return(1);		# v1 is Pareto Dominating
	}else{
		if( v2_atLeastAsGood_no==length(v2) && v2_strictlyDominating_no>0 ){
			return(2);	# v2 is Pareto Dominating
		}else{
			return(3);	# v1 and v2 are Pareto non Dominated
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
	decNo_r1 <- reg1$itemNo;
	decNo_r2 <- reg2$itemNo;
	perNo <- dim(reg1$decEva[[1]])[1];
	criNo <- dim(reg1$decEva[[1]])[2];
	decPerNo_r1 <- (decNo_r1*perNo)-1;	# because in between 10 elements there is 10-1 links
	decPerNo_r2 <- (decNo_r2*perNo)-1;	# because in between 10 elements there is 10-1 links
	
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

	decPer_r1 <- array(0,dim=c(decNo_r1,perNo));
	decPer_r2 <- array(0,dim=c(decNo_r2,perNo));
	r1 <- list("worstTh"=decPer_r1,"betterTh"=decPer_r1,"nonDomi"=decPer_r1);
	r2 <- list("worstTh"=decPer_r2,"betterTh"=decPer_r2,"nonDomi"=decPer_r2);

	for (d1 in 1:decNo_r1){
		for (p1 in 1:perNo){
#			dp1_worstTh <- 0;
#			dp1_betterTh <- 0;
#			dp1_nonDom <- 0;
			for (d2 in 1:decNo_r2){
				for (p2 in 1:perNo){
					##### MULTICRITERIA DECPER COMPARISON
					switch(regVSreg <- paretoDomi_v1VSv2(reg1$decEva[[d1]][p1,1:criNo],reg2$decEva[[d2]][p2,1:criNo]),
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
#			if( dp1_worstTh == decPerNo_r1){
#				r1_dpWorstThanAllExists <- 1;
#			}
#			if( dp1_betterTh == decPerNo_r1){
#				r1_dpBetterThanAllExists <- 1;
#				r1domi$accNonDominated <- 0;
#			}
#			if( dp1_nonDom == decPerNo_r1){
#				r1_dpNonDomExists <- 1;
#			}
		}	
	}

#print(r1domi);
#print(r2domi);
#print("earlier elimination rules are useless");

	##### HERE EVERY DECPER PAIR COMPARISON HAS BEEN COMPUTED
	## definitive non domination
	if (length(r1$worstTh[r1$worstTh==0])==decPerNo_r1 && length(r2$worstTh[r2$worstTh==0])==decPerNo_r2){
		return(3);										
	}
	## r1 definitively dominates r2
	if (length(r1$worstTh[r1$worstTh==0])==decPerNo_r1 && length(r2$worstTh[r2$worstTh==decPerNo_r2])==decPerNo_r2){
		return(1);										
	}
	## r2 definitively dominates r1
	if (length(r2$worstTh[r2$worstTh==0])==decPerNo_r2 && length(r1$worstTh[r1$worstTh==decPerNo_r1])==decPerNo_r1){
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
groupDomi_regIntern <- function(reg)
{
	## as soon as decEva is non NULL it has to be a list
	## otherwise perNo and criNo are going to be NULL
	decNo <- reg$itemNo;
	perNo <- dim(reg$decEva[[1]])[1];
	criNo <- dim(reg$decEva[[1]])[2];
	decPerNo <- (decNo*perNo)-1; # because in between 10 elements there is 10-1 links
	
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
					switch(paretoDomi_v1VSv2(reg$decEva[[d1]][p1,1:criNo],reg$decEva[[d2]][p2,1:criNo]),
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
