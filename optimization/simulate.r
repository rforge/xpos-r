##
 # FILE simulateDebFct.r
 # AUTHOR olivier crespo
 # https://r-forge.r-project.org/projects/xpos-r/
 # simulation interface of models
 ####################################################################
 # > REFERENCES
 # K. Deb, Multi-Objective Genetic Algorithms: Problem Difficulties and Construction of Test Problems, Evolutionary Computation, vol. 7(3), pages 205-230, 1999
 # E. Zitzler, K. Deb, L. Thiele, Comparison of Multiobjective Evolutionary Algorithms: Empirical Results, Evolutionary Computation, vol. 8(2), pages 173-195, 2000
 ####################################################################

##
 # 4 mathematical models based on deb test functions.
 # the uncertainty is facked by changing f2 according to 'per' within the max variation allowed
 ####################################################################
 # > INPUT:
 # mod: 	integer (1:multifrontal; 2:discontinuous; 3:nonConvex; 4:convexNonConvex)
 # dec: 	decision definition vector
 # per:	perturbation
 # criNo:	number of criteria
 # > OUTPUT:
 # final: vector of evaluations of 'dec' subject to 'per'
 ####################################################################
 # > WARNING:
 # The uncertainty is subjectively facked for exercise purposes
 # If your point is to test the deb functions, remove it!!
 ####################################################################
simulateDeb <- function(mod,dec,per)
{
	#### uncertainty max variation amplitude allowed
	switch(mod,
		maxVar <- 10/5,	# f2 max = 10
		maxVar <- 2/5,	# f2 max = 2
		maxVar <- 4/5,	# f2 max = 4
		maxVar <- 4/5	# f2 max = 4
	);

	##### x1, x2
	x1 <- dec[1];
	x2 <- dec[2];

	##### f(x1)
	switch(mod,
		f <- x1,
		f <- x1,
		f <- 4*x1,
		f <- 4*x1
	);	

	##### g(x2)
	switch(mod,
		g <- 2-exp(-((x2-0.2)/0.004)^2)-0.8*exp(-((x2-0.6)/0.4)^2),
		g <- 1+10*x2,
		ifelse (x2<=0.4, g<-4-3*exp(-((x2-0.2)/0.02)^2 ), g<-4-2*exp( -((x2-0.7)/0.2)^2 )),
		ifelse (x2<=0.4, g<-4-3*exp(-((x2-0.2)/0.02)^2 ), g<-4-2*exp( -((x2-0.7)/0.2)^2 ))
	);

	##### required coefficients
	switch(mod,
		a<-NA,
		{a<-2; b<-4;},
		{a<-4; b<-1;},
		b<-1
	);

	##### h(f1,g)
	switch(mod,
		h <- 1/f,
		h <- 1-((f/g)^a )-(sin(2*pi*b*f)*f/g),
		ifelse(f<=b*g, h<-1-((f/b/g)^a), h<-0),
		ifelse (f<=b*g, h<-1-((f/b/g)^(0.25+(3.75*(g-1)/(2-1)))), h<-0)
	);

	##### f1(x1,x2) = x1 = f(x1)
	f1 <- f;

	##### f2(x1,x2) = g(x2)*h(f,g)
	f2 <- g*h;

	##### fake the uncertainty
	## has to be removed for proper multicriteria deb tests
	f2 <- f2 + (per-per/2)*maxVar;

	##### final
	final <- array(c(f1,f2),2);

return(final);
}

##
 # Simulate Model
 ####################################################################
simulateModel <- function(mod,apsimSpec,regEva,perNo,criNo)
{
	simNo <- 0;
	
	## criNo has to be 2 for mathematical models
	if ((mod==1 || mod==2 || mod==3 || mod==4) && criNo!=2) {stop("criNo unvalid for mathModel simulation (see simulate.r)");}

	## only to ease comprehension
	decNo <- regEva$itemNo;
	varNo <- dim(regEva$regDef)[2];

	staSimAtDec <- 1;
	if(!is.null(regEva$decEva)){						# if not the first evaluation of the region
		while (!is.na(regEva$decEva[[staSimAtDec]][1,1])){		# do not resimulate
			staSimAtDec <- staSimAtDec+1;
			if(staSimAtDec>decNo){					# break if reach the end
				break;
			}
		}
	}

	## main loop
	if(staSimAtDec <= decNo){
		for (d in staSimAtDec:decNo){
print(paste("   #           dec ",d," in ",decNo,sep=""));
			####	remove previously created files
			while(file.exists(paste(apsimSpec$path2out,"noYearFile.sim",sep=""))){
				try(file.remove(paste(apsimSpec$path2out,"noYearFile.sim",sep="")),silent=TRUE);
			}

			# new 'per' sequence everytime
			per <- runif(perNo);	#array(0,dim=perNo);
		
			# Deb test functions
			if (mod==1 || mod==2 || mod==3 || mod==4){
				temp <- NULL;
				for (p in 1:perNo){
					# no paralelism
					# temp[p,1:criNo] <- simulateDeb(mod,regEva$decDef[[d]],per[p]);
					tmp <- simulateDeb(mod,regEva$decDef[[d]],per[p]);
				}
				temp <- rbind(temp,tmp);
			}
			# APSIM
			if(mod==10){
				# trying to paralelise
				temp <- simulateApsim(apsimSpec,regEva$decDef[[d]],per,criNo);
			}	
		
			simNo <- simNo + perNo;
			regEva$decEva[[d]] <- temp;
		}
	}

return(list("eva"=regEva,"simNo"=simNo));
}
