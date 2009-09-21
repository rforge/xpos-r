##
 # FILE debTestFct.r
 # AUTHOR olivier crespo
 # DATE july 2009 - july 2009, 3
 # Deb test functions used as mathematical model 
 ####################################################################
 # > REFERENCES
 # K. Deb, Multi-Objective Genetic Algorithms: Problem Difficulties and Construction of Test Problems, Evolutionary Computation, vol. 7(3), pages 205-230, 1999
 # E. Zitzler, K. Deb, L. Thiele, Comparison of Multiobjective Evolutionary Algorithms: Empirical Results, Evolutionary Computation, vol. 8(2), pages 173-195, 2000
 ####################################################################
 # > INPUT:
 # itemNo: sample number (itemNo=10000 produces a nice graph)
 # > FUNCTION CALLS:
 # multiFront_mat <- multiFrontal(itemNo);
 # discont_mat <- discontinuous(itemNo);
 # nonConvex_mat <- nonConvex(itemNo);
 # convNonConv_mat <- convexNonConvex(itemNo);
 # > VISUALISATION IN R:
 # plot(multiFront_mat,pch=".",xlim=c(0,1),ylim=c(0,10));
 # plot(discont_mat,pch=".",xlim=c(0,1),ylim=c(-0.5,2));
 # plot(nonConvex_mat,pch=".",xlim=c(0,4),ylim=c(0,4));
 # plot(convNonConv_mat,pch=".",xlim=c(0,4),ylim=c(0,4));
 ####################################################################

##
 # MULTIPLE CONVEX FRONTS
 ####################################################################
multiFrontal <- function(itemNo)
{
	##### x1, x2
	x1 <- as.matrix(runif(itemNo)); # ]0..1] instead?
	x2 <- as.matrix(runif(itemNo));

	##### f(x1)
	f <- array(NA,dim=c(itemNo));
	for (i in 1:itemNo){
	  f[i]=x1[i];
	}
	##### g(x2)
	g <- array(NA,dim=c(itemNo));
	for (i in 1:itemNo){
	  g[i]=2-exp(-((x2[i]-0.2)/0.004)^2)-0.8*exp(-((x2[i]-0.6)/0.4)^2);
	}
	##### h(f1,g)
	h <- array(NA,dim=c(itemNo));
	for (i in 1:itemNo){
	  h[i]=1/f[i];
	}
	##### f1(x1,x2) = x1 = f(x1)
	f1 <- array(NA,dim=c(itemNo));
	for (i in 1:itemNo){
	  f1[i]=x1[i];
	}	
	##### f2(x1,x2) = g(x2)*h(f,g)
	f2 <- array(NA,dim=c(itemNo));
	for (i in 1:itemNo){
	  f2[i]=g[i]*h[i];
	}

	##### final
	final <- array(NA,dim=c(itemNo,2));
	for (i in 1:itemNo){
	  final[i,1]=f1[i];
	  final[i,2]=f2[i];  
	}

return(final);
##### rm useless
rm(x1,x2);
rm(f,g,h);
rm(f1,f2,final);
}

##
 # DISCONTINUOUS FRONT
 ####################################################################
discontinuous <- function(itemNo)
{
	##### x1, x2
	x1 <- as.matrix(runif(itemNo)); # ]0..1] instead?
	x2 <- as.matrix(runif(itemNo));

	##### f1(x1,x2) = x1
	f1 <- array(NA,dim=c(itemNo));
	for (i in 1:itemNo){
		f1[i]=x1[i];
	}
	##### g(x2)
	g <- array(NA,dim=c(itemNo));
	for (i in 1:itemNo){
		g[i]=1+10*x2[i];
	}
	##### alpha (a) and q
	a<-2; q<-4;

	##### h(f1,g)
	h <- array(NA,dim=c(itemNo));
	for (i in 1:itemNo){
		h[i]=1-( (f1[i]/g[i])^a )-(sin(2*pi*q*f1[i])*f1[i]/g[i]);
	}
	##### f2(x1,x2) = g(x2)*h(f,g)
	f2 <- array(NA,dim=c(itemNo));
	for (i in 1:itemNo){
	  f2[i]=g[i]*h[i];
	}

	##### final
	final <- array(NA,dim=c(itemNo,2));
	for (i in 1:itemNo){
	  final[i,1]=f1[i];
	  final[i,2]=f2[i];  
	}

return(final);
##### rm useless
rm(x1,x2);
rm(a,q,g,h);
rm(f1,f2,final);
}

##
 # GLOBAL NON CONVEX, LOCAL NON CONVEX FRONTS
 ####################################################################
nonConvex <- function(itemNo)
{
	##### x1, x2
	x1 <- as.matrix(runif(itemNo)); # ]0..1] instead?
	x2 <- as.matrix(runif(itemNo));

	##### f1(x1,x2) = x1
	f1 <- array(NA,dim=c(itemNo));
	for (i in 1:itemNo){
		f1[i] <- 4*x1[i];
	}
	##### g(x2)
	g <- array(NA,dim=c(itemNo));
	for (i in 1:itemNo){
  		ifelse (x2[i]<=0.4, g[i]<-4-3*exp(-((x2[i]-0.2)/0.02)^2 ), g[i]<-4-2*exp( -((x2[i]-0.7)/0.2)^2 ));
	}
	##### alpha (a) and beta (b)
	a<-4; b<-1;

	##### h(f1,g)
	h <- array(NA,dim=c(itemNo));
	for (i in 1:itemNo){
		ifelse(f1[i]<=b*g[i], h[i]<-1-((f1[i]/b/g[i])^a), h[i]<-0);
	}
	##### f2(x1,x2) = g(x2)*h(f,g)
	f2 <- array(NA,dim=c(itemNo));
	for (i in 1:itemNo){
		f2[i]=g[i]*h[i];
	}

	##### final
	final <- array(NA,dim=c(itemNo,2));
	for (i in 1:itemNo){
	  final[i,1]=f1[i];
	  final[i,2]=f2[i];  
	}

return(final);
##### rm useless
rm(x1,x2);
rm(a,b,g,h);
rm(f1,f2,final);
}

##
 # GLOBAL CONVEX, LOCAL NON CONVEX FRONTS
 ####################################################################
convexNonConvex <- function(itemNo)
{
	##### x1, x2
	x1 <- as.matrix(runif(itemNo)); # ]0..1] instead?
	x2 <- as.matrix(runif(itemNo));

	##### f1(x1,x2) = x1
	f1 <- array(NA,dim=c(itemNo));
	for (i in 1:itemNo){
		f1[i]=4*x1[i];
	}
	##### g(x2)
	g <- array(NA,dim=c(itemNo));
	for (i in 1:itemNo){
  		ifelse (x2[i]<=0.4, g[i]<-4-3*exp(-((x2[i]-0.2)/0.02)^2 ), g[i]<-4-2*exp( -((x2[i]-0.7)/0.2)^2 ));
	}
	##### alpha (a) and q
	b<-1; # convex: a<=1, non-convex a>1

	##### h(f1,g)
	h <- array(NA,dim=c(itemNo));
	for (i in 1:itemNo){
		ifelse (f1[i]<=b*g[i], h[i]<-1-((f1[i]/b/g[i])^(0.25+(3.75*(g[i]-1)/(2-1)))), h[i]<-0);
	}
	##### f2(x1,x2) = g(x2)*h(f,g)
	f2 <- array(NA,dim=c(itemNo));
	for (i in 1:itemNo){
	  f2[i]=g[i]*h[i];
	}

	##### final
	final <- array(NA,dim=c(itemNo,2));
	for (i in 1:itemNo){
	  final[i,1]=f1[i];
	  final[i,2]=f2[i];  
	}

return(final);
##### rm useless
rm(x1,x2);
rm(a,q,g,h);
rm(f1,f2,final);
}
