##
 # Is vector1 Pareto dominating v2
 ###############################################################################
isV1V2.paretoDominating <- function(v1,v2)
{
	if( all(v1<=v2) && any(v1<v2) ){
		return(TRUE);
	}else{
		return(FALSE);
	}
}

##
 # Is vector1 Pareto dominated by v2
 ###############################################################################
isV1V2.paretoDominated <- function(v1,v2)
{
	if( all(v1>=v2) && any(v1>v2) ){
		return(TRUE);
	}else{
		return(FALSE);
	}
}

##
 # Are vector1 and vector2 non dominated
 ###############################################################################
areV1V2.paretoNonDominated <- function(v1,v2)
{
	if( !isVec.paretoDominated(v1,v2) && !isVec.paretoDominated(v2,v1) ){
		return(TRUE);
	}else{
		return(FALSE);
	}
}

##
 # Is vector1 Pareto dominating matrix2 vectors
 ###############################################################################
isV1M2.paretoDominating <- function(v1,m2)
{
	V1isDominating <- array(NA,dim=dim(m2)[1]);
	for(v in 1:dim(m2)[1]){
		V1isDominating[v] <- isV1V2.paretoDominating(v1,m2[v,]);
	}
return(V1isDominating);
}

##
 # Is vector1 Pareto dominated by matrix2 vectors
 ###############################################################################
isV1M2.paretoDominated <- function(v1,m2)
{
	V1isDominated <- array(NA,dim=dim(m2)[1]);
	for(v in 1:dim(m2)[1]){
		V1isDominated[v] <- isV1V2.paretoDominated(v1,m2[v,]);
	}
return(V1isDominated);
}

