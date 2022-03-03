standardError<-function(vector, ...){
  return(sd(vector, na.rm=T) / sqrt(length(vector[!is.na(vector)])))
}

getChunks<-function(inVector,chunksize){
  splits<-split(inVector, ceiling(seq_along(inVector)/chunksize))
  return(splits)
}

q25<-function(x){
  return(quantile(x,probs=.25, na.rm=T))
}

q75<-function(x){
  return(quantile(x,probs=.75, na.rm=T))
}

estimateNormal<-function(q, alpha=c(.25,.75)) {
  beta <- qnorm(alpha)
  out<-solve(cbind(1, beta), q)
  out<-list(mu=out[1],sigma=out[2])
  return(out)
}

drawFromNormal<-function(q25,q75,N=1000,...){
  if(q25 == q75 || abs(q75-q25)<1E-4){
    samples<-rep(q25,N)
    ECDF<-function(x){return(q25)}
    return(list(samples=samples,ECDF=ECDF))
  }
  t<-estimateNormal(q=c(q25,q75),...)
  samples<-rnorm(N,t$mu,t$sigma)
  ECDF <- ecdf(samples)
  return(list(samples=samples,ECDF=ECDF))
}

scale_manual<-function(x,centers=0,scale=1){
  x<-x-centers
  return(x/scale)
}

range01 <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

inv_range01 <- function(scaled_x,max_x,min_x=0){
  return( scaled_x*(max_x-min_x) + min_x )
}

estimate_from_five_point<-function(a,q1,m,q3,b,n){
  # equation 13 from Wan, X., Wang, W., Liu, J. et al. Estimating the sample mean and standard deviation from the sample size, median, range and/or interquartile range. BMC Med Res Methodol 14, 135 (2014). https://doi.org/10.1186/1471-2288-14-135
  N_parameter<-(n-0.375)/(n+0.25)
  N_parameter2<-(0.75*n-0.125)/(n+0.25)
  out<-(b-a)/(4*qnorm(N_parameter))+(q3-q1)/(4*qnorm(N_parameter2))
  return(out)
}

loadSettings<-function(NAME){
	srcfiles<-list.files(path=SCRIPTDIR,pattern=paste0(NAME,'_.*.R'),full.names=T,recursive=T)
	for (f in srcfiles)
	{
		print(paste0('Loading: ',basename(f)))
		source(f)
	}
}

ma <- function(x, n = 5){
  vars<-names(x)
  x<-as.data.frame(stats::filter(x, rep(1 / n, n), sides = 2))
  names(x)<-vars
  return(x[complete.cases(x),])
}