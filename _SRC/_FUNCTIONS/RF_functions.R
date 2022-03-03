# obsolete
#### functions used during RFGLS modelling ####

# drawFromECDF <- function(pdf,N=10000){
  # y<-approx(cumsum(pdf$y)/sum(pdf$y),pdf$x, runif(N))$y
  # out<-data.frame(y=y,x=1:N)
  # out<-out[complete.cases(out),]
  # return(out)
# }

# splitTrainValidation<-function(data,pTrain=.7,columns=NULL,clean=F){
  # print(paste0('keeping ',pTrain*100,'% of data as training data'))
  
  # if (is.null(columns)){columns<-names(data)}
  
  # train <- sample(nrow(data), pTrain*nrow(data), replace = FALSE)
  # TrainSet <- data[train,columns]
  # ValidSet <- data[-train,columns]
  
  # if (clean){
    # TrainSet<-TrainSet[complete.cases(TrainSet),]
    # ValidSet<-TrainSet[complete.cases(ValidSet),]
  # }
  # return (list(TrainSet = TrainSet,ValidSet = ValidSet))
# }

# bootstrapInd<-function(probs,classDefinition, N=1000,response=RESPONSES[3]){
  # for (r in 1:nrow(probs)){
    # clusters<-names(classDefinition)
    # for (cls in clusters){
      # if(cls=='none'){
        # q<-rep(0,5)
      # }else{
        # ind<-classDefinition[[cls]]$ind
        # samples<-rpois(N,lambda=median(ind))
        # q<-quantile(samples)
      # }
      # probs[[paste0(cls,'_q25')]][r]<-q[2]
      # probs[[paste0(cls,'_q50')]][r]<-q[3]
      # probs[[paste0(cls,'_q75')]][r]<-q[4]
    # }
    
    # Names<-paste(response,c('Q25','Q50','Q75'),sep='_')
    # probs[[Names[1]]][r]<-sum(probs[r,clusters]*probs[r,paste0(clusters,'_q25')])
    # probs[[Names[2]]][r]<-sum(probs[r,clusters]*probs[r,paste0(clusters,'_q50')])
    # probs[[Names[3]]][r]<-sum(probs[r,clusters]*probs[r,paste0(clusters,'_q75')])
  # }
  # return(probs)
# }
