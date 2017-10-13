library(dplyr)
library(reshape2)
library(MASS)
#Functions to generate data for simulations


#First DGP from Roy, Larocque. Type is kind of contamination (Var or Mean)
GenDataVar1=function(ntrain, ntest, p,m, type="Var", DGP=1){
  X=mvrnorm(n = ntrain, mu=c(rep(0,6)), Sigma=diag(6))
  ds=rbinom(ntrain,1,p)  #determine which training cases are outliers
  e=c(rep(NA, ntrain))   #initialize error vector
  e[ds==0]=rnorm(length(e[ds==0]),0,1) #non-contaminated errors
  if (type=="Var"){
  e[ds==1]=rnorm(length(e[ds==1]),0,5) #contaminated errors for variance cont.
  } else {
    e[ds==1]=rnorm(length(e[ds==1]),5,1) #contaminated errors for mean cont.
  }
  Y=m*((1*X[,1]<=0 & X[,2]<=0)+2*(X[,1]<=0 & X[,2]>0 & X[,4]<=0)+3*(X[,1]<=0 & X[,2]>0 & X[,4]>0 & X[,6]<=0)+4*(X[,1]<=0 & X[,2]>0 & X[,4]>0 & X[,6]>0)+5*(X[,1]>0 & X[,3]<=0)+6*(X[,1]>0 & X[,3]>0 & X[5]<=0) + 7*(X[,1]>0 & X[,3]>0 & X[5]>0))+e
  TRAIN=data.frame(X, Y)         #Training Set
  Xt=mvrnorm(n = ntest, mu=c(rep(0,6)), Sigma=diag(6))   #Generate predictors for test data
  et=rnorm(ntest,0,1)                 #Errors for test cases-no contamination
  Y=m*((1*Xt[,1]<=0 & Xt[,2]<=0)+2*(Xt[,1]<=0 & Xt[,2]>0 & Xt[,4]<=0)+3*(Xt[,1]<=0 & Xt[,2]>0 & Xt[,4]>0 & Xt[,6]<=0)+4*(Xt[,1]<=0 & Xt[,2]>0 & Xt[,4]>0 & Xt[,6]>0)+5*(Xt[,1]>0 & Xt[,3]<=0)+6*(Xt[,1]>0 & Xt[,3]>0 & Xt[5]<=0) + 7*(Xt[,1]>0 & Xt[,3]>0 & Xt[5]>0))+et
  TEST=data.frame(Xt, Y)
  return(list(TRAIN, TEST,ds))
}

#Generate data from Roy, Larocque DGP2, for either "Var" or "Mean" cont.
GenDataVar2=function(ntrain, ntest, p,m, type="Var"){
  X=mvrnorm(n = ntrain, mu=c(rep(0,6)), Sigma=diag(6))
  ds=rbinom(ntrain,1,p)                  #which cases are contaminated?
  e=c(rep(NA, ntrain))                   #Vector to store errors
  e[ds==0]=rnorm(length(e[ds==0]),0,1)   #non-contaminated errors
  if(type=="Var"){
    e[ds==1]=rnorm(length(e[ds==1]),0,5)   #contaminated errors
  } else{
    e[ds==1]=rnorm(length(e[ds==1]),5,1)   #contaminated errors
  }
  Y=m*(X[,1]+.707*(X[,2])^2+2*(X[,3]>0)+.873*log(abs(X[,1]))*X[,3]+0.894*X[,2]*X[,4]+2*(X[,5]>0)+0.464*exp(X[,6]))+e
  TRAIN=data.frame(X, Y)                 #training set
  Xt=mvrnorm(n = ntest, mu=c(rep(0,6)), Sigma=diag(6))   #predictors for test set
  et=rnorm(ntest,0,1)                   #Generate test errors no contamination
  Y=m*(Xt[,1]+.707*(Xt[,2])^2+2*(Xt[,3]>0)+.873*log(abs(Xt[,1]))*Xt[,3]+0.894*Xt[,2]*Xt[,4]+2*(Xt[,5]>0)+0.464*exp(Xt[,6]))+et
  TEST=data.frame(Xt, Y)
  return(list(TRAIN, TEST,ds))
}


GenerateDatasets=function(m,p,nreps, ntrain, ntest, type="Var", DGP=1){
  TrainDatasets=array(NA, dim=c( nreps, ntrain, 7 )) #Last index is for 6 exp. vars. and resp
  TestDatasets=array(NA, dim=c(nreps, ntest, 7))
  OutlierIndicators=array(NA, dim=c( nreps, ntrain))
  if(DGP==1){ #for DGP1
      for( rep in 1:nreps){
      DATA=GenDataVar1(ntrain, ntest, p,m, type=type)
      TrainDatasets[rep,,]=as.matrix(DATA[[1]])
      OutlierIndicators[ rep,]=as.matrix(DATA[[3]])
      TestDatasets[rep,,]=as.matrix(DATA[[2]])
    }
  } else{  #for DGP2
        for( rep in 1:nreps){
        DATA=GenDataVar2(ntrain, ntest, p,m, type=type)
        TrainDatasets[rep,,]=as.matrix(DATA[[1]])
        OutlierIndicators[rep,]=as.matrix(DATA[[3]])
        TestDatasets[rep,,]=as.matrix(DATA[[2]])
        }
  }
return(list(TrainDatasets, TestDatasets, OutlierIndicators))
  }

GenDataLi=function(p, rep, ntrain, ntest, Vartype="Id"){
  if (Vartype=="Id"){
  X=mvrnorm(n = ntrain, mu=c(rep(0,10)), Sigma=diag(10))
  } else {
    X=mvrnorm(n = ntrain, mu=c(rep(0,10)), Sigma=toeplitz(0.7^seq.int(0, 10-1)))#Toeplitz(.7) matrix defined in Li, Martin paper
  }
  ds=rbinom(ntrain,1,p)                  #which cases are contaminated?
  e=rnorm(ntrain,0,1)              #non-contaminated errors
  e[ds==1]=e[ds==1]+15*rt(sum(ds==1),2)   #non-contaminated errors
  Y=rowSums(X^2)+e
  TRAIN=data.frame(X, Y)                 #training set
  if (Vartype=="Id"){
    Xt=mvrnorm(n = ntest, mu=c(rep(0,10)), Sigma=diag(10))
  } else {
    Xt=mvrnorm(n = ntrain, mu=c(rep(0,10)), Sigma=toeplitz(0.7^seq.int(0, 10-1)))#Toeplitz(.7) matrix defined in Li, Martin paper
  }
  et=rnorm(ntest,0,1)                   #Generate test errors no contamination
  Yt=rowSums(Xt^2)+et
  TEST=data.frame(Xt, Yt)
  return(list(TRAIN, TEST,ds))
    }

GenerateDatasetsLi=function(p, nreps, ntrain, ntest, Vartype="Id"){
  TrainDatasets=array(NA, dim=c( nreps, ntrain, 11 )) #Last index is for 10 exp. vars. and resp
  TestDatasets=array(NA, dim=c( nreps, ntest, 11))
  OutlierIndicators=array(NA, dim=c(nreps, ntrain))
    for( rep in 1:nreps){
      DATA=GenDataLi(p,rep, ntrain, ntest, Vartype=Vartype)
      TrainDatasets[ rep,,]=as.matrix(DATA[[1]])
      OutlierIndicators[ rep,]=as.matrix(DATA[[3]])
      TestDatasets[ rep,,]=as.matrix(DATA[[2]])
    }
  return(list(TrainDatasets, TestDatasets, OutlierIndicators))
  }
  
  

###########################################################################################################
#Function to convert number of training cases in terminal nodes to weights. Used in ExtractWeights_byCase
ConvertWeights=function(x){
  return(x/sum(x, na.rm=T))
}

#Function to calculate weights of individual training cases used to predict test cases. 
#After running RF save:
#TrainNodes=attr(predict(RF, TRAIN, nodes=TRUE), "nodes")  #Matrix with terminal nodes by tree (0=oob)
#TestNodes=attr(predict(RF, TEST, nodes=TRUE), "nodes")
#Inbag=RF$inbag
#For RFSRC
#TrainNodes=RF$membership
#TestNodes=RFPred$membership
#Inbag=RF$inbag
#######################################################################################
#Extract prediction weights for output saved from RF or RFSRC object. 
#returns matrix of weights on training cases used in prediction of test cases
#This version loops through trees. Faster for large test sets.
ExtractWeights_byTree=function(TrainNodes, TestNodes, Inbag, OOB=FALSE){
  if(OOB==TRUE){  #If making OOB predictions, test set is training set
    TestNodes=TrainNodes
  }
  rownames(TrainNodes)=as.character(1:nrow(TrainNodes))
  rownames(Inbag)=as.character(1:nrow(Inbag))
  rownames(TestNodes)=as.character(1:nrow(TestNodes))
  TreeWeights=array(0, dim=c(ncol(TestNodes), nrow(TestNodes), nrow(TrainNodes))) #dims are 1-ntree, 2-ntest, 3-ntrain
  TrainM=melt(TrainNodes)  #M tells which node each training case lands in for each tree
  names(TrainM)=c("TrCase", "Tree", "Node")
  ntree=max(TrainM$Tree)
  TrainM$Inbag=melt(Inbag)[,3]
  TrainM=TrainM[TrainM$Inbag>0,]
  TestM=melt(TestNodes) #
  names(TestM)=c("TeCase", "Tree", "Node")
  for ( tree in 1:length(unique(TestM$Tree))){  #Loop through trees
    TreeTest=TestM[TestM$Tree==unique(TestM$Tree)[tree],] #test cases tree
    if(OOB==TRUE){
      TreeTest=TreeTest[Inbag[,tree]==0,]
    }
    TreeTrain=TrainM[TrainM$Tree==unique(TrainM$Tree)[tree],] #training cases in bootstrap sample used to grow tree
    NodeSummary=merge(TreeTest, TreeTrain, by="Node")
    TreeNodes=acast(NodeSummary, TeCase~TrCase, value.var="Inbag") #Test Case by Training cases in same terminal node as test case in tree 
    TreeNodes[is.na(TreeNodes)]=0
    TreeWeights[tree,as.numeric(rownames(TreeNodes)),as.numeric(colnames(TreeNodes))]=TreeNodes/rowSums(TreeNodes)
    if(OOB==TRUE){  #If doing OOB predictions, need to NA-out the cases that aren't OOB, so averages are computed correctly over trees
      TreeWeights[tree,!rownames(TrainNodes)%in%TreeTest$TeCase, ]=NA  
    }
  }
  CaseWeights=apply(TreeWeights, c(2,3), mean, na.rm=T)
  if(OOB==TRUE){
    CaseWeights[row(CaseWeights)==col(CaseWeights)]=0 
  }
  return(CaseWeights)
}

########################################################################
#Incorporate OOB predictions

ExtractWeights_byCase=function(TrainNodes, TestNodes, Inbag, OOB=FALSE){
  if(OOB==TRUE){  #If making OOB predictions, test set is training set
    TestNodes=TrainNodes
  }
  M=melt(TrainNodes)  #M tells which node each training case lands in for each tree
  names(M)=c("TrCase", "Tree", "Node")
  ntree=max(M$Tree)
  #Create a new variable that incorporates both trees and nodes. Each node gets its unique value
  #instead of being nested in tree
  #deparse(substitute(RF))
  M$Inbag=melt(Inbag)[,3]
  M=M[M$Inbag>0,]
  M$NodeKey=M$Node+M$Tree/ntree #Unique key for each node, tree combination
  TestM=melt(TestNodes) #
  names(TestM)=c("TeCase", "Tree", "Node")
  TestM$NodeKey=TestM$Node+TestM$Tree/ntree
  CaseWeights=matrix(nrow=nrow(TestNodes), ncol=nrow(TrainNodes))
  for ( i in 1:length(unique(TestM$TeCase))){
    Weights=c(rep(0, nrow(TrainNodes)))
    TestCaseM=TestM[TestM$TeCase==unique(TestM$TeCase)[i],]  #pull out info on just that test case
    if(OOB==TRUE){  #If making OOB predictions, restrict to trees where case was OOB
    TestCaseM=TestCaseM[Inbag[i,]==0,]
    }
    TrCases=M[M$NodeKey%in%TestCaseM$NodeKey,]  #just training cases that end up in a terminal node with test case
    TreeWeights=acast(TrCases, TrCase~Tree, value.var="Inbag") #Training cases in same terminal node as test case by #trees
    TreeWeights[is.na(TreeWeights)]=0
    TreeWeights=apply(TreeWeights, 2, ConvertWeights)
    TreeWeights=rowMeans(TreeWeights)
    #row names of TRAIN not necessarily in inc. order. TreeWeights are. Need to reorder TreeWeights
    Weights[rownames(TrainNodes)%in%names(TreeWeights)]=TreeWeights[rownames(subset(TrainNodes, rownames(TrainNodes)%in%names(TreeWeights)))]
    CaseWeights[i,]=Weights
  }
  return(CaseWeights)
}
#################################################################################
###Code for LOWESS Robustness algorithm

#Helper function
#Tukey Bisquare Function
Tukey=function(t){  
  return(max(c(1-(t)^2,0)))
}

Huber=function(t){  
  return(1/sqrt(1+(t)^2))
}

##########

#Helper function
#Compute Deltas using iterative procedure
#OOB weights is extracted prediction weights for OOB cases
#TRAINY is response vector for training data
#Method is either Tukey or Huber
Compute_deltas=function(OOBWeights, TRAINY, tol, m=6,method){
  d=rep(1, length(TRAINY))
  D0=matrix(rep(d, length(TRAINY)), nrow=length(TRAINY), ncol=length(TRAINY), byrow=T)
  AdjWts=D0*OOBWeights+10^-9
  AdjWts=AdjWts/rowSums(AdjWts)
  OOBPred=as.matrix(AdjWts)%*%TRAINY
  resid=TRAINY-OOBPred
  resid[is.na(resid)]=mean(abs(resid), na.rm=T)   #if case does not come up OOB
  niter=1
  Change=1
  while(Change>tol & niter<100){  
    d0=d
    s=median(abs(resid))
    t=resid/(m*s)
    if (method=="Tukey") {
      d=t(apply(data.frame(t), 1, Tukey)) #Tukey weights
    }else {
      d=t(apply(data.frame(t), 1, Huber)) #Huber weights
    }
    D0=matrix(rep(d, length(TRAINY)), nrow=length(TRAINY), ncol=length(TRAINY), byrow=T)
    AdjWts=D0*OOBWeights+10^-9 # add in very small buffer so that if all are zero we don't divide by 0
    AdjWts=AdjWts/rowSums(AdjWts)
    OOBPred0=OOBPred
    OOBPred=as.matrix(AdjWts)%*%TRAINY
    resid=TRAINY-OOBPred
    resid[is.na(resid)]=mean(abs(resid), na.rm=T)   #if case does not come up OOB
    Change=mean((OOBPred-OOBPred0)^2)
    niter=niter+1
  }
  return(list(d0, OOBPred, niter))
}

#Function to extract case weights. Specify either by tree or by case
ExtractWeights=function(TrainNodes, TestNodes, Inbag, byTree){
if(byTree==TRUE){
  OOBWeights=ExtractWeights_byTree(TrainNodes, TestNodes, Inbag, OOB=TRUE)
  Weights=ExtractWeights_byTree(TrainNodes, TestNodes, Inbag, OOB=FALSE)
}else{
  OOBWeights=ExtractWeights_byCase(TrainNodes, TestNodes, Inbag, OOB=TRUE)
  Weights=ExtractWeights_byCase(TrainNodes, TestNodes, Inbag, OOB=FALSE)
}
  return(list(OOBWeights, Weights))
}


#Function to run LOWESS straight from Training and Test Output using earlier helper
LOWESSPred=function(OOBWeights, PredWeights, TRAINY, tol=0.01, m=6,method="Tukey"){
  Res=Compute_deltas(OOBWeights, TRAINY, tol, m=m, method=method)
  d=Res[[1]]
  D=matrix(rep(d, nrow(PredWeights)), nrow=nrow(PredWeights), ncol=length(TRAINY), byrow=T)
  AdjWts=D*PredWeights
  AdjWts=AdjWts/rowSums(AdjWts)
  Pred=as.matrix(AdjWts)%*%TRAINY
  return(Pred)
}

#Predictions using Li's method with either Tukey or Huber procedure
LiPred=function(OOBWeights, PredWeights, TRAINY, tol=0.000001, delta=0.005,method="Tukey"){
  Weights=PredWeights
  Pred=PredWeights%*%scale(TRAINY)
  Change=1
  niter=1
  while (Change>tol){
    diffmat=outer(as.vector(scale(TRAINY)), as.vector(Pred), "-") #matrix of differences between Y's for training cases and predicted y's for test cases
    diffmat=diffmat/delta
    if (method=="Tukey") {
      RobWts=t(matrix(sapply(diffmat, Tukey), nrow=nrow(diffmat), ncol=ncol(diffmat)))
    }else {
      RobWts=t(matrix(sapply(diffmat, Huber), nrow=nrow(diffmat), ncol=ncol(diffmat)))
    }
    AdjWts=RobWts*Weights
    AdjWts=AdjWts/rowSums(AdjWts)
    Pred0=Pred
    Pred=as.matrix(AdjWts)%*%scale(TRAINY)
    Pred[is.na(Pred)]=Pred0[is.na(Pred)] #In case of Tukey, if all weights are 0, keep pred same
    Change=mean((scale(Pred)-scale(Pred0))^2)
    niter=niter+1
  }
  Pred=Pred*sd(TRAINY)+mean(TRAINY) #Get back to original scale
  return(Pred)
}


#####################################################################################
library(dplyr)
library(splitstackshape)


#Method to get predictions from Other methods used in paper by Roy & Larocque
Node_Tree_Agg=function(TrainNodes, TestNodes, Inbag, TRAINY){
  rownames(TrainNodes)=1:nrow(TrainNodes) #rename so predictions are in right order
  rownames(TestNodes)=1:nrow(TestNodes) #rename so predictions are in right order
  M=melt(TrainNodes)
  ntree=max(M[,2])
  M$YVal=rep(TRAINY, ntree)
  names(M)=c("TrCase", "Tree", "Node", "Yval")
  ntree=max(M$Tree)
  #Create a new variable that incorporates both trees and nodes. Each node gets its unique value
  #instead of being nested in tree
  M$Inbag=melt(Inbag)[,3]
  M=M[M$Inbag>0,]
#  M$NodeKey=M$Node+M$Tree/ntree
  M=expandRows(M, "Inbag")
  NodePreds=data.frame(M %>% 
              group_by(Tree, Node) %>% 
               summarise (Meany = mean(Yval),Medy = median(Yval) ) )
  NodePreds$NodeKey=NodePreds$Node+NodePreds$Tree/ntree
  TestM=melt(TestNodes)
  names(TestM)=c("TeCase", "Tree", "Node")
  TestM$NodeKey=TestM$Node+TestM$Tree/ntree
  MatchNodes=merge(NodePreds, TestM, key="NodeKey") #Contains mean and median response for the terminal node that each test case lands in for each tree
  MedPredMat=acast(MatchNodes, TeCase~Tree, value.var="Medy")
  MeanPredMat=acast(MatchNodes, TeCase~Tree, value.var="Meany")
  Mean_Med_Predictions=apply(MeanPredMat,1,median)       #Took mean within each node and median over trees
  Med_Med_Predictions=apply(MedPredMat,1,median)        #Took median within each node and median over trees
  Med_Mean_Predictions=apply(MedPredMat,1,mean)       #Took median within each node and mean over trees
  return(list(Mean_Med_Predictions, Med_Med_Predictions,Med_Mean_Predictions))
}

###############################################################################################
#Function for tuning our method

Partitiondata=function(ntrain, cvreps, cvfolds){
  foldsize=ntrain/cvfolds
  TrainInd=array(dim=c(cvreps, cvfolds, foldsize))  
  for (rep in 1:cvreps){
    for (fold in 1:cvfolds){
      samp=sample(1:ntrain, ntrain, replace=FALSE)
      TrainInd[rep,fold,] = samp[(foldsize*(fold-1)+1):(fold*foldsize)]
    }
  }
  return(TrainInd)  
}


TuneMultifoldCV=function(TRAIN, TEST, OOBWeights, PredWeights, OutlierInd, ntrees,ntreestune, ndsize, parvec, cvreps, cvfolds){
  TRAINY=TRAIN[,ncol(TRAIN)]
  ChosenPars=array(NA, dim=c(6,3)) #3 is for contribution of all, outliers, and non-outliers
  BestPars=array(NA, dim=c(2))
  Diff=array(NA, dim=c(2,6,3) )#second dimension is for MSE or MAPE #3 is for contribution of all, outliers, and non-outliers
  LOOBERR=array(dim=c(length(parvec), 6,3)) #6 is for 6 different eval. crit (MSE-no weights, MAPE-noweights, MSE-RF weights, MAPE-RFweights, MSE-RFLweights, MAPE-RFLweights) #3 is for all, outliers, nonoutliers
  LPREDERR=array(dim=c(length(parvec), 2)) #Just one entry for MSE and one for MAPE
  ERR=array(NA, dim=c(3,6)) #dims whether all, outliers, or nonoutliers are used, and 6 different types of downweighing
  # Get Errors for tuning by doing CV on training data. Returns errors for each parameter
  LOOBERR=CVTrain(TRAIN, cvreps, cvfolds, ntreestune, ndsize, OutlierInd)
  #Make LOWESS predicton for each possible tuning parameter
  for( ind in 1:length(parvec)){
    LPred=c(LOWESSPred(OOBWeights, PredWeights, TRAINY, tol=0.0001, m=parvec[ind],method="Tukey"))
    LPREDERR[ind,1]=mean((LPred-TEST[, ncol(TEST)])^2)  #Just do MSE or MAPE, not weighted average on Test data
    LPREDERR[ind,2]=mean(abs(LPred-TEST[, ncol(TEST)]))
  }
  ChosenPars[,1]=parvec[apply(LOOBERR[,,1], 2, which.min)] #Parameters chosen using all cases
  if(sum(is.na(LOOBERR[,,2]))==0){  #if there is contamination
    ChosenPars[,2]=parvec[apply(LOOBERR[,,2], 2, which.min)]  #Parameters chosen using only outliers 
  }
  ChosenPars[,3]=parvec[apply(LOOBERR[,,3], 2, which.min)] #Parameters chosen using nonoutliers
  #Note sum of LOOBERR[rep,,,2] and LOOBERR[rep,,,3] is LOOBERR[rep,,,1]
  BestPars=parvec[apply(LPREDERR, 2, which.min)]  #Best parameters for test data
  Diff[1,,]=ChosenPars-BestPars[1]
  Diff[2,,]=ChosenPars-BestPars[2]
  #Find prediction error corresponding to parameter choice we would have made
  #Prediction error using all cases to tune
  ERR[1,1]=LPREDERR[which.min(LOOBERR[,1,1]),1]  #first 1 is for CVtype-unweighted/MSE, second is for using all training cases, 3rd is for MSE
  ERR[1,2]=LPREDERR[which.min(LOOBERR[,2,1]),2] #first 2 is for CVtype-unweighted/MAPE, second is for using all training cases, 3rd is for MAPE
  ERR[1,3]=LPREDERR[which.min(LOOBERR[,3,1]),1] #using RF downweighting
  ERR[1,4]=LPREDERR[which.min(LOOBERR[,4,1]),2]
  ERR[1,5]=LPREDERR[which.min(LOOBERR[,5,1]),1] #using LOWESS downweighting
  ERR[1,6]=LPREDERR[which.min(LOOBERR[,6,1]),2]
  #Prediction error only outliers in TRAIN2 to tune-not sure why we'd want to do this
  if(sum(is.na(LOOBERR[,,2]))==0){
    ERR[2,1]=LPREDERR[which.min(LOOBERR[,1,2]),1]  #first 1 is for CVtype-unweighted/MSE, second is for using all training cases, 3rd is for MSE
    ERR[2,2]=LPREDERR[which.min(LOOBERR[,2,2]),2] #first 2 is for CVtype-unweighted/MAPE, second is for using all training cases, 3rd is for MAPE
    ERR[2,3]=LPREDERR[which.min(LOOBERR[,3,2]),1]
    ERR[2,4]=LPREDERR[which.min(LOOBERR[,4,2]),2]
    ERR[2,5]=LPREDERR[which.min(LOOBERR[,5,2]),1]
    ERR[2,6]=LPREDERR[which.min(LOOBERR[,6,2]),2]
  }
  #Prediction error using only nonoutliers  in TRAIN2 to tune
  ERR[3,1]=LPREDERR[which.min(LOOBERR[,1,3]),1]  #first 1 is for CVtype-unweighted/MSE, second is for using all training cases, 3rd is for MSE
  ERR[3,2]=LPREDERR[which.min(LOOBERR[,2,3]),2] #first 2 is for CVtype-unweighted/MAPE, second is for using all training cases, 3rd is for MAPE
  ERR[3,3]=LPREDERR[which.min(LOOBERR[,3,3]),1]
  ERR[3,4]=LPREDERR[which.min(LOOBERR[,4,3]),2]
  ERR[3,5]=LPREDERR[which.min(LOOBERR[,5,3]),1]
  ERR[3,6]=LPREDERR[which.min(LOOBERR[,6,3]),2]
  return(list(LOOBERR, LPREDERR, ChosenPars, BestPars, Diff, ERR ))
}


CVTrain=function(TRAIN, cvreps, cvfolds, ntreestune, ndsize, OutlierInd){
  TrainInd=Partitiondata(ntrain=nrow(TRAIN), cvreps=cvreps, cvfolds=cvfolds)  
  CVERR=array(dim=c(cvreps, cvfolds, length(parvec), 6, 3))
  for(r in 1: cvreps){
    for (fold in 1:cvfolds){
      samp=TrainInd[r,fold,] #samp is a vector with indices of test cases
      TRAINY=TRAIN[,ncol(TRAIN)]
      TRAIN1=TRAIN[-samp,]   #Subset to grow forest on in CV
      TRAIN2=TRAIN[samp,]  #Subset to test on for CV
      #Grow RF on just TRAIN1 subset
      RF1=randomForest(x=TRAIN1[,-ncol(TRAIN1)], y=TRAIN1[,ncol(TRAIN1)], ntree=ntreestune, nodesize=ndsize, keep.forest=TRUE, keep.inbag=TRUE)
      TrainNodes1=attr(predict(RF1, TRAIN1[,-ncol(TRAIN1)], nodes=TRUE), "nodes")  #Matrix with terminal nodes by tree (0=oob)
      TestNodes1=attr(predict(RF1, TRAIN2[,-ncol(TRAIN2)], nodes=TRUE), "nodes")
      Inbag1=RF1$inbag
      OOBWeights1=ExtractWeights_byTree(TrainNodes1, TestNodes1, Inbag1, OOB=TRUE)
      PredWeights1=ExtractWeights_byTree(TrainNodes1, TestNodes1, Inbag1, OOB=FALSE)
      #For TRAIN2, determine how to downweight outliers effect on error for purpose of CV
      RF2=randomForest(x=TRAIN2[,-ncol(TRAIN2)], y=TRAIN2[,ncol(TRAIN2)], ntree=ntreestune, nodesize=ndsize, keep.forest=TRUE, keep.inbag=TRUE)
      TrainNodes2=attr(predict(RF2, TRAIN2[,-ncol(TRAIN2)], nodes=TRUE), "nodes")  #Matrix with terminal nodes by tree (0=oob)
      Inbag2=RF2$inbag
      OOBWeights2=ExtractWeights_byTree(TrainNodes2, TrainNodes2, Inbag2, OOB=TRUE)
      #RF and RF-LOWESS OOB predictions for cases in TRAIN2 based on cases in TRAIN2
      TRAIN2RFPred=predict(RF2, OOB=TRUE)
      TRAIN2RFLPred=c(LOWESSPred(OOBWeights2, OOBWeights2, TRAINY[samp], tol=0.0001, m=6,method="Tukey"))
      #Determine how to downweight for purpose of CV
      RFTRAIN2Residsc=(TRAINY[samp]-TRAIN2RFPred)/(6*median(abs((TRAINY[samp]-TRAIN2RFPred))))  #This is the only place where we use the Y's for TRAIN2. To downweight likely outliers
      BisqwtRF=c(apply(data.frame(RFTRAIN2Residsc), 1, Tukey))
      RFLTRAIN2Residsc=(TRAINY[samp]-TRAIN2RFLPred)/(6*median(abs((TRAINY[samp]-TRAIN2RFLPred))))  #This is the only place where we use the Y's for TRAIN2. To downweight likely outliers
      BisqwtRFL=apply(data.frame(RFLTRAIN2Residsc), 1, Tukey)
      LOOBPreds=rep(NA, length(parvec))
      for( ind in 1:length(parvec)){
        #RF-LOWESS predictions for cases in TRAIN2 based on cases in TRAIN1 for different m's nore PredWeights1 are weights from TRAIN1 for predictions on TRAIN2
        LCVPred=c(LOWESSPred(OOBWeights1, PredWeights1, TRAINY[-samp], tol=0.0001, m=parvec[ind],method="Tukey"))
        #Compute test case predictions for each value of m (tuning parameter)
        #Evaluate on only TRAIN2  i.e. samp
        CVERR[r,fold,ind,1, 1]=mean((LCVPred-TRAIN[samp, ncol(TRAIN)])^2)   #MSE without downweighting outliers in CV error
        CVERR[r,fold,ind,2, 1]=mean(abs(LCVPred-TRAIN[samp, ncol(TRAIN)]))  #MAPE without downweighting outliers in CV error
        CVERR[r,fold,ind,3, 1]=mean((LCVPred-TRAIN[samp, ncol(TRAIN)])^2*BisqwtRF)  #MSE downweighting outliers according to BisqwtRF
        CVERR[r,fold,ind,4, 1]=mean(abs(LCVPred-TRAIN[samp, ncol(TRAIN)])*BisqwtRF) #MAPE downweighting outliers according to BisqwtRF
        CVERR[r,fold,ind,5, 1]=mean((LCVPred-TRAIN[samp, ncol(TRAIN)])^2*BisqwtRFL) #MSE downweighting outliers according to BisqwtRFL
        CVERR[r,fold,ind,6, 1]=mean(abs(LCVPred-TRAIN[samp, ncol(TRAIN)])*BisqwtRFL) #MAPE downweighting outliers according to BisqwtRFL
        #error on only outlier cases
        if(sum(OutlierInd[samp])>0){
          CVERR[r,fold,ind,1, 2]=mean(((LCVPred-TRAIN[samp, ncol(TRAIN)])^2)[OutlierInd[samp]==1]) 
          CVERR[r,fold,ind,2, 2]=mean(abs(LCVPred-TRAIN[samp, ncol(TRAIN)])[OutlierInd[samp]==1])
          CVERR[r,fold,ind,3, 2]=mean(((LCVPred-TRAIN[samp, ncol(TRAIN)])^2*(BisqwtRF))[OutlierInd[samp]==1])
          CVERR[r,fold,ind,4, 2]=mean((abs(LCVPred-TRAIN[samp, ncol(TRAIN)])*(BisqwtRF))[OutlierInd[samp]==1])
          CVERR[r,fold,ind,5, 2]=mean(((LCVPred-TRAIN[samp, ncol(TRAIN)])^2*(BisqwtRFL))[OutlierInd[samp]==1])
          CVERR[r,fold,ind,6, 2]=mean((abs(LCVPred-TRAIN[samp, ncol(TRAIN)])*(BisqwtRFL))[OutlierInd[samp]==1])
        }
        #error on only non-outliers
        CVERR[r,fold,ind,1, 3]=mean(((LCVPred-TRAIN[samp, ncol(TRAIN)])^2)[OutlierInd[samp]==0]) 
        CVERR[r,fold,ind,2, 3]=mean(abs(LCVPred-TRAIN[samp, ncol(TRAIN)])[OutlierInd[samp]==0])
        CVERR[r,fold,ind,3, 3]=mean(((LCVPred-TRAIN[samp, ncol(TRAIN)])^2*(BisqwtRF))[OutlierInd[samp]==0])
        CVERR[r,fold,ind,4, 3]=mean((abs(LCVPred-TRAIN[samp, ncol(TRAIN)])*(BisqwtRF))[OutlierInd[samp]==0])
        CVERR[r,fold,ind,5, 3]=mean(((LCVPred-TRAIN[samp, ncol(TRAIN)])^2*(BisqwtRFL))[OutlierInd[samp]==0])
        CVERR[r,fold,ind,6, 3]=mean(abs((LCVPred-TRAIN[samp, ncol(TRAIN)])*(BisqwtRFL))[OutlierInd[samp]==0])
      }
    }
  }
  LOOBERR=apply(CVERR, c(3,4,5), mean)
  return(LOOBERR)
}     

########################################################################################################

#Function that makes predictions for simulation with a given number of reps
RobustPreds=function(DATA, ntrees=500, ndsize=5, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename){
  ntrain <- dim(DATA[[1]])[2]
  ntest <- dim(DATA[[2]])[2]
  nreps <- dim(DATA[[1]])[1]
  #store Overall error for all methods
  ERRMat <- array(NA, dim=c(nreps,nPreds, 2))  #2 is for MSE or MAPE
  
  #Store Results for our method with all the tuning info
  ChosenPars=array(NA, dim=c(nreps,6,3)) #3 is for contribution of all, outliers, and non-outliers
  BestPars=array(NA, dim=c(nreps,2))
  Diff=array(NA, dim=c(nreps,2,6,3) )#second dimension is for MSE or MAPE #3 is for contribution of all, outliers, and non-outliers
  LOOBERR=array(dim=c(nreps, length(parvec), 6,3)) #6 is for 6 different eval. crit (MSE-no weights, MAPE-noweights, MSE-RF weights, MAPE-RFweights, MSE-RFLweights, MAPE-RFLweights) #3 is for all, outliers, nonoutliers
  LPREDERR=array(dim=c(nreps, length(parvec), 2)) #Just one entry for MSE and one for MAPE
  ERR=array(NA, dim=c(nreps, 3, 6))
  
  for (rep in 1:nreps){
    TRAIN <- DATA[[1]][rep,,] #pull out test and training set for a given rep
    TEST <- DATA[[2]][rep,,] #pull out test and training set for a given rep
    OutlierInd <- DATA[[3]][rep,] #pull out test and training set for a given rep
    Preds <- array(NA, dim=c(nrow(TEST), nPreds))  #Matrix to store predictions
    RF <- randomForest(x=TRAIN[,-ncol(TRAIN)], y=TRAIN[,ncol(TRAIN)], ntree=ntrees, nodesize=ndsize, keep.forest=TRUE, keep.inbag=TRUE)
    TrainNodes <- attr(predict(RF, TRAIN[,-ncol(TRAIN)], nodes=TRUE), "nodes")  #Matrix with terminal nodes by tree (0=oob)
    TestNodes <- attr(predict(RF, TEST[,-ncol(TRAIN)], nodes=TRUE), "nodes")
    Inbag <- RF$inbag
    TRAINY <- TRAIN[,ncol(TRAIN)]  
    OOBWeights <- ExtractWeights_byTree(TrainNodes, TestNodes, Inbag, OOB=TRUE)
    PredWeights <- ExtractWeights_byTree(TrainNodes, TestNodes, Inbag, OOB=FALSE)
    QRF <- quantregForest(x=TRAIN[,-ncol(TRAIN)], y=TRAIN[,ncol(TRAIN)], ntree=ntrees, nodesize=ndsize, keep.forest=TRUE, keep.inbag=TRUE)
    Preds[,1] <- mean(TRAIN[,ncol(TRAIN)])
    Preds[,2] <- predict(RF,newdata=TEST[,-ncol(TEST)])
    Preds[,3] <- predict(QRF, newdata=TEST[,-ncol(TEST)], what=0.5)
    #Other methods
    Preds[,4] <- LiPred(OOBWeights, PredWeights, TRAINY, tol=0.000001, delta=0.8,method="Tukey")
    Preds[,5] <- LiPred(OOBWeights, PredWeights, TRAINY, tol=0.000001, delta=0.005,method="Huber")
    MedPreds <- Node_Tree_Agg(TrainNodes, TestNodes, Inbag, TRAINY)
    Preds[,6:8] <- as.matrix(data.frame(MedPreds[[1]],MedPreds[[2]],MedPreds[[3]]))
    #Tune and run our method. Get results for all tuning parameters and breakdown of performance on outliers/nonoutliers, and each type of tuning weighting
    Res <- TuneMultifoldCV(TRAIN, TEST, OOBWeights, PredWeights, OutlierInd, ntrees,ntreestune, ndsize, parvec, cvreps, cvfolds)
    CVERR <- Res[[1]]
    #using default tuning par of 6
    Preds[,9] <- LOWESSPred(OOBWeights, PredWeights, TRAINY, tol=0.0001, m=6,method="Tukey")
    #using unweighted CV with MSE
    Preds[,10] <- LOWESSPred(OOBWeights, PredWeights, TRAINY, tol=0.0001, m=parvec[which.min(CVERR[,1,1])],method="Tukey")
    #using unweighted CV with MAPE
    Preds[,11] <- LOWESSPred(OOBWeights, PredWeights, TRAINY, tol=0.0001, m=parvec[which.min(CVERR[,2,1])],method="Tukey")
    #using weighted CV by RF resid with MSE
    Preds[,12] <- LOWESSPred(OOBWeights, PredWeights, TRAINY, tol=0.0001, m=parvec[which.min(CVERR[,3,1])],method="Tukey")
    #using weighted CV by RF resid with MAPE
    Preds[,13] <- LOWESSPred(OOBWeights, PredWeights, TRAINY, tol=0.0001, m=parvec[which.min(CVERR[,4,1])],method="Tukey")
    #using weighted CV by LOWESS resid with MSE
    Preds[,14] <- LOWESSPred(OOBWeights, PredWeights, TRAINY, tol=0.0001, m=parvec[which.min(CVERR[,5,1])],method="Tukey")
    #using weighted CV by LOWESS resid with MAPE
    Preds[,15] <- LOWESSPred(OOBWeights, PredWeights, TRAINY, tol=0.0001, m=parvec[which.min(CVERR[,6,1])],method="Tukey")
    ERRMat[rep,,1] <- apply(Preds, 2, function(x){mean((x-TEST[,ncol(TEST)])^2)})
    ERRMat[rep,,2] <- apply(Preds, 2, function(x){mean(abs(x-TEST[,ncol(TEST)]))})
    LOOBERR[rep,,,] <- as.matrix(Res[[1]])
    LPREDERR[rep,,] <- as.matrix(Res[[2]])
    ChosenPars[rep,,] <- as.matrix(Res[[3]])
    BestPars[rep,] <- as.matrix(Res[[4]])
    Diff[rep,,,] <- as.matrix(Res[[5]])
    ERR[rep,,] <- as.matrix(Res[[6]])
    print(rep)
    save(ERRMat, LOOBERR, LPREDERR, ChosenPars, BestPars, Diff, ERR, file=paste(Filename, ".Rdata", sep=""))
  }
  return(list(ERRMat, LOOBERR, LPREDERR, ChosenPars, BestPars, Diff, ERR))
}

