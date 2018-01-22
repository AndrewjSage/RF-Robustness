########################################################################################
setwd("~/Box Sync/Iowa State/Research/Robustness of Random Forest/Fall 17/Official_Simulation/Final Simulations/All Final Code/")
#setwd("/work/STAT/ajsage")
source("Robustness_Functions.R")

setwd("~/Box Sync/Iowa State/Research/Robustness of Random Forest/Fall 17/Official_Simulation/Final Simulations/Final Results/LS Splitting")
load("Li1p25Res.Rdata")
#1st in list is errors for each test case (1=MSPE, 2=MAPE)
#2 is OOB error for each training case (dims are 1-rep, 2-tuning par value, 3=type of residual weighting (and MSPE or MAPE)), 4=all cases, outliers, or nonoutliers
#3 Error on Test set from LOWESS using each parameter value, 1=MSPE, 2=MAPE
#4 is parameter chosen by each approach (1=rep, 2=MSPE or MAPE, and type of residual weighting), 3=outliers, nonoutliers, or all cases
#5 Best parameter choice based on test set in each rep using MSPE or MAPE
#6 Difference between chosen parameter value and best for test data
#7 error on test data (dims are 1=rep, 2=parameter chosen via all, outliers, or nonoutliers, 3=type of evaluation and weighting used when choosing parameter)

library(randomForest)

Res <- Li1p25[[1]]
MSPE <- Res[,,1][,c(2,3,5,6,7,12)]
MAPE <- Res[,,2][,c(2,3,5,6,7,12)]

MSPErank <- apply(MSPE, 1, rank)[6,]
MAPErank <- apply(MAPE, 1, rank)[6,]

qplot(MSPErank-MAPErank, main="Difference in Ranks")

#which cases are most extreme (change in rank of 4)
which(MSPErank-MAPErank < (-3))

#######################################################################################################
rep <- 422
TRAIN <- DATA[[1]][rep,,]
TEST <- DATA[[2]][rep,c(988,987),] #988 is case 1 in paper, 987 is case 2
Outlier <- DATA[[3]][rep,]
TRAINY <- TRAIN[,ncol(TRAIN)]  

#Which tuning parameter delta was used for rep 422?
TunPar <- Li1p25[[4]]
TunPar[422,,]  
TunPar[422,3,1] #This is the tuning parameter for weighting based on RF residuals on all cases with MSE criteria


ntrees=500
ndsize=10
nPreds=15
ntreestune=100
cvreps=1
cvfolds=5

#set.seed(01042018)
#RF <- randomForest(x=TRAIN[,-ncol(TRAIN)], y=TRAIN[,ncol(TRAIN)], ntree=ntrees, nodesize=ndsize, keep.forest=TRUE, keep.inbag=TRUE)
#TrainNodes <- attr(predict(RF, TRAIN[,-ncol(TRAIN)], nodes=TRUE), "nodes")  #Matrix with terminal nodes by tree (0=oob)
#TestNodes <- attr(predict(RF, TEST[,-ncol(TEST)], nodes=TRUE), "nodes")
#Inbag <- RF$inbag
#TRAINY <- TRAIN[,ncol(TRAIN)]  
#OOBWeights <- ExtractWeights_byTree(TrainNodes, TestNodes, Inbag, OOB=TRUE)
#PredWeights <- ExtractWeights_byTree(TrainNodes, TestNodes, Inbag, OOB=FALSE)
#save(OOBWeights, PredWeights, file="Weights.Rdata")
###################################################################################################

load("Weights.Rdata")

OOBPred <- OOBWeights%*%TRAINY
OOBResid <- TRAINY-OOBPred

for(i in 1:ncol(PredWeights)) {PredWeights[,i] = rev(PredWeights[,i])}  #reverse rows so case referred to as case 1 in paper is in first row.

#Get initial weights when predicting case
round(PredWeights[1,],4)
#Plot prediction weights against y-values, colour by whether from cont. dist
qplot(y=PredWeights[1,], x=TRAINY, col=factor(Outlier))

df <- data.frame(1:1000, TRAINY, scale(TRAINY), Outlier, OOBResid, PredWeights[1,], PredWeights[2,] )
names(df) <- c("Case", "Y", "Y'", "O", "Resid","w1", "w2" )
library(dplyr)
head(df %>%   arrange(desc(w1)))
head(df %>%   arrange(desc(w2)))

sum(PredWeights[1,]*TRAINY) #Prediction for first case
sum(PredWeights[2,]*TRAINY) #Prediction for second case






#LOWESS Pred
Res <- Compute_deltas(OOBWeights, TRAINY, tol=0.0001, m=12.25, method="Tukey")
d=Res[[1]]
D=matrix(rep(d, nrow(PredWeights)), nrow=nrow(PredWeights), ncol=length(TRAINY), byrow=T)
AdjWts=D*PredWeights
AdjWts=AdjWts/rowSums(AdjWts)
Pred=as.matrix(AdjWts)%*%TRAINY


df$Lw1 <- AdjWts[1,]  #LOWESS weights for case 1
df$Lw2 <- AdjWts[2,]  #LOWESS weights for case 2
head(df %>%   arrange(desc(w1)))
head(df %>%   arrange(desc(w2)))




#Li-Martin Pred
Weights=PredWeights
Pred=PredWeights%*%scale(TRAINY)
Change=1
niter=1
tol=10^-6
delta=0.005
method="Huber"
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

df$LMw1 <- AdjWts[1,]  #LOWESS weights for case 1
df$LMw2 <- AdjWts[2,]  #LOWESS weights for case 2
round(head(df %>%   arrange(desc(w1))),3) #all weight columns should add to 1
round(head(df %>%   arrange(desc(w2))),3) #all weight columns should add to 1

#Get original RF prediction for standardized response


TEST
X1<-TEST[1,1:10]
sum(X1^2)
X2<-TEST[2,1:10]
sum(X2^2)


#compute adjusted weights for cases 483 and 402 after first iteration of each algorithm
#First iteration of LOWESS pred
m=12.25
d=rep(1, length(TRAINY))
D0=matrix(rep(d, length(TRAINY)), nrow=length(TRAINY), ncol=length(TRAINY), byrow=T)
AdjWts=D0*OOBWeights+10^-9
AdjWts=AdjWts/rowSums(AdjWts)
OOBPred=as.matrix(AdjWts)%*%TRAINY
resid=TRAINY-OOBPred
resid[is.na(resid)]=mean(abs(resid), na.rm=T)   #if case does not come up OOB
d0=d
s=median(abs(resid))
t=resid/(m*s)
d=t(apply(data.frame(t), 1, Tukey)) #Tukey weights

AdjWt1 <- (PredWeights[1,]*d)/(sum(PredWeights[1,]*d))  
AdjWt2 <- (PredWeights[2,]*d)/(sum(PredWeights[2,]*d))

AdjWt1[483]  #test case 1 is the one that relies on training case 483
AdjWt2[402] #test case 2 is the one that relies on training case 402
