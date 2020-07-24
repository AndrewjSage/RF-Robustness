
library(randomForestSRC)
library(quantregForest)
library(splitstackshape)
library(reshape2)
library(tidyverse)
library(abind)
source("https://raw.githubusercontent.com/AndrewjSage/RFLOWESS/master/R/RobustPreds.R")
source("https://raw.githubusercontent.com/AndrewjSage/RFLOWESS/master/R/GenerateData.R")
source("https://raw.githubusercontent.com/AndrewjSage/RFLOWESS/master/R/Tuning.R")

DATA <- generate_RLdata(ntrain=1000, ntest=1000, p=0.25, m=0.2, type="Var", DGP=1)
TRAIN <- data.frame(DATA[[1]]) #pull out test and training set for a given rep
TEST1 <- data.frame(DATA[[2]]) #pull out test and training set for a given rep
OutlierInd <- DATA[[3]] #pull out test and training set for a given rep
nPreds <- 16 #consider 15 different types of predictions
Preds <- array(NA, dim=c(nrow(TEST1), nPreds))  #Matrix to store predictions
niter <- rep(NA, 7) #Create length 7 vector to store number of iterations for each type of RFLowess prediction
TRAINY <-TRAIN[,ncol(TRAIN)]

#Time ordinary RF growing tree
ptm <- proc.time()
RF <- randomForestSRC::rfsrc(Y~., data=TRAIN, ntree=1000, nodesize=7, forest.wt="oob", membership = T)
RFTime <- proc.time() - ptm

#Time QRF
ptm <- proc.time()
QRF <- quantregForest::quantregForest(x=TRAIN[,-ncol(TRAIN)], y=TRAIN[,ncol(TRAIN)], ntree=1000, nodesize=7, keep.forest=TRUE, keep.inbag=TRUE)
QRFTime <- proc.time() - ptm

#Extract node info
RFpredInfo <- predict(RF, newdata=TEST1, forest.wt=TRUE, membership = T)
TrainNodes <- RF$membership
TestNodes1 <- RFpredInfo$membership
Inbag <- RF$inbag
OOBWeights <- RF$forest.wt
PredWeights1 <- RFpredInfo$forest.wt


Calc_Times<- function(testsize){

TEST <- TEST1[1:testsize, ]
PredWeights <- PredWeights1[1:testsize, ]
TestNodes <- TestNodes1[1:testsize, ]

Times <- rep(NA, 7)
Times[1] <- RFTime  
Times[3] <- QRFTime

#Time ordinary RF prediction
ptm <- proc.time()
RFpred <- RFpredInfo$predicted
Times[2] <- proc.time() - ptm


#Time QRF prediction
ptm <- proc.time()
QRFPred <- predict(QRF, newdata=TEST[,-ncol(TEST)], what=0.5)
Times[4] <- proc.time() - ptm


#time LM-Huber
ptm <- proc.time()
LMPred <- LiPred(OOBWeights, PredWeights, TRAINY, method="Huber", delta=0.005,   tol=10^-6, maxiter=100)[[1]]
Times[5] <- proc.time() - ptm

#Time RF-LOWESS (untuned)
ptm <- proc.time()
Lpred <- LOWESSPred(OOBWeights, PredWeights, TRAINY, tol=10^-6, alpha=6, method="Tukey")
Times[6] <- proc.time() - ptm

ntrees <-1000; ntreestune=100; ndsize=7; parvec=c(1000, 100, seq(from=1, to=30, by=0.25)); cvreps=1;  cvfolds=5; tol=10^-6
#Time RF-LOWESS (tuned)
ptm <- proc.time()
Res <- TuneMultifoldCV(TRAIN, TEST, OOBWeights, PredWeights, OutlierInd, ntrees, ntreestune, ndsize, parvec, cvreps, cvfolds, tol=10^-6)
CVERR <- Res[[1]]
niter[1] <- Lpred[[3]]
Lpred <- LOWESSPred(OOBWeights, PredWeights, TRAINY, tol=10^-6, alpha=parvec[which.min(CVERR[3,1,])], method="Tukey")
Times[7] <- proc.time() - ptm

return(Times)
}

set.seed(07232020)
TimesRes <- matrix(NA, nrow=3, ncol=7)
TimesRes[1,] <- Calc_Times(testsize = 10)
TimesRes[2,] <- Calc_Times(testsize = 100)
TimesRes[3,] <- Calc_Times(testsize = 1000)
