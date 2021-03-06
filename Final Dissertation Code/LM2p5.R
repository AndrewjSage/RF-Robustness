#setwd("~/Box Sync/Iowa State/Research/Robustness of Random Forest/Fall 17/Official_Simulation/All Final Code/")
setwd("/work/STAT/ajsage")
source("Robustness_Functions.R")

library(randomForest)
library(quantregForest)
library(abind)

########################################################################################################
#p=0.2
print("p=.2")
set.seed(10112017)
DATA <- GenerateDatasetsLi(p=0.2, nreps=500, ntrain=1000, ntest=1000,Vartype="Toeplitz")
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Li2p20 <- RobustPreds(DATA, ntrees=500, ndsize=10, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Li2p20")
save(DATA, Li2p20, file="Li2p20Res.Rdata")  
