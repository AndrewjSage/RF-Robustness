#setwd("~/Box Sync/Iowa State/Research/Robustness of Random Forest/Fall 17/Official_Simulation/All Final Code/")
setwd("/work/STAT/ajsage")
source("Robustness_Functions.R")

library(randomForest)
library(quantregForest)
library(abind)

#Second simulation from Roy-Larocque
#m=.3

#p=0
print("p=0")
set.seed(10112017)
DATA <- GenerateDatasets(m=.3, p=0, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m30p0 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m30p0")
save(DATA, Sim2m30p0, file="Sim2m30p0Res.Rdata")  

#p=0.05
print("p=0.05")
set.seed(10112017)
DATA <- GenerateDatasets(m=.3, p=0.05, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m30p05 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m30p05")
save(DATA, Sim2m30p05, file="Sim2m30p05Res.Rdata")  
