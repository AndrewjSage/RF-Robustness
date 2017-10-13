#setwd("~/Box Sync/Iowa State/Research/Robustness of Random Forest/Fall 17/Official_Simulation/All Final Code/")
setwd("/work/STAT/ajsage")
source("Robustness_Functions.R")

library(randomForest)
library(quantregForest)
library(abind)


#Second simulation from Roy-Larocque
#m=.15


#p=0.1
print("p=0.1")
set.seed(10112017)
DATA <- GenerateDatasets(m=.15, p=0.1, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m15p10 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m15p10")
save(DATA, Sim2m15p10, file="Sim2m15p10Res.Rdata")  

#p=0.15
print("p=0.15")
set.seed(10112017)
DATA <- GenerateDatasets(m=.15, p=0.15, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m15p15 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m15p15")
save(DATA, Sim2m15p15, file="Sim2m15p15Res.Rdata")  
