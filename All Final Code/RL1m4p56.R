#setwd("~/Box Sync/Iowa State/Research/Robustness of Random Forest/Fall 17/Official_Simulation/All Final Code/")
setwd("/work/STAT/ajsage")
source("Robustness_Functions.R")

library(randomForest)
library(quantregForest)
library(abind)

########################################################################################################

#p=0.2
print("p=0.2")
set.seed(10112017)
DATA <- GenerateDatasets(m=.8, p=0.2, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m80p20 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m80p20")
save(DATA, Sim1m80p20, file="Sim1m80p20Res.Rdata")  

#p=0.25
print("p=0.25")
set.seed(10112017)
DATA <- GenerateDatasets(m=.8, p=0.25, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m80p25 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m80p25")
save(DATA, Sim1m80p25, file="Sim1m80p25Res.Rdata")  
