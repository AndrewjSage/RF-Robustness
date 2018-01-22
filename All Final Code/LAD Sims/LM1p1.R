#setwd("~/Box Sync/Iowa State/Research/Robustness of Random Forest/Fall 17/Official_Simulation/All Final Code/")
setwd("/work/STAT/ajsage")
source("Robustness_Functions.R")

library(randomForest)
library(quantregForest)
library(abind)

########################################################################################################
#######################################################################################################################################
#First Simulation from Li-Martin

#p=0
print("p=0")
set.seed(10112017)
DATA <- GenerateDatasetsLi(p=0, nreps=500, ntrain=1000, ntest=1000,Vartype="Id")
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Li1p0 <- RobustPreds(DATA, ntrees=500, ndsize=10, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Li1p0")
save(DATA, Li1p0, file="Li1p0Res.Rdata")  
