#setwd("~/Box Sync/Iowa State/Research/Robustness of Random Forest/Fall 17/Official_Simulation/All Final Code/")
setwd("/work/STAT/ajsage")
source("Robustness_Functions.R")

library(randomForest)
library(quantregForest)
library(abind)

########################################################################################################
#First simulation from Roy-Larocque
#m=.2

#p=0
print("p=0")
set.seed(10112017)
DATA <- GenerateDatasets(m=.2, p=0, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m20p0 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m20p0")
save(DATA, Sim1m20p0, file="Sim1m20p0Res.Rdata")  

#p=0.05
print("p=0.05")
set.seed(10112017)
DATA <- GenerateDatasets(m=.2, p=0.05, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m20p05 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m20p05")
save(DATA, Sim1m20p05, file="Sim1m20p05Res.Rdata")  

#p=0.1
print("p=0.1")
set.seed(10112017)
DATA <- GenerateDatasets(m=.2, p=0.1, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m20p10 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m20p10")
save(DATA, Sim1m20p10, file="Sim1m20p10Res.Rdata")  

#p=0.15
print("p=0.15")
set.seed(10112017)
DATA <- GenerateDatasets(m=.2, p=0.15, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m20p15 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m20p15")
save(DATA, Sim1m20p15, file="Sim1m20p15Res.Rdata")  

#p=0.2
print("p=0.2")
set.seed(10112017)
DATA <- GenerateDatasets(m=.2, p=0.2, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m20p20 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m20p20")
save(DATA, Sim1m20p20, file="Sim1m20p20Res.Rdata")  

#p=0.25
print("p=0.25")
set.seed(10112017)
DATA <- GenerateDatasets(m=.2, p=0.25, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m20p25 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m20p25")
save(DATA, Sim1m20p25, file="Sim1m20p25Res.Rdata")  

########################################################################################################
#First simulation from Roy-Larocque
#m=.4

#p=0
print("p=0")
set.seed(10112017)
DATA <- GenerateDatasets(m=.4, p=0, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m40p0 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m40p0")
save(DATA, Sim1m40p0, file="Sim1m40p0Res.Rdata")  

#p=0.05
print("p=0.05")
set.seed(10112017)
DATA <- GenerateDatasets(m=.4, p=0.05, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m40p05 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m40p05")
save(DATA, Sim1m40p05, file="Sim1m40p05Res.Rdata")  

#p=0.1
print("p=0.1")
set.seed(10112017)
DATA <- GenerateDatasets(m=.4, p=0.1, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m40p10 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m40p10")
save(DATA, Sim1m40p10, file="Sim1m40p10Res.Rdata")  

#p=0.15
print("p=0.15")
set.seed(10112017)
DATA <- GenerateDatasets(m=.4, p=0.15, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m40p15 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m40p15")
save(DATA, Sim1m40p15, file="Sim1m40p15Res.Rdata")  

#p=0.2
print("p=0.2")
set.seed(10112017)
DATA <- GenerateDatasets(m=.4, p=0.2, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m40p20 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m40p20")
save(DATA, Sim1m40p20, file="Sim1m40p20Res.Rdata")  

#p=0.25
print("p=0.25")
set.seed(10112017)
DATA <- GenerateDatasets(m=.4, p=0.25, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m40p25 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m40p25")
save(DATA, Sim1m40p25, file="Sim1m40p25Res.Rdata")  

#########################################################################################################
#First simulation from Roy-Larocque
#m=.6

#p=0
print("p=0")
set.seed(10112017)
DATA <- GenerateDatasets(m=.6, p=0, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m60p0 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m60p0")
save(DATA, Sim1m60p0, file="Sim1m60p0Res.Rdata")  

#p=0.05
print("p=0.05")
set.seed(10112017)
DATA <- GenerateDatasets(m=.6, p=0.05, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m60p05 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m60p05")
save(DATA, Sim1m60p05, file="Sim1m60p05Res.Rdata")  

#p=0.1
print("p=0.1")
set.seed(10112017)
DATA <- GenerateDatasets(m=.6, p=0.1, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m60p10 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m60p10")
save(DATA, Sim1m60p10, file="Sim1m60p10Res.Rdata")  

#p=0.15
print("p=0.15")
set.seed(10112017)
DATA <- GenerateDatasets(m=.6, p=0.15, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m60p15 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m60p15")
save(DATA, Sim1m60p15, file="Sim1m60p15Res.Rdata")  

#p=0.2
print("p=0.2")
set.seed(10112017)
DATA <- GenerateDatasets(m=.6, p=0.2, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m60p20 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m60p20")
save(DATA, Sim1m60p20, file="Sim1m60p20Res.Rdata")  

#p=0.25
print("p=0.25")
set.seed(10112017)
DATA <- GenerateDatasets(m=.6, p=0.25, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m60p25 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m60p25")
save(DATA, Sim1m60p25, file="Sim1m60p25Res.Rdata")  

#########################################################################################################
#First simulation from Roy-Larocque
#m=.8

#p=0
print("p=0")
set.seed(10112017)
DATA <- GenerateDatasets(m=.8, p=0, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m80p0 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m80p0")
save(DATA, Sim1m80p0, file="Sim1m80p0Res.Rdata")  

#p=0.05
print("p=0.05")
set.seed(10112017)
DATA <- GenerateDatasets(m=.8, p=0.05, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m80p05 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m80p05")
save(DATA, Sim1m80p05, file="Sim1m80p05Res.Rdata")  

#p=0.1
print("p=0.1")
set.seed(10112017)
DATA <- GenerateDatasets(m=.8, p=0.1, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m80p10 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m80p10")
save(DATA, Sim1m80p10, file="Sim1m80p10Res.Rdata")  

#p=0.15
print("p=0.15")
set.seed(10112017)
DATA <- GenerateDatasets(m=.8, p=0.15, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=1)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim1m80p15 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim1m80p15")
save(DATA, Sim1m80p15, file="Sim1m80p15Res.Rdata")  

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

###########################################################################################################################################
###########################################################################################################################################

#Second simulation from Roy-Larocque
#m=.15

#p=0
print("p=0")
set.seed(10112017)
DATA <- GenerateDatasets(m=.15, p=0, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m15p0 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m15p0")
save(DATA, Sim2m15p0, file="Sim2m15p0Res.Rdata")  

#p=0.05
print("p=0.05")
set.seed(10112017)
DATA <- GenerateDatasets(m=.15, p=0.05, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m15p05 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m15p05")
save(DATA, Sim2m15p05, file="Sim2m15p05Res.Rdata")  

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

#p=0.2
print("p=0.2")
set.seed(10112017)
DATA <- GenerateDatasets(m=.15, p=0.2, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m15p20 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m15p20")
save(DATA, Sim2m15p20, file="Sim2m15p20Res.Rdata")  

#p=0.25
print("p=0.25")
set.seed(10112017)
DATA <- GenerateDatasets(m=.15, p=0.25, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m15p25 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m15p25")
save(DATA, Sim2m15p25, file="Sim2m15p25Res.Rdata")  

#########################################################################################################################################

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

#p=0.1
print("p=0.1")
set.seed(10112017)
DATA <- GenerateDatasets(m=.30, p=0.1, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m30p10 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m30p10")
save(DATA, Sim2m30p10, file="Sim2m30p10Res.Rdata")  

#p=0.15
print("p=0.15")
set.seed(10112017)
DATA <- GenerateDatasets(m=.30, p=0.15, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m30p15 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m30p15")
save(DATA, Sim2m30p15, file="Sim2m30p15Res.Rdata")  

#p=0.2
print("p=0.2")
set.seed(10112017)
DATA <- GenerateDatasets(m=.30, p=0.2, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m30p20 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m30p20")
save(DATA, Sim2m30p20, file="Sim2m30p20Res.Rdata")  

#p=0.25
print("p=0.25")
set.seed(10112017)
DATA <- GenerateDatasets(m=.30, p=0.25, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m30p25 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m30p25")
save(DATA, Sim2m30p25, file="Sim2m30p25Res.Rdata")  

#########################################################################################################################################
#Second simulation from Roy-Larocque
#m=.45

#p=0
print("p=0")
set.seed(10112017)
DATA <- GenerateDatasets(m=.45, p=0, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m45p0 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m45p0")
save(DATA, Sim2m45p0, file="Sim2m45p0Res.Rdata")  

#p=0.05
print("p=0.05")
set.seed(10112017)
DATA <- GenerateDatasets(m=.45, p=0.05, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m45p05 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m45p05")
save(DATA, Sim2m45p05, file="Sim2m45p05Res.Rdata")  

#p=0.1
print("p=0.1")
set.seed(10112017)
DATA <- GenerateDatasets(m=.45, p=0.1, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m45p10 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m45p10")
save(DATA, Sim2m45p10, file="Sim2m45p10Res.Rdata")  

#p=0.15
print("p=0.15")
set.seed(10112017)
DATA <- GenerateDatasets(m=.45, p=0.15, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m45p15 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m45p15")
save(DATA, Sim2m45p15, file="Sim2m45p15Res.Rdata")  

#p=0.2
print("p=0.2")
set.seed(10112017)
DATA <- GenerateDatasets(m=.45, p=0.2, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m45p20 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m45p20")
save(DATA, Sim2m45p20, file="Sim2m45p20Res.Rdata")  

#p=0.25
print("p=0.25")
set.seed(10112017)
DATA <- GenerateDatasets(m=.45, p=0.25, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m45p25 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m45p25")
save(DATA, Sim2m45p25, file="Sim2m45p25Res.Rdata")  

##########################################################################################################################################
#Second simulation from Roy-Larocque
#m=.6

#p=0
print("p=0")
set.seed(10112017)
DATA <- GenerateDatasets(m=.6, p=0, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m60p0 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m60p0")
save(DATA, Sim2m60p0, file="Sim2m60p0Res.Rdata")  

#p=0.05
print("p=0.05")
set.seed(10112017)
DATA <- GenerateDatasets(m=.60, p=0.05, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m60p05 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m60p05")
save(DATA, Sim2m60p05, file="Sim2m60p05Res.Rdata")  

#p=0.1
print("p=0.1")
set.seed(10112017)
DATA <- GenerateDatasets(m=.60, p=0.1, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m60p10 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m60p10")
save(DATA, Sim2m60p10, file="Sim2m60p10Res.Rdata")  

#p=0.15
print("p=0.15")
set.seed(10112017)
DATA <- GenerateDatasets(m=.60, p=0.15, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m60p15 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m60p15")
save(DATA, Sim2m60p15, file="Sim2m60p15Res.Rdata")  

#p=0.2
print("p=0.2")
set.seed(10112017)
DATA <- GenerateDatasets(m=.60, p=0.2, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m60p20 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m60p20")
save(DATA, Sim2m60p20, file="Sim2m60p20Res.Rdata")  

#p=0.25
print("p=0.25")
set.seed(10112017)
DATA <- GenerateDatasets(m=.60, p=0.25, nreps=500, ntrain=500, ntest=1000, type="Var", DGP=2)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Sim2m60p25 <- RobustPreds(DATA, ntrees=500, ndsize=15, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Sim2m60p25")
save(DATA, Sim2m60p25, file="Sim2m60p25Res.Rdata")  

#######################################################################################################################################
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

#p=0.05
print("p=0.05")
set.seed(10112017)
DATA <- GenerateDatasetsLi(p=0.05, nreps=500, ntrain=1000, ntest=1000,Vartype="Id")
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Li1p05 <- RobustPreds(DATA, ntrees=500, ndsize=10, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Li1p05")
save(DATA, Li1p05, file="Li1p05Res.Rdata")  

#p=0.1
print("p=0.1")
set.seed(10112017)
DATA <- GenerateDatasetsLi(p=0.1, nreps=500, ntrain=1000, ntest=1000,Vartype="Id")
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Li1p10 <- RobustPreds(DATA, ntrees=500, ndsize=10, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Li1p10")
save(DATA, Li1p10, file="Li1p10Res.Rdata")  

#p=0.15
print("p=0.15")
set.seed(10112017)
DATA <- GenerateDatasetsLi(p=0.15, nreps=500, ntrain=1000, ntest=1000,Vartype="Id")
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Li1p15 <- RobustPreds(DATA, ntrees=500, ndsize=10, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Li1p15")
save(DATA, Li1p15, file="Li1p15Res.Rdata")  

#p=0.2
print("p=0.2")
set.seed(10112017)
DATA <- GenerateDatasetsLi(p=0.2, nreps=500, ntrain=1000, ntest=1000,Vartype="Id")
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Li1p20 <- RobustPreds(DATA, ntrees=500, ndsize=10, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Li1p20")
save(DATA, Li1p20, file="Li1p20Res.Rdata")  

#p=0.25
print("p=0.25")
set.seed(10112017)
DATA <- GenerateDatasetsLi(p=0.25, nreps=500, ntrain=1000, ntest=1000,Vartype="Id")
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Li1p25 <- RobustPreds(DATA, ntrees=500, ndsize=10, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Li1p25")
save(DATA, Li1p25, file="Li1p25Res.Rdata")  


#######################################################################################################################################
#######################################################################################################################################
#Second Simulation from Li-Martin

#p=0
print("p=0")
set.seed(10112017)
DATA <- GenerateDatasetsLi(p=0, nreps=500, ntrain=1000, ntest=1000,Vartype="Toeplitz")
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Li2p0 <- RobustPreds(DATA, ntrees=500, ndsize=10, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Li2p0")
save(DATA, Li2p0, file="Li2p0Res.Rdata")  

#p=0.05
print("p=0.05")
set.seed(10112017)
DATA <- GenerateDatasetsLi(p=0.05, nreps=500, ntrain=1000, ntest=1000,Vartype="Toeplitz")
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Li2p05 <- RobustPreds(DATA, ntrees=500, ndsize=10, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Li2p05")
save(DATA, Li2p05, file="Li2p05Res.Rdata")  

#p=0.1
print("p=0.1")
set.seed(10112017)
DATA <- GenerateDatasetsLi(p=0.1, nreps=500, ntrain=1000, ntest=1000,Vartype="Toeplitz")
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Li2p10 <- RobustPreds(DATA, ntrees=500, ndsize=10, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Li2p10")
save(DATA, Li2p10, file="Li2p10Res.Rdata")  

#p=0.15
print("p=0.15")
set.seed(10112017)
DATA <- GenerateDatasetsLi(p=0.15, nreps=500, ntrain=1000, ntest=1000,Vartype="Toeplitz")
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Li2p15 <- RobustPreds(DATA, ntrees=500, ndsize=10, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Li2p15")
save(DATA, Li2p15, file="Li2p15Res.Rdata")  

#p=0.2
print("p=.2")
set.seed(10112017)
DATA <- GenerateDatasetsLi(p=0.2, nreps=500, ntrain=1000, ntest=1000,Vartype="Toeplitz")
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Li2p20 <- RobustPreds(DATA, ntrees=500, ndsize=10, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Li2p20")
save(DATA, Li2p20, file="Li2p20Res.Rdata")  

#p=0.25
print("p=0.25")
set.seed(10112017)
DATA <- GenerateDatasetsLi(p=0.25, nreps=500, ntrain=1000, ntest=1000,Vartype="Toeplitz")
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
set.seed(10112017)
Li2p25 <- RobustPreds(DATA, ntrees=500, ndsize=10, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=5, Filename="Li2p25")
save(DATA, Li2p25, file="Li2p25Res.Rdata")  
