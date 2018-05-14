setwd("/work/STAT/ajsage")
#setwd("~/Box Sync/Iowa State/Research/Robustness of Random Forest/Fall 17/Official_Simulation/Final Simulations/All Final Code")
source("Robustness_Functions.R")
#setwd("~/Box Sync/Iowa State/Research/Robustness of Random Forest/Fall 17/Official_Simulation/Real Data/Final Real Data Test")


library(randomForest)
library(quantregForest)
library(abind)

########################################################################################################

DATA <- read.csv("Concrete_Data.csv")

library(randomForest)
library(quantregForest)
library(abind)

nreps=30
nfolds=5
set.seed(10112017)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))

#uncontaminated
set.seed(02042017)  #Important to keep seed same for all files, so we're dealing with same datasets
DATA <- DivideRealData(DATA, nreps=30, nfolds=5, p=0)
set.seed(10112017)
ConcreteRes <- RobustPreds(DATA, ntrees=500, ndsize=10, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=4, Filename="ConcreteResults.Rdata")
save(DATA, ConcreteRes, file="ConcreteRes.Rdata")  

DATA <- read.csv("Concrete_Data.csv")

#contaminated
set.seed(02042017)  #Important to keep seed same for all files, so we're dealing with same datasets
DATA <- DivideRealData(DATA, nreps=30, nfolds=5, p=0.2)
set.seed(10112017)
ConcreteRes <- RobustPreds(DATA, ntrees=500, ndsize=10, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=4, Filename="ConcreteResultscont.Rdata")
save(DATA, ConcreteRes, file="ConcreteRescont.Rdata")  
