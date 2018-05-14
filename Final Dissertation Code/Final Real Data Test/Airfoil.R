setwd("/work/STAT/ajsage")
#setwd("~/Box Sync/Iowa State/Research/Robustness of Random Forest/Fall 17/Official_Simulation/Final Simulations/All Final Code")
source("Robustness_Functions.R")
#setwd("~/Box Sync/Iowa State/Research/Robustness of Random Forest/Fall 17/Official_Simulation/Real Data/Final Real Data Test")


library(randomForest)
library(quantregForest)
library(abind)

########################################################################################################

#DATA needs to be in form of a list of dataframes 
#1-TRAINING Sets:  dims are (nreps*nfolds, ntrain, nvars)
#2-TEST Sets: (nreps*nfolds, ntest, nvars)
#3 Outlier indicators: dims are (nreps*nfolds, ntrain)

set.seed(10112017)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))
DATA <- read.csv("Airfoil_Data.csv", header=F)

#uncontaminated
set.seed(02042017)  #Important to keep seed same for all files, so we're dealing with same datasets
DATA <- DivideRealData(DATA, nreps=30, nfolds=9, p=0)
set.seed(10112017)
AirfoilRes <- RobustPreds(DATA, ntrees=500, ndsize=10, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=4, Filename="AirfoilResults.Rdata")
save(DATA, AirfoilRes, file="AirfoilRes.Rdata")  

DATA <- read.csv("Airfoil_Data.csv", header=F)

#contaminated
set.seed(02042017)  #Important to keep seed same for all files, so we're dealing with same datasets
DATA <- DivideRealData(DATA, nreps=30, nfolds=9, p=0.2)
set.seed(10112017)
AirfoilRes <- RobustPreds(DATA, ntrees=500, ndsize=10, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=4, Filename="AirfoilResultscont.Rdata")
save(DATA, AirfoilRes, file="AirfoilRescont.Rdata")  
