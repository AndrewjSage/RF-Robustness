setwd("/work/STAT/ajsage")
#setwd("~/Box Sync/Iowa State/Research/Robustness of Random Forest/Fall 17/Official_Simulation/Final Simulations/All Final Code")
source("Robustness_Functions.R")
#setwd("~/Box Sync/Iowa State/Research/Robustness of Random Forest/Fall 17/Official_Simulation/Real Data/Final Real Data Test")


library(randomForest)
library(quantregForest)
library(abind)

########################################################################################################

library(openintro)
download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData")
DATA <- ames[,3:ncol(ames)]
DATA$SalePrice <- DATA$SalePrice/1000

FixNAs=function(x){
if(is.integer(x)|is.numeric(x)){
  x[is.na(x)] <- 0} else{
  x <- as.character(x)  
  x[is.na(x)]="NA"
  }
  return(x)
}

DATA <- as.data.frame(lapply(DATA, FixNAs))

library(randomForest)
library(quantregForest)
library(abind)

set.seed(10112017)
parvec <- c(1000,100,seq(from=1, to=30, by=0.25))

#uncontaminated
set.seed(02042017)  #Important to keep seed same for all files, so we're dealing with same datasets
DATA <- DivideRealData(DATA, nreps=30, nfolds=10, p=0)
set.seed(10112017)
AmesRes <- RobustPreds(DATA, ntrees=500, ndsize=10, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=9, Filename="AmesResults.Rdata")
save(DATA, AmesRes, file="AmesRes.Rdata")  


load("ames.RData")
DATA <- ames[,3:ncol(ames)]
DATA$SalePrice <- DATA$SalePrice/1000

DATA <- as.data.frame(lapply(DATA, FixNAs))




#contaminated
set.seed(02042017)  #Important to keep seed same for all files, so we're dealing with same datasets
DATA <- DivideRealData(DATA, nreps=30, nfolds=10, p=0.2)
set.seed(10112017)
AmesRes <- RobustPreds(DATA, ntrees=500, ndsize=10, nPreds=15,ntreestune=100, parvec=parvec, cvreps=1, cvfolds=9, Filename="AmesResultscont.Rdata")
save(DATA, AmesRes, file="AmesRescont.Rdata")  
