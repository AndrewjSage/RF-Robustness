#setwd("/work/STAT/ajsage")
#library(RFLOWESS)
library(parallel)

#Apply iteratively using parSapply

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)
clusterEvalQ(cl, {
  #library(RFLOWESS)
  source("Large_Data_Sim.R")
  library(abind)
  library(dplyr)
  library(magrittr)
  library(MASS)
  library(quantregForest) 
  library(randomForestSRC)
  library(reshape2)
  library(splitstackshape)
})

clusterSetRNGStream(cl, 03142018)
RL2 <- parSapply(cl=cl, X=1:500, simplify="array", FUN=function(i){ApplyAcross_m_and_p(Sim = "RL", ntrain=1000, ntest=1000, p=c(0, 0.05, 0.1, 0.15, 0.2, 0.25), m=c(0.15, 0.3, 0.45, 0.6), contamination="Var", Vartype=NA, DGP=2, ntrees=1000, ndsize=7, ntreestune=100, parvec=c(1000, 100, seq(from=1, to=30, by=0.25)), cvreps=1, cvfolds=5, tol=10^-6)})
#RL2 <- parSapply(cl=cl, X=1:2, simplify="array", FUN=function(i){ApplyAcross_m_and_p(Sim = "RL", ntrain=100, ntest=100, p=c(0, 0.15, 0.25), m=c(0.15, 0.3), contamination="Var", Vartype=NA, DGP=2, ntrees=1000, ndsize=7, ntreestune=100, parvec=c(1000, 100, seq(from=1, to=30, by=0.25)), cvreps=1, cvfolds=5, tol=10^-6)})
stopCluster(cl)

save(RL2, file="RL2_Large.Rdata")
