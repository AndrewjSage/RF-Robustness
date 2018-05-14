setwd("/work/STAT/ajsage")
library(RFLOWESS)
library(parallel)

#Apply iteratively using parSapply

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)
clusterEvalQ(cl, {
  library(RFLOWESS)
})

clusterSetRNGStream(cl, 03142018)
RL1 <- parSapply(cl=cl, X=1:5, simplify="array", FUN=function(i){ApplyAcross_m_and_p(Sim = "RL", ntrain=100, ntest=100, p=c(0,.1,.2), m=c(.2,.4), contamination="Var", Vartype=NA, DGP=2, ntrees=500, ndsize=15, ntreestune=100, parvec=c(1000, 100, seq(from=1, to=30, by=0.25)), cvreps=1, cvfolds=5, tol=10^-6)})
stopCluster(cl)

save(RL1, file="RL1.Rdata")


#Apply iteratively using parSapply

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)
clusterEvalQ(cl, {
  library(RFLOWESS)
})

clusterSetRNGStream(cl, 03142018)
LM2 <- parSapply(cl=cl, X=1:3, simplify="array", FUN=function(i){ApplyAcross_m_and_p(Sim = "LM", ntrain=100, ntest=100, p=c(0, 0.05, 0.1), m=c(1), contamination="Var", Vartype="Toeplitz", DGP=2, ntrees=1000, ndsize=10, ntreestune=100, parvec=c(1000, 100, seq(from=1, to=30, by=0.25)), cvreps=1, cvfolds=5, tol=10^-6)})
stopCluster(cl)

save(LM2, file="LM2.Rdata")

