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
RL1 <- parSapply(cl=cl, X=1:500, simplify="array", FUN=function(i){ApplyAcross_m_and_p(Sim = "RL", ntrain=1000, ntest=1000, p=c(0, 0.05, 0.1, 0.15, 0.2, 0.25), m=c(0.2, 0.4, 0.6, 0.8), contamination="Var", Vartype=NA, DGP=1, ntrees=1000, ndsize=7, ntreestune=100, parvec=c(1000, 100, seq(from=1, to=30, by=0.25)), cvreps=1, cvfolds=5, tol=10^-6)})
stopCluster(cl)

save(RL1, file="RL1.Rdata")
