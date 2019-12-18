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
LM1 <- parSapply(cl=cl, X=1:2, simplify="array", FUN=function(i){ApplyAcross_m_and_p(Sim = "LM", ntrain=100, ntest=100, p=c(0, 0.05, 0.1, 0.15, 0.2, 0.25), m=c(1), contamination="Var", Vartype="Id", DGP=2, ntrees=100, ndsize=5, ntreestune=100, parvec=c(1000, 100, seq(from=1, to=30, by=0.25)), cvreps=1, cvfolds=5, tol=10^-6)})
stopCluster(cl)

save(LM1, file="LM1.Rdata")
