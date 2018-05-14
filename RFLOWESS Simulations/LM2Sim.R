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
LM2 <- parSapply(cl=cl, X=1:500, simplify="array", FUN=function(i){ApplyAcross_m_and_p(Sim = "LM", ntrain=1000, ntest=1000, p=c(0, 0.05, 0.1, 0.15, 0.2, 0.25), m=c(1), contamination="Var", Vartype="Toeplitz", DGP=2, ntrees=1000, ndsize=5, ntreestune=100, parvec=c(1000, 500, seq(from=1, to=300, by=0.5)), cvreps=1, cvfolds=5, tol=10^-6)})
stopCluster(cl)

save(LM2, file="LM2.Rdata")
