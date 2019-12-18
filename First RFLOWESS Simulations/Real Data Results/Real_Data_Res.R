setwd("~/Box Sync/Iowa State/Research/Robustness of Random Forest/RFLOWESS sim/Real Data Results")
load("CSLURescont.Rdata")

nreps <- 30
nfolds <- dim(Res)[2]

X <- Res[1,,][[5]]
apply(X, 2, function(x){sum((x-X[,16])^2)})

df <- Res

CalculateMSPE <- function(M){
  return(colMeans((M-M[,ncol(M)])^2))
}

CalculateMAPE <- function(M){
  return(colMeans(abs(M-M[,ncol(M)])))
}


MSPE_Rep_Fold <- function(Res, rep, fold){
  df <- Res[1, rep, fold][[1]]
  return(CalculateMSPE(df))
}

ERR <- sapply(1:nreps, simplify = "array", FUN=function(rep){sapply(1:nfolds, simplify = "array", FUN=MSPE_Rep_Fold, Res=Res)})

AvgERR <- apply(ERR, 1, mean)[c(2,3,5,6,7,12)] 

AvgERR / AvgERR[1]
