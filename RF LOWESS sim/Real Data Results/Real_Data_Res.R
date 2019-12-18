setwd("~/Box Sync/Iowa State/Research/Robustness of Random Forest/RFLOWESS sim/Real Data Results")
load("ConcRescont.Rdata")

nreps <- dim(Res)[3]
nfolds <- dim(Res)[2]

X <- Res[1,,][[5]][c(2,3,5,6,7,12,16),]
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

MAPE_Rep_Fold <- function(Res, rep, fold){
  df <- Res[1, rep, fold][[1]]
  return(CalculateMAPE(df))
}


ERR <- sapply(1:nreps, simplify = "array", FUN=function(rep){sapply(1:nfolds, simplify = "array", FUN=MSPE_Rep_Fold, Res=Res)})
ERR <- sapply(1:nfolds, simplify = "array", FUN=function(rep){sapply(1:nreps, simplify = "array", FUN=function(fold){MSPE_Rep_Fold(Res, rep, fold)})})
ERR <- sapply(1:nfolds, simplify = "array", FUN=function(rep){sapply(1:nreps, simplify = "array", FUN=function(fold){MAPE_Rep_Fold(Res, rep, fold)})})

AvgERR <- apply(ERR, 1, mean)[c(2,3,5,6,7,12)]

AvgERR / AvgERR[1]











ERR[c(2,3,5,6,7,12), 1, 11]

X <- Res[1,11, 1][[1]][,c(2,3,5,6,7,12,16)]

mean((X[,1]-X[,7])^2)
mean((X[,2]-X[,7])^2)
mean((X[,3]-X[,7])^2)
mean((X[,4]-X[,7])^2)
mean((X[,5]-X[,7])^2)
mean((X[,6]-X[,7])^2)
