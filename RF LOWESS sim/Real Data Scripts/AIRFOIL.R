setwd("/work/STAT/ajsage")

#setwd("~/Box Sync/Iowa State/Research/Robustness of Random Forest/RFLOWESS sim/Real Data")
library(RFLOWESS)

dataset <- read.csv("AIRFOIL.csv")

parvec <- c(1000,100,seq(from=3, to=30, by=0.25))

#uncontaminated
#set.seed(02042017)  #Important to keep seed same for all files, so we're dealing with same datasets
#Res <- sapply(X=1:30, simplify="array", FUN=function(i){Assess_Real_Data(dataset, nfolds=9, p=0, ntrees=1000, ndsize=5, ntreestune=100, parvec=parvec, cvreps=1, cvfolds=8, tol=10^-6 )})
#save(Res, file="AirfoilRes.Rdata")


#contaminated
set.seed(02042017)  #Important to keep seed same for all files, so we're dealing with same datasets
Res <- sapply(X=1:30, simplify="array", FUN=function(i){Assess_Real_Data(dataset, nfolds=9, p=0.15, ntrees=1000, ndsize=5, ntreestune=100, parvec=parvec, cvreps=1, cvfolds=8, tol=10^-6 )})
save(Res, file="AirfoilRescont.Rdata")
