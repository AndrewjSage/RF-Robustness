setwd("/work/STAT/ajsage")

#setwd("~/Box Sync/Iowa State/Research/Robustness of Random Forest/RFLOWESS sim/Real Data Scripts")
library(RFLOWESS)

dataset <- read.csv("BIRT.csv")


#uncontaminated
parvec <- c(1000,100,seq(from=3, to=30, by=0.25))

set.seed(02042017)
Res <- sapply(X=1:30, simplify="array", FUN=function(i){Assess_Real_Data(dataset, nfolds=9, p=0, ntrees=1000, ndsize=5, ntreestune=100, parvec=parvec, cvreps=1, cvfolds=8, tol=10^-6 )})
Res <- save(Res, file="BirthwtRes.Rdata")


#contaminated
set.seed(02042017)
Res <- sapply(X=1:30, simplify="array", FUN=function(i){Assess_Real_Data(dataset, nfolds=9, p=0.15, ntrees=1000, ndsize=5, ntreestune=100, parvec=parvec, cvreps=1, cvfolds=8, tol=10^-6 )})
save(Res, file="BirthwtRescont.Rdata")
