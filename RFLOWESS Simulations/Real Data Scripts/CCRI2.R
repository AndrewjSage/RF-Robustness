setwd("/work/STAT/ajsage")

#setwd("~/Box Sync/Iowa State/Research/Robustness of Random Forest/RFLOWESS sim/Real Data Scripts")
library(RFLOWESS)

dataset <- read.csv("CCRI.csv")
dataset <- dataset[which(dataset$X27!="?"),] #get rid of missing value
dataset$X27 <- as.numeric(as.character(dataset$X27))

parvec <- c(1000,100,seq(from=3, to=30, by=0.25))

#uncontaminated
#set.seed(02042017)  #Important to keep seed same for all files, so we're dealing with same datasets
#Res <- sapply(X=1:30, simplify="array", FUN=function(i){Assess_Real_Data(dataset, nfolds=10, p=0, ntrees=1000, ndsize=5, ntreestune=100, parvec=parvec, cvreps=1, cvfolds=11, tol=10^-6 )})
#save(Res, file="CCRIRes.Rdata")


#contaminated
set.seed(02042017)  #Important to keep seed same for all files, so we're dealing with same datasets
Res <- sapply(X=1:30, simplify="array", FUN=function(i){Assess_Real_Data(dataset, nfolds=10, p=0.15, ntrees=1000, ndsize=5, ntreestune=100, parvec=parvec, cvreps=1, cvfolds=11, tol=10^-6 )})
save(Res, file="CCRIRescont.Rdata")
