setwd("/work/STAT/ajsage")

library(RFLOWESS)


########################################################################################################

library(openintro)
download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData")
DATA <- ames[,3:ncol(ames)]
DATA$SalePrice <- DATA$SalePrice/1000

FixNAs=function(x){
if(is.integer(x)|is.numeric(x)){
  x[is.na(x)] <- 0} else{
  x <- as.character(x)
  x[is.na(x)] <- "Missing"
  }
  return(x)
}

dataset <- as.data.frame(lapply(DATA, FixNAs))
names(dataset)[names(dataset)=="SalePrice"] <- "Y"

parvec <- c(1000,100,seq(from=3, to=30, by=0.25))

#uncontaminated
set.seed(02042017)  #Important to keep seed same for all files, so we're dealing with same datasets
Res <- sapply(X=1:30, simplify="array", FUN=function(i){Assess_Real_Data(dataset, nfolds=10, p=0, ntrees=1000, ndsize=5, ntreestune=100, parvec=parvec, cvreps=1, cvfolds=9, tol=10^-6 )})
save(Res, file="AmesRes.Rdata")


#contaminated
#set.seed(02042017)  #Important to keep seed same for all files, so we're dealing with same datasets
#Res <- sapply(X=1:30, simplify="array", FUN=function(i){Assess_Real_Data(dataset, nfolds=10, p=0.15, ntrees=1000, ndsize=5, ntreestune=100, parvec=parvec, cvreps=1, cvfolds=9, tol=10^-6 )})
#save(Res, file="AmesRescont.Rdata")
