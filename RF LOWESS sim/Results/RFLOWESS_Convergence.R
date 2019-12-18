#The comments here are actually based on the datasets load("RL1old.Rdata") and load("RL2old.Rdata").
#To replicate, these should be loaded instead of RL1.Rdata and RL2.Rdata.

setwd("~/Box Sync/Iowa State/Research/Robustness of Random Forest/RFLOWESS sim/Results")
load("RL1.Rdata")
dim(RL1)
#dims are 3x6x4x1000
#For first index, 1=Predictions, 2=#interations, 3=info on tuning
#for second index, 1)p=0, 2)p=.05, 3)p=.1, 4)p=.15, 5)p=.2, 6)p=.25
#for 3rd index, 1)m=0.2, 2)m=0.4, 3)m=0.6, 4)m=0.8
#for 4th index iteration number gives 1000x16 matrix with predictions for each technique using each iteration
  #1-y-bar
  #2-RF
  #3-QRF
  #4-Li-Tukey
  #5-Li_Huber
  #6-8 Median type agg
  #9 Lowess (alpha=6)
  #10 Lowess tune unwt MSE
  #11 Lowess tune unwt MAPE
  #12 Lowess tune RF MSE
  #13 Lowess tune RF MAPE
  #14 Lowess tune LOW MSE
  #15 Lowess tune LOW MAPE
  #16 True value of Y

summary(unlist(RL1[2,,,]))  #Look at number of iterations required. Make sure "convergence" was reached

load("RL2.Rdata")
summary(unlist(RL2[2,6,4,]))  #Look at number of iterations required. Make sure "convergence" was reached
#problem with m=2, p=3 and m=3,p=6 and m=4, p=4
summary(unlist(RL2[2,3,2,]))
which(unlist(RL2[2,3,2,])==1000)  #only an issue with iteration 340 using pred #11
which(unlist(RL2[2,6,3,])==1000)  #only an issue with iteration 201 using pred #11
which(unlist(RL2[2,4,4,])==1000)  #only an issue with iteration 435 using pred #11
#look at choice of parameters causing problems
RL2[3,3,2,340][[1]][[3]]

str(RL2[1,6,1,])


A <- array(NA, dim=c(500, 1000, 16))

for(i in 1:500){
A[i,,] <- as.matrix(RL2[1,6,1,i][[1]])
}

CalculateMSPE <- function(M){
return(colMeans((M-M[,ncol(M)])^2))
}

CalculateMAPE <- function(M){
  return(colMeans(abs(M-M[,ncol(M)])))
}

MSPE <- apply(apply(A,1,CalculateMSPE), 1, mean)
MSPE

MAPE <- apply(apply(A,1,CalculateMAPE), 1, mean)
MAPE

MSPEfunc <- function(p, m, df){
  for(i in 1:500){
    A[i,,] <- as.matrix(df[1,p,m,i][[1]])}
    MSPE <- apply(apply(A,1,CalculateMSPE), 1, mean)
    MAPE <- apply(apply(A,1,CalculateMAPE), 1, mean)
    return(list(MSPE, MAPE))
    }

ntechs <- 16
nprops <- 6
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntechs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA", "Truth")
Technique <- c(rep(Techs, nprops))

########################################################################################################
#Simulation RL1
df=RL1
#RL1 (m=0.2)
m=1
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL1m1 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.20")+ theme(plot.title = element_text(hjust = 0.5))
RL1m1a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.20")+ theme(plot.title = element_text(hjust = 0.5))

#RL1 (m=0.4)
m=2
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL1m2 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.20")+ theme(plot.title = element_text(hjust = 0.5))
RL1m2a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.20")+ theme(plot.title = element_text(hjust = 0.5))

#RL1 (m=0.6)
m=3
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL1m3 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.20")+ theme(plot.title = element_text(hjust = 0.5))
RL1m3a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.20")+ theme(plot.title = element_text(hjust = 0.5))

#RL1 (m=0.8)
m=4
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL1m4 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.20")+ theme(plot.title = element_text(hjust = 0.5))
RL1m4a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.20")+ theme(plot.title = element_text(hjust = 0.5))

########################################################################################################


########################################################################################################
#Simulation RL2
df=RL2
#RL2 (m=0.15)
m=1
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL2m1 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.15")+ theme(plot.title = element_text(hjust = 0.5))
RL2m1a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.15")+ theme(plot.title = element_text(hjust = 0.5))

#RL2 (m=0.3)
m=2
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL2m2 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.30")+ theme(plot.title = element_text(hjust = 0.5))
RL2m2a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.30")+ theme(plot.title = element_text(hjust = 0.5))

#RL3 (m=0.45)
m=3
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL2m3 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.45")+ theme(plot.title = element_text(hjust = 0.5))
RL2m3a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.45")+ theme(plot.title = element_text(hjust = 0.5))

#RL4 (m=0.6)
m=4
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL2m4 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.60")+ theme(plot.title = element_text(hjust = 0.5))
RL2m4a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.60")+ theme(plot.title = element_text(hjust = 0.5))

########################################################################################################


##################################################################################################################################################################################
library(gridExtra)
#Get legend at bottom of all plots
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
##################################################################################
#First Roy-Larocque plots

mylegend<-g_legend(RL1m1)

p <- grid.arrange(arrangeGrob(RL1m1 + theme(legend.position="none"),
                              RL1m2 + theme(legend.position="none"),
                              RL1m3 + theme(legend.position="none"),
                              RL1m4 + theme(legend.position="none"),
                              nrow=2),
                  mylegend, nrow=2,heights=c(10, 2))
p


##################
#Second Roy-Larocque plots

mylegend<-g_legend(RL1m1)

p <- grid.arrange(arrangeGrob(RL2m1 + theme(legend.position="none"),
                              RL2m2 + theme(legend.position="none"),
                              RL2m3 + theme(legend.position="none"),
                              RL2m4 + theme(legend.position="none"),
                              nrow=2),
                  mylegend, nrow=2,heights=c(10, 2))
p

## for defense presentation
mylegend<-g_legend(RL1m1)

p <- grid.arrange(arrangeGrob(RL2m1 + theme(legend.position="none"),
                              RL2m2 + theme(legend.position="none"),
                              nrow=1),
                  mylegend, nrow=2,heights=c(10, 2))
p


mylegend<-g_legend(RL1m1)

p <- grid.arrange(arrangeGrob(RL2m3 + theme(legend.position="none"),
                              RL2m4 + theme(legend.position="none"),
                              nrow=1),
                  mylegend, nrow=2,heights=c(10, 2))
p

##################


load("LM1.Rdata")
summary(unlist(LM1[2,6,1,]))  #Look at number of iterations required. Make sure "convergence" was reached
#How many RF-LOWESS predictions didn't come from convergence
sum(matrix(unlist(LM1[2,1,1,]), nrow=500, ncol=7, byrow=T)[,4]==1000)
sum(matrix(unlist(LM1[2,2,1,]), nrow=500, ncol=7, byrow=T)[,4]==1000)
sum(matrix(unlist(LM1[2,3,1,]), nrow=500, ncol=7, byrow=T)[,4]==1000)
sum(matrix(unlist(LM1[2,4,1,]), nrow=500, ncol=7, byrow=T)[,4]==1000)
sum(matrix(unlist(LM1[2,5,1,]), nrow=500, ncol=7, byrow=T)[,4]==1000)
sum(matrix(unlist(LM1[2,6,1,]), nrow=500, ncol=7, byrow=T)[,4]==1000)

#look at choice of parameters causing problems
matrix(unlist(LM1[2,6,1,]), nrow=500, ncol=7, byrow=T) #all alphas
matrix(unlist(LM1[2,6,1,]), nrow=500, ncol=7, byrow=T)[,4] #alphas used for RF-LOWESS
which(matrix(unlist(LM1[2,6,1,]), nrow=500, ncol=7, byrow=T)[,4]==1000)
which(matrix(unlist(LM1[2,6,1,]), nrow=500, ncol=7, byrow=T)[,1]==1000) #no problems using alpha=6

parvec=c(1000, 100, seq(from=1, to=30, by=0.25))
LM1[2,6,1,][[257]]  #problem using alpha chosen by unweighted cv
LM1[3,6,1,257][[1]][[3]] #fails to converge for alpha=30 and alpha=24.75
data.frame(parvec,LM1[3,6,1,257][[1]][[2]] ) #prediction errors for each alpha
data.frame(parvec,t(LM1[3,6,1,257][[1]][[1]][,1,]) )  #weighted OOB error for each alpha using each of 6 weighting approaches
LM1[3,6,1,257][[1]][[3]] #problem using alpha=25.75
LM1[3,6,1,257][[1]][[6]]


LM1[1,6,1,257][[1]][,10] #predictions using alpha=30
LM1[1,6,1,257][[1]][,12] #predictions using alpha=29



qplot(LM1[1,6,1,257][[1]][,10], LM1[1,6,1,257][[1]][,12])

load("LM2.Rdata")
summary(unlist(LM2[2,6,1,]))  #Look at number of iterations required. Make sure "convergence" was reached
#How many RF-LOWESS predictions didn't come from convergence
sum(matrix(unlist(LM2[2,1,1,]), nrow=500, ncol=7, byrow=T)[,1]==1000)
sum(matrix(unlist(LM2[2,2,1,]), nrow=500, ncol=7, byrow=T)[,1]==1000)
sum(matrix(unlist(LM2[2,3,1,]), nrow=500, ncol=7, byrow=T)[,1]==1000)
sum(matrix(unlist(LM2[2,4,1,]), nrow=500, ncol=7, byrow=T)[,1]==1000)
sum(matrix(unlist(LM2[2,5,1,]), nrow=500, ncol=7, byrow=T)[,1]==1000)
sum(matrix(unlist(LM2[2,6,1,]), nrow=500, ncol=7, byrow=T)[,1]==1000)


###########################################################################

library(RFLOWESS)

set.seed(04042018)
SimRes <- RunSimulation("LM", ntrain=100, ntest=100, p=.25, m=1,contamination = "Var", Vartype="Id", DGP=2, ntrees=1000, ndsize=5, ntreestune=100, parvec=c(1000, 100, seq(from=1, to=30, by=0.25)),cvreps=1, cvfolds=5, tol=10^-6)
TRAIN <- SimRes[[1]][[1]]
TEST <- SimRes[[1]][[2]]
TRAINY <-TRAIN[,ncol(TRAIN)]

SimRes[[4]][[8]]
parvec[which(SimRes[[4]][[8]]==1000)]
which(SimRes[[4]][[8]]==1000)
str(SimRes[[4]])
SimRes[[4]][[7]][[98]] #weights using alpha=24.75 (didn't converge)
SimRes[[4]][[7]][[97]] #weights using alpha=24.5 (did converge)
round(SimRes[[4]][[7]][[98]]-SimRes[[4]][[7]][[97]],4)

############################################################################################
library(RFLOWESS)
set.seed(04042018)
DATA <- generate_LMdata(ntrain=100, ntest=100, p=.25, Vartype="Id")
TRAIN <- DATA[[1]]
TEST <- DATA[[2]]
TRAINY <- TRAIN$Y
RF <- randomForestSRC::rfsrc(Y~., data=TRAIN, nodesize=5, forest.wt="oob", membership = T)
RFpredInfo <- predict(RF, newdata=TEST, forest.wt=TRUE, membership = T)
RFpred <- RFpredInfo$predicted
TrainNodes <- RF$membership
TestNodes <- RFpredInfo$membership
Inbag <- RF$inbag
OOBWeights <- RF$forest.wt
PredWeights <- RFpredInfo$forest.wt
PredInfo1 <- LOWESSPred(OOBWeights, PredWeights, TRAINY, alpha = 24.75, method = "Tukey", tol = 10^-6)
PredInfo1[[3]]
PredInfo2 <- LOWESSPred(OOBWeights, PredWeights, TRAINY, alpha = 24.5, method = "Tukey", tol = 10^-6)
PredInfo2[[3]]




#PredInfo1[[1]]
#PredInfo2[[1]]
#round(PredInfo1[[1]]-PredInfo2[[1]],3)
#qplot(PredInfo1[[1]],PredInfo2[[1]])

#PredInfo1[[2]]
#PredInfo2[[2]]
#round(PredInfo1[[2]]-PredInfo2[[2]],3)
#qplot(PredInfo1[[1]],PredInfo2[[1]])


alpha <- 24.75
#alpha <- 24.5
method <- "Tukey"
tol <- 10^-6
maxiter <- 1000

d <- rep(1, length(TRAINY))
D0 <- matrix(rep(d, length(TRAINY)), nrow=length(TRAINY), ncol=length(TRAINY), byrow=T)
AdjWts <- D0 * OOBWeights + 10^-9  #last term adds small amount to ensure weights don't all get set to 0 when alpha is very small
AdjWts <- AdjWts / rowSums(AdjWts)
OOBPred <- as.matrix(AdjWts) %*% TRAINY
resid <- TRAINY- OOBPred
resid[is.na(resid)] <- mean(abs(resid), na.rm=T)   #if case does not come up OOB set residual to mean of residuals
niter <- 1
Change <- 1

OOBPREDMAT <- array(NA, dim=c(length(TRAINY), 1001))
LAMBDAS <- array(NA, dim=c(length(TRAINY), 1001))
CHANGE <- rep(NA, 1001)

OOBPREDMAT[,1] <- RF$predicted.oob
LAMBDAS[,1] <- 1


for(iter in 1:1000){
  d0 <- d
  s <- median(abs(resid))
  t <- resid/(alpha*s)
  if (method=="Tukey") {
    d <- t(apply(data.frame(t), 1, Tukey)) #Tukey weights
  }else {
    d <- t(apply(data.frame(t), 1, Huber)) #Huber weights
  }
  D0 <- matrix(rep(d, length(TRAINY)), nrow=length(TRAINY), ncol=length(TRAINY), byrow=T)
  AdjWts <- D0*OOBWeights + 10^-9 # add in very small buffer so that if all are zero we don't divide by 0
  AdjWts <- AdjWts / rowSums(AdjWts)
  OOBPred0 <- OOBPred
  OOBPred <- as.matrix(AdjWts) %*% TRAINY
  resid <- TRAINY - OOBPred
  resid[is.na(resid)] <- mean(abs(resid), na.rm=T)   #if case does not come up OOB
  Change <- mean((OOBPred-OOBPred0)^2)
  CHANGE[iter+1] <- Change
  niter <- niter + 1
OOBPREDMAT[,iter+1] <- OOBPred
LAMBDAS[,iter+1] <- d
}

round(LAMBDAS[,10]-LAMBDAS[,9],2)
which(abs(LAMBDAS[,10]-LAMBDAS[,9]) > 0.02)

data.frame(OOBWeights[13,], TRAINY)  #13 depends on 66
data.frame(OOBWeights[66,], TRAINY)  #66 depends on 24

round(LAMBDAS[c(13,24, 25, 66),1:10],5)  #robustness weights
CHANGE
OOBWeights[c(13, 24, 25, 66),c(13, 24, 25, 66)]  # RFOOB weights (13<->66, 66<->24)

TRAINY[c(13, 24, 25, 66)]  #True y for training cases
round(LAMBDAS[c(13,24, 25, 66),1:10],5)  #robustness weights
round(OOBPREDMAT[c(13,24, 25, 66),1:10],5) #OOB predictions
round((TRAINY-OOBPREDMAT)[c(13, 24, 25, 66),1:10],5) #OOB residuals

# Case 13 is much lower than expected, case 66 is much higher than expected
# and both have considerable weight in prediction of other.
# Once case 24 is knocked out, prediction for case 66 improves (increases) and residual decreases.
# Thus, 66 is weighed more heavily in iteration 3 than 2.
# However, down-weighting case 66 in step 2 improved (decreased) prediction for case 13.
# So when case 66 gets more weight in iter. 3, prediction for case 13 gets bigger (worse)
# At same time, increasing weight on case 13 in step 3 causes prediction for case 66 to get slightly lower (worse)
# So, in iter 3, both 13 and 66 have larger residuals and get downweighted
# Down-weighting 66 decreases (improves) prediction on 13
# and downweighting 13 improves prediction on 66
# Both are up-weighted in step 5, and cycle continues
# Case 25 also jumps around a lot, but I'm not sure why, not heavilty influenced by others


round(apply(abs(TRAINY-OOBPREDMAT),2,median),5)[1:10] #median of abs of residuals (jumps up and down)

l1 <- LAMBDAS[,9]  #weights from iteration 9
l2 <- LAMBDAS[,10] #weights from iteration 10

L1 <- matrix(rep(LAMBDAS[,9],100), nrow=100, ncol=100, byrow=T)
L2 <- matrix(rep(LAMBDAS[,10],100), nrow=100, ncol=100, byrow=T)

LPred1 <- (L1*PredWeights)%*%TRAINY/rowSums(L1*PredWeights) #Predictions using lambda from iter. 9
LPred2 <- (L2*PredWeights)%*%TRAINY/rowSums(L2*PredWeights) #Predictions using lambda from iter. 9
qplot(LPred1, LPred2)+geom_abline()  #Predictions for 2 different RFL lambda sets.
qplot(LPred1, RFpredInfo$predicted) #RFL1 vs RF
qplot(RFpredInfo$predicted, TEST$Yt) #RF vs True
qplot(LPred1, TEST$Yt) #RFL vs True

sum((RFpredInfo$predicted-TEST$Y)^2) #SSE for RF
sum((LPred1-TEST$Y)^2)               #SSE for RFL1
sum((LPred2-TEST$Y)^2)               #SSE for RFL2
