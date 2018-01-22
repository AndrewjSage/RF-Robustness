setwd("~/Box Sync/Iowa State/Research/Robustness of Random Forest/Fall 17/Official_Simulation/Final Simulations/Final Results/LS Splitting")
library(ggplot2)

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


################################################################################################
#Find index of parameters that would have been chosen using only non-outliers

#function to get MSPE out of LPREDERR for indices chosen
GetMSPE <- function(i){
  return(LPREDERR[i,ParInd[i],1])
}
GetMAPE <- function(i){
  return(LPREDERR[i,ParInd[i],2])
}

#find index of best parameter choice for each of 500 reps
ParInd <- apply(LOOBERR[,,1,3],1,which.min)  #1 indicates MSPE without weighting, #3 indicated only parameters using nonOutliers
#Note these do match wth ChosenPars
#Find MSPE for test data using these pars
mean(sapply(1:500, GetERR))
#############################################################################################################

ntecs <- 15
nprops <- 6

#Roy-Larocque Simulation 1:m=0.20
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntecs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA", "LOWESS-Nonoutliers")
Weighting <- c(rep(Techs, nprops))
load("Sim1m20p0.Rdata")
ParIndMSPE <- apply(LOOBERR[,,1,3],1,which.min)  #1 indicates MSPE without weighting, #3 indicated only parameters using nonOutliers
ParIndMAPE <- apply(LOOBERR[,,2,3],1,which.min)  #2 indicates MAPE without weighting, #3 indicated only parameters using nonOutliers
MSPE <- c(apply(ERRMat[,,1], 2, mean, na.rm=T), mean(sapply(1:500, GetMSPE))) #attach MSPE using only non-outliers for tuning
MAPE <-c(apply(ERRMat[,,2], 2, mean, na.rm=T), mean(sapply(1:500, GetMAPE))) #attach MAPE using only non-outliers for tuning
load("Sim1m20p05.Rdata")  
ParIndMSPE <- apply(LOOBERR[,,1,3],1,which.min)  #1 indicates MSPE without weighting, #3 indicated only parameters using nonOutliers
ParIndMAPE <- apply(LOOBERR[,,2,3],1,which.min)  #2 indicates MAPE without weighting, #3 indicated only parameters using nonOutliers
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T), mean(sapply(1:500, GetMSPE)))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T), mean(sapply(1:500, GetMAPE)))
load("Sim1m20p10.Rdata")
ParIndMSPE <- apply(LOOBERR[,,1,3],1,which.min)  #1 indicates MSPE without weighting, #3 indicated only parameters using nonOutliers
ParIndMAPE <- apply(LOOBERR[,,2,3],1,which.min)  #2 indicates MAPE without weighting, #3 indicated only parameters using nonOutliers
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T), mean(sapply(1:500, GetMSPE)))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T), mean(sapply(1:500, GetMAPE)))
load("Sim1m20p15.Rdata")
ParIndMSPE <- apply(LOOBERR[,,1,3],1,which.min)  #1 indicates MSPE without weighting, #3 indicated only parameters using nonOutliers
ParIndMAPE <- apply(LOOBERR[,,2,3],1,which.min)  #2 indicates MAPE without weighting, #3 indicated only parameters using nonOutliers
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T), mean(sapply(1:500, GetMSPE)))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T), mean(sapply(1:500, GetMAPE)))
load("Sim1m20p20.Rdata")
ParIndMSPE <- apply(LOOBERR[,,1,3],1,which.min)  #1 indicates MSPE without weighting, #3 indicated only parameters using nonOutliers
ParIndMAPE <- apply(LOOBERR[,,2,3],1,which.min)  #2 indicates MAPE without weighting, #3 indicated only parameters using nonOutliers
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T), mean(sapply(1:500, GetMSPE)))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T), mean(sapply(1:500, GetMAPE)))
load("Sim1m20p25.Rdata")
ParIndMSPE <- apply(LOOBERR[,,1,3],1,which.min)  #1 indicates MSPE without weighting, #3 indicated only parameters using nonOutliers
ParIndMAPE <- apply(LOOBERR[,,2,3],1,which.min)  #2 indicates MAPE without weighting, #3 indicated only parameters using nonOutliers
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T), mean(sapply(1:500, GetMSPE)))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T), mean(sapply(1:500, GetMAPE)))
df <- data.frame(p, Weighting, MSPE, MAPE)
df <- subset(df, Weighting%in%Techs[c(10,12,14,15)])
df$Weighting <- rep(c("Unweighted", "RF Residuals", "RFL Residuals", "NonOutliers"),6)
RL1m1 <- ggplot(df, aes(x = p, y = MSPE, color = Weighting)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.20")+ theme(plot.title = element_text(hjust = 0.5))
RL1m1a <- ggplot(df, aes(x = p, y = MAPE, color = Weighting)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.20")+ theme(plot.title = element_text(hjust = 0.5))

#Roy-Larocque Simulation 1:m=0.40
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntecs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA")
Weighting <- c(rep(Techs, nprops))
load("Sim1m40p0.Rdata")
MSPE <- apply(ERRMat[,,1], 2, mean, na.rm=T)
MAPE <- apply(ERRMat[,,2], 2, mean, na.rm=T)
load("Sim1m40p05.Rdata")   #Need to fix this to p05 when sim is run
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim1m40p10.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim1m40p15.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim1m40p20.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim1m40p25.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
df <- data.frame(p, Weighting, MSPE, MAPE)
df <- subset(df, Weighting%in%Techs[c(10,12,14)])
df$Weighting <- rep(c("Unweighted", "RF Residuals", "RFL Residuals"),6)
RL1m2 <- ggplot(df, aes(x = p, y = MSPE, color = Weighting)) + geom_line()+geom_point()+ggtitle("b) m=0.40")+ theme(plot.title = element_text(hjust = 0.5))
RL1m2a <- ggplot(df, aes(x = p, y = MAPE, color = Weighting)) + geom_line()+geom_point()+ggtitle("b) m=0.40")+ theme(plot.title = element_text(hjust = 0.5))


#Roy-Larocque Simulation 1:m=0.60
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntecs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA")
Weighting <- c(rep(Techs, nprops))
load("Sim1m60p0.Rdata")
MSPE <- apply(ERRMat[,,1], 2, mean, na.rm=T)
MAPE <- apply(ERRMat[,,2], 2, mean, na.rm=T)
load("Sim1m60p05.Rdata")   #Need to fix this to p05 when sim is run
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim1m60p10.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim1m60p15.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim1m60p20.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim1m60p25.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
df <- data.frame(p, Weighting, MSPE, MAPE)
df <- subset(df, Weighting%in%Techs[c(10,12,14)])
df$Weighting <- rep(c("Unweighted", "RF Residuals", "RFL Residuals"),6)
RL1m3 <- ggplot(df, aes(x = p, y = MSPE, color = Weighting)) + geom_line()+geom_point()+ggtitle("c) m=0.60")+ theme(plot.title = element_text(hjust = 0.5))
RL1m3a <- ggplot(df, aes(x = p, y = MAPE, color = Weighting)) + geom_line()+geom_point()+ggtitle("c) m=0.60")+ theme(plot.title = element_text(hjust = 0.5))

#Roy-Larocque Simulation 1:m=0.8
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntecs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA")
Weighting <- c(rep(Techs, nprops))
load("Sim1m80p0.Rdata")
MSPE <- apply(ERRMat[,,1], 2, mean, na.rm=T)
MAPE <- apply(ERRMat[,,2], 2, mean, na.rm=T)
load("Sim1m80p05.Rdata")   #Need to fix this to p05 when sim is run
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim1m80p10.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim1m80p15.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim1m80p20.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim1m80p25.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
df <- data.frame(p, Weighting, MSPE, MAPE)
df <- subset(df, Weighting%in%Techs[c(10,12,14)])
df$Weighting <- rep(c("Unweighted", "RF Residuals", "RFL Residuals"),6)
RL1m4 <- ggplot(df, aes(x = p, y = MSPE, color = Weighting))+ geom_line()+geom_point()+ggtitle("d) m=0.80")+ theme(plot.title = element_text(hjust = 0.5))
RL1m4a <- ggplot(df, aes(x = p, y = MAPE, color = Weighting)) + geom_line()+geom_point()+ggtitle("d) m=0.80")+ theme(plot.title = element_text(hjust = 0.5))

p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntecs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA")
Weighting <- c(rep(Techs, nprops))
load("Sim2m15p0.Rdata")
MSPE <- apply(ERRMat[,,1], 2, mean, na.rm=T)
MAPE <- apply(ERRMat[,,2], 2, mean, na.rm=T)
load("Sim2m15p05.Rdata")   
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim2m15p10.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim2m15p15.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim2m15p20.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim2m15p25.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
df <- data.frame(p, Weighting, MSPE, MAPE)
df <- subset(df, Weighting%in%Techs[c(10,12,14)])
df$Weighting <- rep(c("Unweighted", "RF Residuals", "RFL Residuals"),6)
RL2m1 <- ggplot(df, aes(x = p, y = MSPE, color = Weighting)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ theme(plot.title = element_text(hjust = 0.5))
RL2m1a <- ggplot(df, aes(x = p, y = MAPE, color = Weighting)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.15")+ theme(plot.title = element_text(hjust = 0.5))

#Roy-Larocque Simulation 2:m=0.30
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntecs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA")
Weighting <- c(rep(Techs, nprops))
load("Sim2m30p0.Rdata")
MSPE <- apply(ERRMat[,,1], 2, mean, na.rm=T)
MAPE <- apply(ERRMat[,,2], 2, mean, na.rm=T)
load("Sim2m30p05.Rdata")   #Need to fix this to p05 when sim is run
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim2m30p10.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim2m30p15.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim2m30p20.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim2m30p25.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
df <- data.frame(p, Weighting, MSPE, MAPE)
df <- subset(df, Weighting%in%Techs[c(10,12,14)])
df$Weighting <- rep(c("Unweighted", "RF Residuals", "RFL Residuals"),6)
RL2m2 <- ggplot(df, aes(x = p, y = MSPE, color = Weighting)) + geom_line()+geom_point()+ggtitle("b) m=0.30")+ theme(plot.title = element_text(hjust = 0.5))
RL2m2a <- ggplot(df, aes(x = p, y = MAPE, color = Weighting)) + geom_line()+geom_point()+ggtitle("b) m=0.30")+ theme(plot.title = element_text(hjust = 0.5))


#Roy-Larocque Simulation 2:m=0.45
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntecs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA")
Weighting <- c(rep(Techs, nprops))
load("Sim2m45p0.Rdata")
MSPE <- apply(ERRMat[,,1], 2, mean, na.rm=T)
MAPE <- apply(ERRMat[,,2], 2, mean, na.rm=T)
load("Sim2m45p05.Rdata")   #Need to fix this to p05 when sim is run
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim2m45p10.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim2m45p15.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim2m45p20.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim2m45p25.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
df <- data.frame(p, Weighting, MSPE, MAPE)
df <- subset(df, Weighting%in%Techs[c(10,12,14)])
df$Weighting <- rep(c("Unweighted", "RF Residuals", "RFL Residuals"),6)
RL2m3 <- ggplot(df, aes(x = p, y = MSPE, color = Weighting)) + geom_line()+geom_point()+ggtitle("c) m=0.45")+ theme(plot.title = element_text(hjust = 0.5))
RL2m3a <- ggplot(df, aes(x = p, y = MAPE, color = Weighting)) + geom_line()+geom_point()+ggtitle("c) m=0.45")+ theme(plot.title = element_text(hjust = 0.5))

#Roy-Larocque Simulation 2:m=0.6
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntecs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA")
Weighting <- c(rep(Techs, nprops))
load("Sim2m60p0.Rdata")
MSPE <- apply(ERRMat[,,1], 2, mean, na.rm=T)
MAPE <- apply(ERRMat[,,2], 2, mean, na.rm=T)
load("Sim2m60p05.Rdata")   #Need to fix this to p05 when sim is run
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim2m60p10.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim2m60p15.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim2m60p20.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim2m60p25.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
df <- data.frame(p, Weighting, MSPE, MAPE)
df <- subset(df, Weighting%in%Techs[c(10,12,14)])
df$Weighting <- rep(c("Unweighted", "RF Residuals", "RFL Residuals"),6)
RL2m4 <- ggplot(df, aes(x = p, y = MSPE, color = Weighting)) + geom_line()+geom_point()+ggtitle("d) m=0.60")+ theme(plot.title = element_text(hjust = 0.5))
RL2m4a <- ggplot(df, aes(x = p, y = MAPE, color = Weighting)) + geom_line()+geom_point()+ggtitle("d) m=0.60")+ theme(plot.title = element_text(hjust = 0.5))

#######################################################################################

#Li-Martin Simulation 1
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntecs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA")
Weighting <- c(rep(Techs, nprops))
load("Li1p0.Rdata")
MSPE <- apply(ERRMat[,,1], 2, mean, na.rm=T)
MAPE <- apply(ERRMat[,,2], 2, mean, na.rm=T)
load("Li1p05.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Li1p10.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Li1p15.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Li1p20.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Li1p25.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
df <- data.frame(p, Weighting, MSPE, MAPE)
df1 <- subset(df, Weighting%in%Techs[c(10,12,14)])
df$Weighting <- rep(c("Unweighted", "RF Residuals", "RFL Residuals"),6)
LM1 <- ggplot(df1, aes(x = p, y = MSPE, color = Weighting)) + geom_line()+geom_point()+ylim(c(7,13))+ theme(legend.position = "bottom")+ggtitle("Li, Martin: Simulation 1")+ theme(plot.title = element_text(hjust = 0.5))
df2 <- subset(df, Weighting%in%Techs[c(11,13,15)])
LM1a <- ggplot(df2, aes(x = p, y = MAPE, color = Weighting)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("Li, Martin: Simulation 1")+ theme(plot.title = element_text(hjust = 0.5))

#######################################################################################

#Li-Martin Simulation 2
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntecs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA")
Weighting <- c(rep(Techs, nprops))
load("Li2p0.Rdata")
MSPE <- apply(ERRMat[,,1], 2, mean, na.rm=T)
MAPE <- apply(ERRMat[,,2], 2, mean, na.rm=T)
load("Li2p05.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Li2p10.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Li2p15.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Li2p20.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Li2p25.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
df <- data.frame(p, Weighting, MSPE, MAPE)
df1 <- subset(df, Weighting%in%Techs[c(10,12,14)])
df1$Weighting <- rep(c("Unweighted", "RF Residuals", "RFL Residuals"),6)
LM2 <- ggplot(df1, aes(x = p, y = MSPE, color = Weighting)) + geom_line()+geom_point()+ylim(c(7,13))+ theme(legend.position = "bottom")+ggtitle("Li, Martin: Simulation 2")+ theme(plot.title = element_text(hjust = 0.5))
df2 <- subset(df, Weighting%in%Techs[c(11,13,15)])
df2$Weighting <- rep(c("Unweighted", "RF Residuals", "RFL Residuals"),6)
LM2a <- ggplot(df2, aes(x = p, y = MAPE, color = Weighting)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("Li, Martin: Simulation 2")+ theme(plot.title = element_text(hjust = 0.5))


###########################################################################################
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


##################
#Li-Martin-MSPE

mylegend<-g_legend(RL1m1)

p <- grid.arrange(arrangeGrob(LM1 + theme(legend.position="none"),
                              LM2 + theme(legend.position="none"),
                              nrow=1),
                  mylegend, nrow=2,heights=c(10, 2))
p

#Li-Martin-MAPE

mylegend<-g_legend(RL1m1)

p <- grid.arrange(arrangeGrob(LM1a + theme(legend.position="none"),
                              LM2a + theme(legend.position="none"),
                              nrow=1),
                  mylegend, nrow=2,heights=c(10, 2))
p


###################################################################################################

#Check which values are chosen for parameters

#Column 1 is for all observations
#Column 2 is for outliers
#Column 3 is for non-outliers

#Use rows 1,3,5 to get par. choices using MSPE
#Use rows 2,4,6 to get par. choices using MAPE

q25=function(x){
  return(quantile(x, .25))
}
q75=function(x){
  return(quantile(x, .75))
}


#m=0.15
load("Sim2m15p0.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1] #Median choice by reach method
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))  #unweighted
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))  #weight by RF rediduals
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))  #weight by RF lowess residuals
c(quantile(BestPars[,1], .25),quantile(BestPars[,1], .75) )
 #m=0.15
load("Sim2m15p05.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1]
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))
c(quantile(BestPars[,1], .25),quantile(BestPars[,1], .75) )
load("Sim2m15p10.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1]
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))
load("Sim2m15p15.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1]
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))
load("Sim2m15p20.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1]
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))
load("Sim2m15p25.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1]
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))

#m=0.30
load("Sim2m30p0.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1]
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))
load("Sim2m30p05.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1]
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))
load("Sim2m30p10.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1]
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))
load("Sim2m30p15.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1]
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))
load("Sim2m30p20.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1]
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))
load("Sim2m30p25.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1]
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))

#m=0.45
load("Sim2m45p0.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1]
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))
load("Sim2m45p05.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1]
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))
load("Sim2m45p10.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1]
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))
load("Sim2m45p15.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1]
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))
load("Sim2m45p20.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1]
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))
load("Sim2m45p25.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1]
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))

#m=0.60
load("Sim2m60p0.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1]
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))
load("Sim2m60p05.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1]
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))
load("Sim2m60p10.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1]
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))
load("Sim2m60p15.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1]
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))
load("Sim2m60p20.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1]
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))
load("Sim2m60p25.Rdata")
apply(ChosenPars, c(2,3), median)[c(1,3,5),1]
c(quantile(ChosenPars[,1,1], .25), quantile(ChosenPars[,1,1], .75))
c(quantile(ChosenPars[,3,1], .25), quantile(ChosenPars[,3,1], .75))
c(quantile(ChosenPars[,5,1], .25), quantile(ChosenPars[,5,1], .75))

#####################################################################################################################
#Get best values from test data
#LPREDERR #in 3rd dim, 1=MSPE, 2=MAPE

parvec <- c(1000,100,seq(from=1, to=30, by=0.25))

load("Sim2m60p0.Rdata")
plot(seq(from=1, to=30, by=0.25), apply(LPREDERR[,,1], 2, mean)[3:119], type="l")
parvec[which.min(apply(LPREDERR[,,1], 2, mean))] #get best par. value according to test data
load("Sim2m60p05.Rdata")
plot(seq(from=1, to=30, by=0.25), apply(LPREDERR[,,1], 2, mean)[3:119], type="l")
parvec[which.min(apply(LPREDERR[,,1], 2, mean))]
load("Sim2m60p10.Rdata")
plot(seq(from=1, to=30, by=0.25), apply(LPREDERR[,,1], 2, mean)[3:119], type="l")
parvec[which.min(apply(LPREDERR[,,1], 2, mean))]
load("Sim2m60p15.Rdata")
plot(seq(from=1, to=30, by=0.25), apply(LPREDERR[,,1], 2, mean)[3:119], type="l")
parvec[which.min(apply(LPREDERR[,,1], 2, mean))]
load("Sim2m60p20.Rdata")
plot(seq(from=1, to=30, by=0.25), apply(LPREDERR[,,1], 2, mean)[3:119], type="l")
parvec[which.min(apply(LPREDERR[,,1], 2, mean))]
load("Sim2m60p25.Rdata")
plot(seq(from=1, to=30, by=0.25), apply(LPREDERR[,,1], 2, mean)[3:119], type="l")
parvec[which.min(apply(LPREDERR[,,1], 2, mean))]


###########################################################################
#Graphs for paper

load("Sim2m15p25.Rdata")
Par <- seq(from=1, to=30, by=0.25)
MSPE <- apply(LPREDERR[,,1], 2, mean)[3:119]
df <- data.frame(Par, MSPE)
ggplot(df, aes(x = Par, y = MSPE)) +xlab(expression(delta)) + geom_line()+ggtitle("a) m=0.15, p=0.25")+ theme(plot.title = element_text(hjust = 0.5))

load("Sim2m60p25.Rdata")
Par <- seq(from=1, to=30, by=0.25)
MSPE <- apply(LPREDERR[,,1], 2, mean)[3:119]
df <- data.frame(Par, MSPE)
ggplot(df, aes(x = Par, y = MSPE)) +xlab(expression(delta)) + geom_line()+ggtitle("b) m=0.60, p=0.25")+ theme(plot.title = element_text(hjust = 0.5))



