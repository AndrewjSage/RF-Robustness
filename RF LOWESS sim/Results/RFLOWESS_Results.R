setwd("~/Box Sync/Iowa State/Research/Robustness of Random Forest/RFLOWESS sim/Results")
setwd("~/OneDrive - Lawrence University/Research/Robust RF/RFLOWESS sim/Results")

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

CalculateMSPE <- function(M){
return(colMeans((M-M[,ncol(M)])^2))
}

CalculateMAPE <- function(M){
  return(colMeans(abs(M-M[,ncol(M)])))
}


MSPEfunc <- function(p, m, df){
  nreps <- dim(df)[4]
  ntest <- dim(df[1,1,1,1][[1]])[1]
  A <- array(NA, dim=c(nreps,ntest,16)) #nreps x ntest x npreds
  for(i in 1:nreps){
    A[i,,] <- as.matrix(df[1,p,m,i][[1]])}
    MSPE <- apply(apply(A,1,CalculateMSPE), 1, mean)
    MAPE <- apply(apply(A,1,CalculateMAPE), 1, mean)
    MSPESD <- apply(apply(A,1,CalculateMSPE), 1, sd)
    MAPESD <- apply(apply(A,1,CalculateMAPE), 1, sd)
    return(list(MSPE, MAPE, MSPESD, MAPESD))
    }

ntechs <- 16
nprops <- 6
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntechs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA", "Truth")
Technique <- c(rep(Techs, nprops))
dodge <- position_dodge(width=0.01)

#Function to create plot
Createplot <- function(dfList, xvar){ 
  df <- do.call("rbind", dfList)
  df <- subset(df, var==xvar)
  p <- ggplot(df, aes(x=p, y=VI, colour = Method))+theme(text = element_text(size=16))
  dodge <- position_dodge(width=0.05)
  p1 <- p +geom_line(aes(group = Method)) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), position = dodge, width = 0.05)+ labs(x = "Proportion Missing", y="Scaled Importance")+ theme(legend.position = "none")+ theme(plot.title = element_text(hjust = 0.5))
  return(p1)
}

########################################################################################################
#Simulation RL1
df <- RL1
#RL1 (m=0.2)
m <- 1
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
MSPESD <- c(MSPEfunc(p=1, m=m, df=df)[[3]],MSPEfunc(p=2, m=m, df=df)[[3]],MSPEfunc(p=3, m=m, df=df)[[3]],MSPEfunc(p=4, m=m, df=df)[[3]],MSPEfunc(p=5, m=m, df=df)[[3]],MSPEfunc(p=6, m=m, df=df)[[3]] )
MAPESD <- c(MSPEfunc(p=1, m=m, df=df)[[4]],MSPEfunc(p=2, m=m, df=df)[[4]],MSPEfunc(p=3, m=m, df=df)[[4]],MSPEfunc(p=4, m=m, df=df)[[4]],MSPEfunc(p=5, m=m, df=df)[[4]],MSPEfunc(p=6, m=m, df=df)[[4]] )
LowerMSPE <- MSPE - qt(.025, 499)*MSPESD/sqrt(500)
LowerMAPE <- MAPE - qt(.025, 499)*MAPESD/sqrt(500)
UpperMSPE <- MSPE + qt(.025, 499)*MSPESD/sqrt(500)
UpperMAPE <- MAPE + qt(.025, 499)*MAPESD/sqrt(500)
df <- data.frame(p, Technique, MSPE, MAPE, LowerMSPE, LowerMAPE, UpperMSPE, UpperMAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL1m1 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+
  geom_errorbar(aes(ymin = LowerMSPE, ymax = UpperMSPE), position = dodge, width = 0.05) + theme(legend.position = "bottom")+ggtitle("a) m=0.20")+ theme(plot.title = element_text(hjust = 0.5))
RL1m1a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+
  geom_errorbar(aes(ymin = LowerMAPE, ymax = UpperMAPE), position = dodge, width = 0.05)+ theme(legend.position = "bottom")+ggtitle("a) m=0.20")+ theme(plot.title = element_text(hjust = 0.5))
RL1m1df <- df

#RL1 (m=0.4)
df <- RL1
m <- 2
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
MSPESD <- c(MSPEfunc(p=1, m=m, df=df)[[3]],MSPEfunc(p=2, m=m, df=df)[[3]],MSPEfunc(p=3, m=m, df=df)[[3]],MSPEfunc(p=4, m=m, df=df)[[3]],MSPEfunc(p=5, m=m, df=df)[[3]],MSPEfunc(p=6, m=m, df=df)[[3]] )
MAPESD <- c(MSPEfunc(p=1, m=m, df=df)[[4]],MSPEfunc(p=2, m=m, df=df)[[4]],MSPEfunc(p=3, m=m, df=df)[[4]],MSPEfunc(p=4, m=m, df=df)[[4]],MSPEfunc(p=5, m=m, df=df)[[4]],MSPEfunc(p=6, m=m, df=df)[[4]] )
LowerMSPE <- MSPE - qt(.025, 499)*MSPESD/sqrt(500)
LowerMAPE <- MAPE - qt(.025, 499)*MAPESD/sqrt(500)
UpperMSPE <- MSPE + qt(.025, 499)*MSPESD/sqrt(500)
UpperMAPE <- MAPE + qt(.025, 499)*MAPESD/sqrt(500)
df <- data.frame(p, Technique, MSPE, MAPE, LowerMSPE, LowerMAPE, UpperMSPE, UpperMAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL1m2 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+
  geom_errorbar(aes(ymin = LowerMSPE, ymax = UpperMSPE), position = dodge, width = 0.05) + theme(legend.position = "bottom")+ggtitle("a) m=0.40")+ theme(plot.title = element_text(hjust = 0.5))
RL1m2a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+
  geom_errorbar(aes(ymin = LowerMAPE, ymax = UpperMAPE), position = dodge, width = 0.05)+ theme(legend.position = "bottom")+ggtitle("a) m=0.40")+ theme(plot.title = element_text(hjust = 0.5))
RL1m2df <- df

#RL1 (m=0.6)
df <- RL1
m <- 3
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
MSPESD <- c(MSPEfunc(p=1, m=m, df=df)[[3]],MSPEfunc(p=2, m=m, df=df)[[3]],MSPEfunc(p=3, m=m, df=df)[[3]],MSPEfunc(p=4, m=m, df=df)[[3]],MSPEfunc(p=5, m=m, df=df)[[3]],MSPEfunc(p=6, m=m, df=df)[[3]] )
MAPESD <- c(MSPEfunc(p=1, m=m, df=df)[[4]],MSPEfunc(p=2, m=m, df=df)[[4]],MSPEfunc(p=3, m=m, df=df)[[4]],MSPEfunc(p=4, m=m, df=df)[[4]],MSPEfunc(p=5, m=m, df=df)[[4]],MSPEfunc(p=6, m=m, df=df)[[4]] )
LowerMSPE <- MSPE - qt(.025, 499)*MSPESD/sqrt(500)
LowerMAPE <- MAPE - qt(.025, 499)*MAPESD/sqrt(500)
UpperMSPE <- MSPE + qt(.025, 499)*MSPESD/sqrt(500)
UpperMAPE <- MAPE + qt(.025, 499)*MAPESD/sqrt(500)
df <- data.frame(p, Technique, MSPE, MAPE, LowerMSPE, LowerMAPE, UpperMSPE, UpperMAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL1m3 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+
  geom_errorbar(aes(ymin = LowerMSPE, ymax = UpperMSPE), position = dodge, width = 0.05) + theme(legend.position = "bottom")+ggtitle("a) m=0.60")+ theme(plot.title = element_text(hjust = 0.5))
RL1m3a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+
  geom_errorbar(aes(ymin = LowerMAPE, ymax = UpperMAPE), position = dodge, width = 0.05)+ theme(legend.position = "bottom")+ggtitle("a) m=0.60")+ theme(plot.title = element_text(hjust = 0.5))
RL1m3df <- df


#RL1 (m=0.8)
df <- RL1
m <- 4
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
MSPESD <- c(MSPEfunc(p=1, m=m, df=df)[[3]],MSPEfunc(p=2, m=m, df=df)[[3]],MSPEfunc(p=3, m=m, df=df)[[3]],MSPEfunc(p=4, m=m, df=df)[[3]],MSPEfunc(p=5, m=m, df=df)[[3]],MSPEfunc(p=6, m=m, df=df)[[3]] )
MAPESD <- c(MSPEfunc(p=1, m=m, df=df)[[4]],MSPEfunc(p=2, m=m, df=df)[[4]],MSPEfunc(p=3, m=m, df=df)[[4]],MSPEfunc(p=4, m=m, df=df)[[4]],MSPEfunc(p=5, m=m, df=df)[[4]],MSPEfunc(p=6, m=m, df=df)[[4]] )
LowerMSPE <- MSPE - qt(.025, 499)*MSPESD/sqrt(500)
LowerMAPE <- MAPE - qt(.025, 499)*MAPESD/sqrt(500)
UpperMSPE <- MSPE + qt(.025, 499)*MSPESD/sqrt(500)
UpperMAPE <- MAPE + qt(.025, 499)*MAPESD/sqrt(500)
df <- data.frame(p, Technique, MSPE, MAPE, LowerMSPE, LowerMAPE, UpperMSPE, UpperMAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL1m4 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+
  geom_errorbar(aes(ymin = LowerMSPE, ymax = UpperMSPE), position = dodge, width = 0.05) + theme(legend.position = "bottom")+ggtitle("a) m=0.80")+ theme(plot.title = element_text(hjust = 0.5))
RL1m4a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+
  geom_errorbar(aes(ymin = LowerMAPE, ymax = UpperMAPE), position = dodge, width = 0.05)+ theme(legend.position = "bottom")+ggtitle("a) m=0.80")+ theme(plot.title = element_text(hjust = 0.5))
RL1m4df <- df

save(RL1m1df, RL1m2df, RL1m3df, RL1m4df, file="RL1_Results.Rdata")
rm(RL1)
########################################################################################################


########################################################################################################
#Simulation RL2
load("RL2.Rdata")
df <- RL2
#RL2 (m=0.15)
m <- 1
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
MSPESD <- c(MSPEfunc(p=1, m=m, df=df)[[3]],MSPEfunc(p=2, m=m, df=df)[[3]],MSPEfunc(p=3, m=m, df=df)[[3]],MSPEfunc(p=4, m=m, df=df)[[3]],MSPEfunc(p=5, m=m, df=df)[[3]],MSPEfunc(p=6, m=m, df=df)[[3]] )
MAPESD <- c(MSPEfunc(p=1, m=m, df=df)[[4]],MSPEfunc(p=2, m=m, df=df)[[4]],MSPEfunc(p=3, m=m, df=df)[[4]],MSPEfunc(p=4, m=m, df=df)[[4]],MSPEfunc(p=5, m=m, df=df)[[4]],MSPEfunc(p=6, m=m, df=df)[[4]] )
LowerMSPE <- MSPE - qt(.025, 499)*MSPESD/sqrt(500)
LowerMAPE <- MAPE - qt(.025, 499)*MAPESD/sqrt(500)
UpperMSPE <- MSPE + qt(.025, 499)*MSPESD/sqrt(500)
UpperMAPE <- MAPE + qt(.025, 499)*MAPESD/sqrt(500)
df <- data.frame(p, Technique, MSPE, MAPE, LowerMSPE, LowerMAPE, UpperMSPE, UpperMAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL2m1 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+
  geom_errorbar(aes(ymin = LowerMSPE, ymax = UpperMSPE), position = dodge, width = 0.05) + theme(legend.position = "bottom")+ggtitle("a) m=0.15")+ theme(plot.title = element_text(hjust = 0.5))
RL2m1a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+
  geom_errorbar(aes(ymin = LowerMAPE, ymax = UpperMAPE), position = dodge, width = 0.05)+ theme(legend.position = "bottom")+ggtitle("a) m=0.15")+ theme(plot.title = element_text(hjust = 0.5))
RL2m1df <- df

#RL2 (m=0.3)
df <- RL2
m <- 2
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
MSPESD <- c(MSPEfunc(p=1, m=m, df=df)[[3]],MSPEfunc(p=2, m=m, df=df)[[3]],MSPEfunc(p=3, m=m, df=df)[[3]],MSPEfunc(p=4, m=m, df=df)[[3]],MSPEfunc(p=5, m=m, df=df)[[3]],MSPEfunc(p=6, m=m, df=df)[[3]] )
MAPESD <- c(MSPEfunc(p=1, m=m, df=df)[[4]],MSPEfunc(p=2, m=m, df=df)[[4]],MSPEfunc(p=3, m=m, df=df)[[4]],MSPEfunc(p=4, m=m, df=df)[[4]],MSPEfunc(p=5, m=m, df=df)[[4]],MSPEfunc(p=6, m=m, df=df)[[4]] )
LowerMSPE <- MSPE - qt(.025, 499)*MSPESD/sqrt(500)
LowerMAPE <- MAPE - qt(.025, 499)*MAPESD/sqrt(500)
UpperMSPE <- MSPE + qt(.025, 499)*MSPESD/sqrt(500)
UpperMAPE <- MAPE + qt(.025, 499)*MAPESD/sqrt(500)
df <- data.frame(p, Technique, MSPE, MAPE, LowerMSPE, LowerMAPE, UpperMSPE, UpperMAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL2m2 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+
  geom_errorbar(aes(ymin = LowerMSPE, ymax = UpperMSPE), position = dodge, width = 0.05) + theme(legend.position = "bottom")+ggtitle("a) m=0.30")+ theme(plot.title = element_text(hjust = 0.5))
RL2m2a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+
  geom_errorbar(aes(ymin = LowerMAPE, ymax = UpperMAPE), position = dodge, width = 0.05)+ theme(legend.position = "bottom")+ggtitle("a) m=0.30")+ theme(plot.title = element_text(hjust = 0.5))
RL2m2df <- df

#RL2 (m=0.45)
df <- RL2
m <- 3
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
MSPESD <- c(MSPEfunc(p=1, m=m, df=df)[[3]],MSPEfunc(p=2, m=m, df=df)[[3]],MSPEfunc(p=3, m=m, df=df)[[3]],MSPEfunc(p=4, m=m, df=df)[[3]],MSPEfunc(p=5, m=m, df=df)[[3]],MSPEfunc(p=6, m=m, df=df)[[3]] )
MAPESD <- c(MSPEfunc(p=1, m=m, df=df)[[4]],MSPEfunc(p=2, m=m, df=df)[[4]],MSPEfunc(p=3, m=m, df=df)[[4]],MSPEfunc(p=4, m=m, df=df)[[4]],MSPEfunc(p=5, m=m, df=df)[[4]],MSPEfunc(p=6, m=m, df=df)[[4]] )
LowerMSPE <- MSPE - qt(.025, 499)*MSPESD/sqrt(500)
LowerMAPE <- MAPE - qt(.025, 499)*MAPESD/sqrt(500)
UpperMSPE <- MSPE + qt(.025, 499)*MSPESD/sqrt(500)
UpperMAPE <- MAPE + qt(.025, 499)*MAPESD/sqrt(500)
df <- data.frame(p, Technique, MSPE, MAPE, LowerMSPE, LowerMAPE, UpperMSPE, UpperMAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL2m3 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+
  geom_errorbar(aes(ymin = LowerMSPE, ymax = UpperMSPE), position = dodge, width = 0.05) + theme(legend.position = "bottom")+ggtitle("a) m=0.45")+ theme(plot.title = element_text(hjust = 0.5))
RL2m3a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+
  geom_errorbar(aes(ymin = LowerMAPE, ymax = UpperMAPE), position = dodge, width = 0.05)+ theme(legend.position = "bottom")+ggtitle("a) m=0.45")+ theme(plot.title = element_text(hjust = 0.5))
RL2m3df <- df


#RL2 (m=0.6)
df <- RL2
m <- 4
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
MSPESD <- c(MSPEfunc(p=1, m=m, df=df)[[3]],MSPEfunc(p=2, m=m, df=df)[[3]],MSPEfunc(p=3, m=m, df=df)[[3]],MSPEfunc(p=4, m=m, df=df)[[3]],MSPEfunc(p=5, m=m, df=df)[[3]],MSPEfunc(p=6, m=m, df=df)[[3]] )
MAPESD <- c(MSPEfunc(p=1, m=m, df=df)[[4]],MSPEfunc(p=2, m=m, df=df)[[4]],MSPEfunc(p=3, m=m, df=df)[[4]],MSPEfunc(p=4, m=m, df=df)[[4]],MSPEfunc(p=5, m=m, df=df)[[4]],MSPEfunc(p=6, m=m, df=df)[[4]] )
LowerMSPE <- MSPE - qt(.025, 499)*MSPESD/sqrt(500)
LowerMAPE <- MAPE - qt(.025, 499)*MAPESD/sqrt(500)
UpperMSPE <- MSPE + qt(.025, 499)*MSPESD/sqrt(500)
UpperMAPE <- MAPE + qt(.025, 499)*MAPESD/sqrt(500)
df <- data.frame(p, Technique, MSPE, MAPE, LowerMSPE, LowerMAPE, UpperMSPE, UpperMAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL2m4 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+
  geom_errorbar(aes(ymin = LowerMSPE, ymax = UpperMSPE), position = dodge, width = 0.05) + theme(legend.position = "bottom")+ggtitle("a) m=0.60")+ theme(plot.title = element_text(hjust = 0.5))
RL2m4a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+
  geom_errorbar(aes(ymin = LowerMAPE, ymax = UpperMAPE), position = dodge, width = 0.05)+ theme(legend.position = "bottom")+ggtitle("a) m=0.60")+ theme(plot.title = element_text(hjust = 0.5))
RL2m4df <- df


save(RL2m1df, RL2m2df, RL2m3df, RL2m4df, file="RL2_Results.Rdata")

########################################################################################################
#Li & Martin 1 Results
load("LM1.Rdata")

df <- LM1
m <- 1
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
MSPESD <- c(MSPEfunc(p=1, m=m, df=df)[[3]],MSPEfunc(p=2, m=m, df=df)[[3]],MSPEfunc(p=3, m=m, df=df)[[3]],MSPEfunc(p=4, m=m, df=df)[[3]],MSPEfunc(p=5, m=m, df=df)[[3]],MSPEfunc(p=6, m=m, df=df)[[3]] )
MAPESD <- c(MSPEfunc(p=1, m=m, df=df)[[4]],MSPEfunc(p=2, m=m, df=df)[[4]],MSPEfunc(p=3, m=m, df=df)[[4]],MSPEfunc(p=4, m=m, df=df)[[4]],MSPEfunc(p=5, m=m, df=df)[[4]],MSPEfunc(p=6, m=m, df=df)[[4]] )
LowerMSPE <- MSPE - qt(.025, 499)*MSPESD/sqrt(500)
LowerMAPE <- MAPE - qt(.025, 499)*MAPESD/sqrt(500)
UpperMSPE <- MSPE + qt(.025, 499)*MSPESD/sqrt(500)
UpperMAPE <- MAPE + qt(.025, 499)*MAPESD/sqrt(500)
df <- data.frame(p, Technique, MSPE, MAPE, LowerMSPE, LowerMAPE, UpperMSPE, UpperMAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
LM1plot <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+
  geom_errorbar(aes(ymin = LowerMSPE, ymax = UpperMSPE), position = dodge, width = 0.05) + theme(legend.position = "bottom")+ggtitle("a) Uncorrelated Predictors")+ theme(plot.title = element_text(hjust = 0.5))
LM1aplot <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+
  geom_errorbar(aes(ymin = LowerMAPE, ymax = UpperMAPE), position = dodge, width = 0.05)+ theme(legend.position = "bottom")+ggtitle("b) Uncorrelated Predictors")+ theme(plot.title = element_text(hjust = 0.5))
LM1mdf <- df


load("LM2.Rdata")

df <- LM2
m <- 1
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
MSPESD <- c(MSPEfunc(p=1, m=m, df=df)[[3]],MSPEfunc(p=2, m=m, df=df)[[3]],MSPEfunc(p=3, m=m, df=df)[[3]],MSPEfunc(p=4, m=m, df=df)[[3]],MSPEfunc(p=5, m=m, df=df)[[3]],MSPEfunc(p=6, m=m, df=df)[[3]] )
MAPESD <- c(MSPEfunc(p=1, m=m, df=df)[[4]],MSPEfunc(p=2, m=m, df=df)[[4]],MSPEfunc(p=3, m=m, df=df)[[4]],MSPEfunc(p=4, m=m, df=df)[[4]],MSPEfunc(p=5, m=m, df=df)[[4]],MSPEfunc(p=6, m=m, df=df)[[4]] )
LowerMSPE <- MSPE - qt(.025, 499)*MSPESD/sqrt(500)
LowerMAPE <- MAPE - qt(.025, 499)*MAPESD/sqrt(500)
UpperMSPE <- MSPE + qt(.025, 499)*MSPESD/sqrt(500)
UpperMAPE <- MAPE + qt(.025, 499)*MAPESD/sqrt(500)
df <- data.frame(p, Technique, MSPE, MAPE, LowerMSPE, LowerMAPE, UpperMSPE, UpperMAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
LM2plot <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+
  geom_errorbar(aes(ymin = LowerMSPE, ymax = UpperMSPE), position = dodge, width = 0.05) + theme(legend.position = "bottom")+ggtitle("a) Correlated Predictors")+ theme(plot.title = element_text(hjust = 0.5))
LM2aplot <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+
  geom_errorbar(aes(ymin = LowerMAPE, ymax = UpperMAPE), position = dodge, width = 0.05)+ theme(legend.position = "bottom")+ggtitle("b) Correlated Predictors")+ theme(plot.title = element_text(hjust = 0.5))
LM2mdf <- df


save(LM1mdf, LM2mdf, file="LM_Results.Rdata")

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

load("RL2.Rdata")

#Used to create ERR by p plots for each type of tuning
TuningResFunc <- function(p,m,r,Crit){
  ERR <- rep(NA,3)
  if(Crit=="MSPE"){
  ERR[1] <- df[3,p,m,r][[1]][[6]][1,1]  #all cases no weighting in CV
  ERR[2] <- df[3,p,m,r][[1]][[6]][1,3]  #all cases with weighted CV
  ERR[3] <- df[3,p,m,r][[1]][[6]][3,1]  #only non-outliers (ideal scenario)
  } else{
  ERR[1] <- df[3,p,m,r][[1]][[6]][1,2]  #all cases no weighting in CV
  ERR[2] <- df[3,p,m,r][[1]][[6]][1,4]  #all cases with weighted CV
  ERR[3] <- df[3,p,m,r][[1]][[6]][3,2]  #only non-outliers (ideal scenario)
  }
return(ERR)
}

#Used to plot avg. error against alpha
ERRbyAlpha <- function(p,m,r,Crit){
  if(Crit=="MSPE"){
    df <- df[3,p,m,r][[1]][[2]][,1]
  } else{
    df <- df[3,p,m,r][[1]][[2]][,2]
  }
  return(df)
}

#used for boxplots with alphas
GetAlpha <- function(df, p,m,r,Crit){
  Res <- c(NA, NA)
  if(Crit=="MSPE"){
    Res[1] <- df[3,p,m,r][[1]][[3]][1,1]  #unweighted
    Res[2] <- df[3,p,m,r][[1]][[3]][1,3]  #weighted
  } else{
    Res[1] <- df[3,p,m,r][[1]][[3]][1,2]
    Res[2] <- df[3,p,m,r][[1]][[3]][1,4]
  }
  return(Res)
}

ntechs <- 3
nprops <- 6
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntechs))
Techs=c("Unweighted CV", "Weighted CV", "Noncontaminated Cases")
Technique <- c(rep(Techs, nprops))

nreps <- 5
#m=1
MSPEarray <- sapply(1:6, simplify="array", function(prop){sapply(1:nreps, FUN=TuningResFunc, p=prop, m=1, Crit="MSPE")})
MAPEarray <- sapply(1:6, simplify="array", function(prop){sapply(1:nreps, FUN=TuningResFunc, p=prop, m=1, Crit="MAPE")})
MSPE <- c(apply(MSPEarray,c(1,3),mean))  #Row1=unwt CV, row2= wt. CV, row3=nonoutliers, cols index p
MAPE <- c(apply(MAPEarray,c(1,3),mean))  #Row1=unwt CV, row2= wt. CV, row3=nonoutliers, cols index p
df <- data.frame(p, Technique, MSPE, MAPE)
df$Technique <- rep(c("Unweighted CV", "Weighted CV", "Noncontaminated Cases"),6)
RL2m1CVdf <- df
RL2m1CV <- ggplot(RL2m1CVdf, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.15")+ theme(plot.title = element_text(hjust = 0.5))
RL2m1CVa <- ggplot(RL2m1CVdf, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.15")+ theme(plot.title = element_text(hjust = 0.5))


#m=2
MSPEarray <- sapply(1:6, simplify="array", function(prop){sapply(1:500, FUN=TuningResFunc, p=prop, m=2, Crit="MSPE")})
MAPEarray <- sapply(1:6, simplify="array", function(prop){sapply(1:500, FUN=TuningResFunc, p=prop, m=2, Crit="MAPE")})
MSPE <- c(apply(MSPEarray,c(1,3),mean))  #Row1=unwt CV, row2= wt. CV, row3=nonoutliers, cols index p
MAPE <- c(apply(MAPEarray,c(1,3),mean))  #Row1=unwt CV, row2= wt. CV, row3=nonoutliers, cols index p
df <- data.frame(p, Technique, MSPE, MAPE)
df$Technique <- rep(c("Unweighted CV", "Weighted CV", "Noncontaminated Cases"),6)
RL2m2CVdf <- df
RL2m2CV <- ggplot(RL2m2CVdf, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.3")+ theme(plot.title = element_text(hjust = 0.5))
RL2m2CVa <- ggplot(RL2m2CVdf, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.3")+ theme(plot.title = element_text(hjust = 0.5))

#m=3
MSPEarray <- sapply(1:6, simplify="array", function(prop){sapply(1:500, FUN=TuningResFunc, p=prop, m=3, Crit="MSPE")})
MAPEarray <- sapply(1:6, simplify="array", function(prop){sapply(1:500, FUN=TuningResFunc, p=prop, m=3, Crit="MAPE")})
MSPE <- c(apply(MSPEarray,c(1,3),mean))  #Row1=unwt CV, row2= wt. CV, row3=nonoutliers, cols index p
MAPE <- c(apply(MAPEarray,c(1,3),mean))  #Row1=unwt CV, row2= wt. CV, row3=nonoutliers, cols index p
df <- data.frame(p, Technique, MSPE, MAPE)
df$Technique <- rep(c("Unweighted CV", "Weighted CV", "Noncontaminated Cases"),6)
RL2m3CVdf <- df
RL2m3CV <- ggplot(RL2m3CVdf, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.45")+ theme(plot.title = element_text(hjust = 0.5))
RL2m3CVa <- ggplot(RL2m3CVdf, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.45")+ theme(plot.title = element_text(hjust = 0.5))

#m=4
MSPEarray <- sapply(1:6, simplify="array", function(prop){sapply(1:500, FUN=TuningResFunc, p=prop, m=4, Crit="MSPE")})
MAPEarray <- sapply(1:6, simplify="array", function(prop){sapply(1:500, FUN=TuningResFunc, p=prop, m=4, Crit="MAPE")})
MSPE <- c(apply(MSPEarray,c(1,3),mean))  #Row1=unwt CV, row2= wt. CV, row3=nonoutliers, cols index p
MAPE <- c(apply(MAPEarray,c(1,3),mean))  #Row1=unwt CV, row2= wt. CV, row3=nonoutliers, cols index p
df <- data.frame(p, Technique, MSPE, MAPE)
df$Technique <- rep(c("Unweighted CV", "Weighted CV", "Noncontaminated Cases"),6)
RL2m4CVdf <- df
RL2m4CV <- ggplot(RL2m4CVdf, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.6")+ theme(plot.title = element_text(hjust = 0.5))
RL2m4CVa <- ggplot(RL2m4CVdf, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.6")+ theme(plot.title = element_text(hjust = 0.5))

##########################################################################################################
#Plot ERR by alpha
alpha <- seq(from=1, to=30, by=0.25)
alpha <- seq(from=3, to=300, by=0.25)

#for m=1, p=6
AlphERR <- sapply(1:500, FUN=ERRbyAlpha, p=6, m=1, Crit="MSPE")
MSPE <- apply(AlphERR, 1, mean)[3:597]
df <- data.frame(MSPE, alpha)
pl <- ggplot(df, aes(x = alpha, y = MSPE)) + geom_line()+geom_line()
pl

#for m=1, p=6
AlphERR <- sapply(1:500, FUN=ERRbyAlpha, p=6, m=4, Crit="MSPE")
MSPE <- apply(AlphERR, 1, mean)[3:119]
df <- data.frame(MSPE, alpha)
pl <- ggplot(df, aes(x = alpha, y = MSPE)) + geom_line()+geom_line()
pl

############################################################################################################
#Plot ERR by alpha

RL2[3,1,1,1]

GetAlpha(1,1,1,"MSPE")

#m=1
Alphas <- c(sapply(1:500, FUN=GetAlpha, p=6, m=1, Crit="MSPE"))
Technique <- rep(c("Unweighted CV", "Weighted CV"),500)
df <- data.frame(Alphas, Technique)
pl <- ggplot(df, aes(x = Technique, y = Alphas)) + geom_boxplot()+ylim(c(0,30))
pl

#m=6
Alphas <- c(sapply(1:500, FUN=GetAlpha, p=6, m=4, Crit="MSPE"))
Technique <- rep(c("Unweighted CV", "Weighted CV"),500)
df <- data.frame(Alphas, Technique)
pl <- ggplot(df, aes(x = Technique, y = Alphas)) + geom_boxplot()+ylim(c(0,30))
pl
