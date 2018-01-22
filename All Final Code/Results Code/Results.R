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
#Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA")


ntecs <- 15
nprops <- 6


##################################################################################
#Roy-Larocque Simulation 1:m=0.20
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntecs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA")
Technique <- c(rep(Techs, nprops))
load("Sim1m20p0.Rdata")
MSPE <- apply(ERRMat[,,1], 2, mean, na.rm=T)
MAPE <- apply(ERRMat[,,2], 2, mean, na.rm=T)
load("Sim1m20p05.Rdata")   
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T)) #Note we're adding this to existing version of MSPE
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim1m20p10.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim1m20p15.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim1m20p20.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
load("Sim1m20p25.Rdata")
MSPE <- c(MSPE, apply(ERRMat[,,1], 2, mean, na.rm=T))
MAPE <- c(MAPE, apply(ERRMat[,,2], 2, mean, na.rm=T))
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL1m1 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.20")+ theme(plot.title = element_text(hjust = 0.5))
RL1m1a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.20")+ theme(plot.title = element_text(hjust = 0.5))

#Roy-Larocque Simulation 2:m=0.40
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntecs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA")
Technique <- c(rep(Techs, nprops))
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
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL1m2 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("b) m=0.40")+ theme(plot.title = element_text(hjust = 0.5))
RL1m2a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("b) m=0.40")+ theme(plot.title = element_text(hjust = 0.5))


#Roy-Larocque Simulation 2:m=0.60
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntecs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA")
Technique <- c(rep(Techs, nprops))
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
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL1m3 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("c) m=0.60")+ theme(plot.title = element_text(hjust = 0.5))
RL1m3a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("c) m=0.60")+ theme(plot.title = element_text(hjust = 0.5))

#Roy-Larocque Simulation 1:m=0.8
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntecs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA")
Technique <- c(rep(Techs, nprops))
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
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL1m4 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("d) m=0.80")+ theme(plot.title = element_text(hjust = 0.5))
RL1m4a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("d) m=0.80")+ theme(plot.title = element_text(hjust = 0.5))

p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntecs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA")
Technique <- c(rep(Techs, nprops))
load("Sim2m15p0.Rdata")
MSPE <- apply(ERRMat[,,1], 2, mean, na.rm=T)
MAPE <- apply(ERRMat[,,2], 2, mean, na.rm=T)
load("Sim2m15p05.Rdata")   #Need to fix this to p05 when sim is run
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
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL2m1 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.15")+ theme(plot.title = element_text(hjust = 0.5))
RL2m1a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.15")+ theme(plot.title = element_text(hjust = 0.5))

#Roy-Larocque Simulation 2:m=0.30
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntecs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA")
Technique <- c(rep(Techs, nprops))
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
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL2m2 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("b) m=0.30")+ theme(plot.title = element_text(hjust = 0.5))
RL2m2a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("b) m=0.30")+ theme(plot.title = element_text(hjust = 0.5))


#Roy-Larocque Simulation 2:m=0.45
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntecs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA")
Technique <- c(rep(Techs, nprops))
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
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL2m3 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("c) m=0.45")+ theme(plot.title = element_text(hjust = 0.5))
RL2m3a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("c) m=0.45")+ theme(plot.title = element_text(hjust = 0.5))

#Roy-Larocque Simulation 2:m=0.6
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntecs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA")
Technique <- c(rep(Techs, nprops))
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
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
RL2m4 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("d) m=0.60")+ theme(plot.title = element_text(hjust = 0.5))
RL2m4a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("d) m=0.60")+ theme(plot.title = element_text(hjust = 0.5))

#######################################################################################

#Li-Martin Simulation 1
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntecs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA")
Technique <- c(rep(Techs, nprops))
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
df <- data.frame(p, Technique, MSPE, MAPE)
df1 <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df1$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
LM1 <- ggplot(df1, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ theme(plot.title = element_text(hjust = 0.5))
df2 <- subset(df, Technique%in%Techs[c(2,3,5,6,7,13)])
df2$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
LM1a <- ggplot(df2, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ theme(plot.title = element_text(hjust = 0.5))+ggtitle("a) Simulation 3")


#Li-Martin Simulation 2
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntecs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA")
Technique <- c(rep(Techs, nprops))
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
df <- data.frame(p, Technique, MSPE, MAPE)
df1 <- subset(df, Technique%in%Techs[c(2,3,5,6,7,12)])
df1$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
LM2 <- ggplot(df1, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ theme(plot.title = element_text(hjust = 0.5))
df2 <- subset(df, Technique%in%Techs[c(2,3,5,6,7,13)])
df2$Technique <- rep(c("RF", "QRF", "Huber", "Mean-Med. Agg.", "Med-Med Agg." ,"RF-LOWESS"),6)
LM2a <- ggplot(df2, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ theme(plot.title = element_text(hjust = 0.5))+ggtitle("b) Simulation 4")



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


##################
#Li-Martin-MSPE

mylegend<-g_legend(RL1m1)

p <- grid.arrange(arrangeGrob(LM1 + theme(legend.position="none")+ggtitle("a) Simulation 3"),
                              LM2 + theme(legend.position="none")+ggtitle("b) Simulation 4"),
                              LM1 + theme(legend.position="none")+ylim(c(7,12))+ggtitle("c) Simulation 3"),
                              LM2 + theme(legend.position="none")+ylim(c(7,12))+ggtitle("d) Simulation 4"),
                              nrow=2),
                  mylegend, nrow=2,heights=c(10, 2))
p

#Li-Martin-MAPE

mylegend<-g_legend(RL1m1)

p <- grid.arrange(arrangeGrob(LM1a + theme(legend.position="none"),
                              LM2a + theme(legend.position="none"),
                              nrow=1),
                  mylegend, nrow=2,heights=c(10, 2))
p
