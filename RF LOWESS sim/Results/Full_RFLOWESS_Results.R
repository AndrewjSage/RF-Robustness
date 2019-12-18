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
    MSPESD <- apply(apply(A,1,CalculateMSPE), 1, sd)/sqrt(dim(A)[1]) #divide by sqrt(n)
    MAPESD <- apply(apply(A,1,CalculateMAPE), 1, sd)/sqrt(dim(A)[1])
    return(list(MSPE, MAPE))
    }

ntechs <- 16
nprops <- 6
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntechs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA", "Truth")
Technique <- c(rep(Techs, nprops))

########################################################################################################
#Simulation RL1
df <- RL1
#RL1 (m=0.2)
m <- 1
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2:8,12)])
df$Technique <- rep(c("RF", "QRF", "Tukey", "Huber", "Mean-Med. Agg.", "Med-Med Agg.", "Med.-Mean Agg.","RF-LOWESS"),6)
RL1m1 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.20")+ theme(plot.title = element_text(hjust = 0.5))
RL1m1a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.20")+ theme(plot.title = element_text(hjust = 0.5))
RL1m1df <- df

#RL1 (m=0.4)
df <- RL1
m <- 2
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2:8,12)])
df$Technique <- rep(c("RF", "QRF", "Tukey", "Huber", "Mean-Med. Agg.", "Med-Med Agg.", "Med.-Mean Agg.","RF-LOWESS"),6)
RL1m2 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.40")+ theme(plot.title = element_text(hjust = 0.5))
RL1m2a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.40")+ theme(plot.title = element_text(hjust = 0.5))
RL1m2df <- df

#RL1 (m=0.6)
df <- RL1
m <- 3
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2:8,12)])
df$Technique <- rep(c("RF", "QRF", "Tukey", "Huber", "Mean-Med. Agg.", "Med-Med Agg.", "Med.-Mean Agg.","RF-LOWESS"),6)
RL1m3 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.60")+ theme(plot.title = element_text(hjust = 0.5))
RL1m3a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.60")+ theme(plot.title = element_text(hjust = 0.5))
RL1m3df <- df


#RL1 (m=0.8)
df <- RL1
m <- 4
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2:8,12)])
df$Technique <- rep(c("RF", "QRF", "Tukey", "Huber", "Mean-Med. Agg.", "Med-Med Agg.", "Med.-Mean Agg.","RF-LOWESS"),6)
RL1m4 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.80")+ theme(plot.title = element_text(hjust = 0.5))
RL1m4a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.80")+ theme(plot.title = element_text(hjust = 0.5))
RL1m4df <- df

save(RL1m1df, RL1m2df, RL1m3df, RL1m4df, file="RL1_Results_Full.Rdata")
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
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2:8,12)])
df$Technique <- rep(c("RF", "QRF", "Tukey", "Huber", "Mean-Med. Agg.", "Med-Med Agg.", "Med.-Mean Agg.","RF-LOWESS"),6)
RL2m1df <- df
RL2m1 <- ggplot(RL2m1df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.15")+ theme(plot.title = element_text(hjust = 0.5))
RL2m1a <- ggplot(RL2m1df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.15")+ theme(plot.title = element_text(hjust = 0.5))

#RL2 (m=0.3)
df <- RL2
m <- 2
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2:8,12)])
df$Technique <- rep(c("RF", "QRF", "Tukey", "Huber", "Mean-Med. Agg.", "Med-Med Agg.", "Med.-Mean Agg.","RF-LOWESS"),6)
RL2m2df <- df
RL2m2 <- ggplot(RL2m2df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.30")+ theme(plot.title = element_text(hjust = 0.5))
RL2m2a <- ggplot(RL2m2df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.30")+ theme(plot.title = element_text(hjust = 0.5))

#RL3 (m=0.45)
df <- RL2
m <- 3
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2:8,12)])
df$Technique <- rep(c("RF", "QRF", "Tukey", "Huber", "Mean-Med. Agg.", "Med-Med Agg.", "Med.-Mean Agg.","RF-LOWESS"),6)
RL2m3df <- df
RL2m3 <- ggplot(RL2m3df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.45")+ theme(plot.title = element_text(hjust = 0.5))
RL2m3a <- ggplot(RL2m3df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.45")+ theme(plot.title = element_text(hjust = 0.5))


#RL4 (m=0.6)
df <- RL2
m <- 4
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2:8,12)])
df$Technique <- rep(c("RF", "QRF", "Tukey", "Huber", "Mean-Med. Agg.", "Med-Med Agg.", "Med.-Mean Agg.","RF-LOWESS"),6)
RL2m4df <- df
RL2m4 <- ggplot(RL2m4df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.60")+ theme(plot.title = element_text(hjust = 0.5))
RL2m4a <- ggplot(RL2m4df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.60")+ theme(plot.title = element_text(hjust = 0.5))


save(RL2m1df, RL2m2df, RL2m3df, RL2m4df, file="RL2_Results_Full.Rdata")

########################################################################################################
#Li & Martin 1 Results
load("LM1.Rdata")

df <- LM1
m <- 1
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2:8,12)])
df$Technique <- rep(c("RF", "QRF", "Tukey", "Huber", "Mean-Med. Agg.", "Med-Med Agg.", "Med.-Mean Agg.","RF-LOWESS"),6)
LM1mdf <- df
LM1plot <- ggplot(LM1mdf, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("LM1-MSPE")+ theme(plot.title = element_text(hjust = 0.5))+ylim(c(7,13))
LM1aplot <- ggplot(LM1mdf, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("LM1-MAPE")+ theme(plot.title = element_text(hjust = 0.5))+ylim(c(2,2.5))


load("LM2.Rdata")

df <- LM2
m <- 1
MSPE <- c(MSPEfunc(p=1, m=m, df=df)[[1]],MSPEfunc(p=2, m=m, df=df)[[1]],MSPEfunc(p=3, m=m, df=df)[[1]],MSPEfunc(p=4, m=m, df=df)[[1]],MSPEfunc(p=5, m=m, df=df)[[1]],MSPEfunc(p=6, m=m, df=df)[[1]] )
MAPE <- c(MSPEfunc(p=1, m=m, df=df)[[2]],MSPEfunc(p=2, m=m, df=df)[[2]],MSPEfunc(p=3, m=m, df=df)[[2]],MSPEfunc(p=4, m=m, df=df)[[2]],MSPEfunc(p=5, m=m, df=df)[[2]],MSPEfunc(p=6, m=m, df=df)[[2]] )
df <- data.frame(p, Technique, MSPE, MAPE)
df <- subset(df, Technique%in%Techs[c(2:8,12)])
df$Technique <- rep(c("RF", "QRF", "Tukey", "Huber", "Mean-Med. Agg.", "Med-Med Agg.", "Med.-Mean Agg.","RF-LOWESS"),6)
LM2mdf <- df
LM2plot <- ggplot(LM2mdf, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("LM2-MSPE")+ theme(plot.title = element_text(hjust = 0.5))+ylim(c(8,11))
LM2aplot <- ggplot(LM2mdf, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("LM2-MAPE")+ theme(plot.title = element_text(hjust = 0.5))+ylim(c(1.75,2.25))


save(LM1mdf, LM2mdf, file="LM_Results_Full.Rdata")

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

