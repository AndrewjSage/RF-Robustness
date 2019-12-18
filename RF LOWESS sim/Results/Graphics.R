setwd("~/Box Sync/Iowa State/Research/Robustness of Random Forest/RFLOWESS sim/Results")

library(tidyverse)

ntechs <- 16
nprops <- 6
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntechs))
Techs=c("Y-bar", "RF", "QRF", "Li-Martin(Tukey)", "Li-Martin(Huber)","Mean-Med.", "Med.-Med.", "Med.-Mean","LOWESS-6","LOWESS-U","LOWESS-UA", "LOWESS-RF", "LOWESS-RFA", "LOWESS-L", "LOWESS-LA", "Truth")
Technique <- c(rep(Techs, nprops))

################################################################################################
#Roy-Larocque 1
load("RL1_Results.Rdata")

df <- RL1m1df
RL1m1 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) s=0.20")+ theme(plot.title = element_text(hjust = 0.5))
RL1m1a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) s=0.20")+ theme(plot.title = element_text(hjust = 0.5))

df <- RL1m2df
RL1m2 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("b) s=0.40")+ theme(plot.title = element_text(hjust = 0.5))
RL1m2a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("b) s=0.40")+ theme(plot.title = element_text(hjust = 0.5))

df <- RL1m3df
RL1m3 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("c) s=0.60")+ theme(plot.title = element_text(hjust = 0.5))
RL1m3a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("c) s=0.60")+ theme(plot.title = element_text(hjust = 0.5))

df <- RL1m4df
RL1m4 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("d) s=0.80")+ theme(plot.title = element_text(hjust = 0.5))
RL1m4a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("d) s=0.80")+ theme(plot.title = element_text(hjust = 0.5))

################################################################################################
#Roy-Larocque 2
load("RL2_Results.Rdata")

df <- RL2m1df
RL2m1 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) s=0.15")+ theme(plot.title = element_text(hjust = 0.5)) 
RL2m1a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) s=0.15")+ theme(plot.title = element_text(hjust = 0.5))

df <- RL2m2df
RL2m2 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("b) s=0.30")+ theme(plot.title = element_text(hjust = 0.5))
RL2m2a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("b) s=0.30")+ theme(plot.title = element_text(hjust = 0.5))

df <- RL2m3df
RL2m3 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("c) s=0.45")+ theme(plot.title = element_text(hjust = 0.5))
RL2m3a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("c) s=0.45")+ theme(plot.title = element_text(hjust = 0.5))

df <- RL2m4df
RL2m4 <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("d) s=0.60")+ theme(plot.title = element_text(hjust = 0.5))
RL2m4a <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("d) s=0.60")+ theme(plot.title = element_text(hjust = 0.5))



################################################################################################
#Li-Martin Results
load("LM_Results.Rdata")

#Eliminate RF from graphic since it's so big makes others hard to see
df <- LM1mdf %>% filter(Technique !="RF")
LM1p <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) Uncorrelated Predictors")+ theme(plot.title = element_text(hjust = 0.5)) 
LM1pa <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("b) Uncorrelated Predictors")+ theme(plot.title = element_text(hjust = 0.5))

df <- LM2mdf %>% filter(Technique !="RF")
LM2p <- ggplot(df, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) Correlated Predictors")+ theme(plot.title = element_text(hjust = 0.5)) 
LM2pa <- ggplot(df, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("b) Correlated Predictors")+ theme(plot.title = element_text(hjust = 0.5))


#########################################################################
#Function to create plots with one legend
library(gridExtra)
#Get legend at bottom of all plots
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

###################################################################################
# Plots

#RL1
mylegend<-g_legend(RL1m1)

p <- grid.arrange(arrangeGrob(RL1m1 + theme(legend.position="none"),
                              RL1m2 + theme(legend.position="none"),
                              RL1m3 + theme(legend.position="none"),
                              RL1m4 + theme(legend.position="none"),
                              nrow=2),
                  mylegend, nrow=2,heights=c(10, 2))

ggsave(filename="RL1MSPE_Plots.eps", plot=p, device="eps", width= 6, height=6, units="in", dpi=300)
ggsave(filename="RL1MSPE_Plots.jpeg", plot=p, device="jpeg", width= 6, height=6, units="in", dpi=300)

#RL1
mylegend<-g_legend(RL1m1)

p <- grid.arrange(arrangeGrob(RL1m1a + theme(legend.position="none"),
                              RL1m2a + theme(legend.position="none"),
                              RL1m3a + theme(legend.position="none"),
                              RL1m4a + theme(legend.position="none"),
                              nrow=2),
                  mylegend, nrow=2,heights=c(10, 2))

ggsave(filename="RL1MAPE_Plots.eps", plot=p, device="eps", width= 6, height=6, units="in", dpi=300)
ggsave(filename="RL1MAPE_Plots.jpeg", plot=p, device="jpeg", width= 6, height=6, units="in", dpi=300)

#RL2
mylegend<-g_legend(RL1m1)

p <- grid.arrange(arrangeGrob(RL2m1 + theme(legend.position="none"),
                              RL2m2 + theme(legend.position="none"),
                              RL2m3 + theme(legend.position="none"),
                              RL2m4 + theme(legend.position="none"),
                              nrow=2),
                  mylegend, nrow=2,heights=c(10, 2))

ggsave(filename="RL2MSPE_Plots.eps", plot=p, device="eps", width= 6, height=6, units="in", dpi=300)
ggsave(filename="RL2MSPE_Plots.jpeg", plot=p, device="jpeg", width= 6, height=6, units="in", dpi=300)


#RL2
mylegend<-g_legend(RL2m1)

p <- grid.arrange(arrangeGrob(RL2m1a + theme(legend.position="none"),
                              RL2m2a + theme(legend.position="none"),
                              RL2m3a + theme(legend.position="none"),
                              RL2m4a + theme(legend.position="none"),
                              nrow=2),
                  mylegend, nrow=2,heights=c(10, 2))

ggsave(filename="RL2MAPE_Plots.eps", plot=p, device="eps", width= 6, height=6, units="in", dpi=300)
ggsave(filename="RL2MAPE_Plots.jpeg", plot=p, device="jpeg", width= 6, height=6, units="in", dpi=300)


#LM1-MSPE
mylegend<-g_legend(LM1p)

p <- grid.arrange(arrangeGrob(LM1p + theme(legend.position="none")+ylim(c(8,12.5))+ggtitle("a) Uncorrelated Predictors")+ theme(plot.title = element_text(hjust = 0.5)) ,
                              LM2p + theme(legend.position="none")+ylim(c(8,11.2))+ggtitle("b) Correlated Predictors")+ theme(plot.title = element_text(hjust = 0.5)) ,
                              nrow=1),
                  mylegend, nrow=2,heights=c(10, 2))

ggsave(filename="LMMSPE_Plots.eps", plot=p, device="eps", width= 6, height=3, units="in", dpi=300)
ggsave(filename="LMMSPE_Plots.jpeg", plot=p, device="jpeg", width= 6, height=3, units="in", dpi=300)


#LM1-MAPE
mylegend<-g_legend(LM1p)

p <- grid.arrange(arrangeGrob(LM1pa + theme(legend.position="none")+ylim(c(2.05,2.45))+ggtitle("a) Uncorrelated Predictors")+ theme(plot.title = element_text(hjust = 0.5)) ,
                              LM2pa + theme(legend.position="none")+ylim(c(1.8,2.15))+ggtitle("b) Correlated Predictors")+ theme(plot.title = element_text(hjust = 0.5)) ,
                              nrow=1),
                  mylegend, nrow=2,heights=c(10, 2))

ggsave(filename="LMMAPE_Plots.eps", plot=p, device="eps", width= 6, height=3, units="in", dpi=300)
ggsave(filename="LMMAPE_Plots.jpeg", plot=p, device="jpeg", width= 6, height=3, units="in", dpi=300)


############################################################################################
#Parameter Tuning Results

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
    df1 <- df[3,p,m,r][[1]][[2]][,1]
  } else{
    df1 <- df[3,p,m,r][[1]][[2]][,2]
  }
  return(df1)
}  


ntechs <- 3
nprops <- 6
p <- c(rep(c(0, 0.05, 0.10, 0.15, 0.20, 0.25),each=ntechs))
Techs=c("UCV", "WCV", "NC")
Technique <- c(rep(Techs, nprops))

df <- RL2
#s=1
MSPEarray <- sapply(1:6, simplify="array", function(prop){sapply(1:500, FUN=TuningResFunc, p=prop, m=1, Crit="MSPE")})
MAPEarray <- sapply(1:6, simplify="array", function(prop){sapply(1:500, FUN=TuningResFunc, p=prop, m=1, Crit="MAPE")})
MSPE <- c(apply(MSPEarray,c(1,3),mean))  #Row1=unwt CV, row2= wt. CV, row3=nonoutliers, cols index p
MAPE <- c(apply(MAPEarray,c(1,3),mean))  #Row1=unwt CV, row2= wt. CV, row3=nonoutliers, cols index p
df <- data.frame(p, Technique, MSPE, MAPE)
df$Technique <- rep(c("UCV", "WCV", "NC"),6)
RL2m1CVdf <- df
RL2m1CV <- ggplot(RL2m1CVdf, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.15")+ theme(plot.title = element_text(hjust = 0.5))
RL2m1CVa <- ggplot(RL2m1CVdf, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.15")+ theme(plot.title = element_text(hjust = 0.5))


#m=2
df <- RL2
MSPEarray <- sapply(1:6, simplify="array", function(prop){sapply(1:500, FUN=TuningResFunc, p=prop, m=2, Crit="MSPE")})
MAPEarray <- sapply(1:6, simplify="array", function(prop){sapply(1:500, FUN=TuningResFunc, p=prop, m=2, Crit="MAPE")})
MSPE <- c(apply(MSPEarray,c(1,3),mean))  #Row1=unwt CV, row2= wt. CV, row3=nonoutliers, cols index p
MAPE <- c(apply(MAPEarray,c(1,3),mean))  #Row1=unwt CV, row2= wt. CV, row3=nonoutliers, cols index p
df <- data.frame(p, Technique, MSPE, MAPE)
df$Technique <- rep(c("UCV", "WCV", "NC"),6)
RL2m2CVdf <- df
RL2m2CV <- ggplot(RL2m2CVdf, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.3")+ theme(plot.title = element_text(hjust = 0.5))
RL2m2CVa <- ggplot(RL2m2CVdf, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.3")+ theme(plot.title = element_text(hjust = 0.5))

#m=3
df <- RL2
MSPEarray <- sapply(1:6, simplify="array", function(prop){sapply(1:500, FUN=TuningResFunc, p=prop, m=3, Crit="MSPE")})
MAPEarray <- sapply(1:6, simplify="array", function(prop){sapply(1:500, FUN=TuningResFunc, p=prop, m=3, Crit="MAPE")})
MSPE <- c(apply(MSPEarray,c(1,3),mean))  #Row1=unwt CV, row2= wt. CV, row3=nonoutliers, cols index p
MAPE <- c(apply(MAPEarray,c(1,3),mean))  #Row1=unwt CV, row2= wt. CV, row3=nonoutliers, cols index p
df <- data.frame(p, Technique, MSPE, MAPE)
df$Technique <- rep(c("UCV", "WCV", "NC"),6)
RL2m3CVdf <- df
RL2m3CV <- ggplot(RL2m3CVdf, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.45")+ theme(plot.title = element_text(hjust = 0.5))
RL2m3CVa <- ggplot(RL2m3CVdf, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(legend.position = "bottom")+ggtitle("a) m=0.45")+ theme(plot.title = element_text(hjust = 0.5))

#m=4
df <- RL2
MSPEarray <- sapply(1:6, simplify="array", function(prop){sapply(1:500, FUN=TuningResFunc, p=prop, m=4, Crit="MSPE")})
MAPEarray <- sapply(1:6, simplify="array", function(prop){sapply(1:500, FUN=TuningResFunc, p=prop, m=4, Crit="MAPE")})
MSPE <- c(apply(MSPEarray,c(1,3),mean))  #Row1=unwt CV, row2= wt. CV, row3=nonoutliers, cols index p
MAPE <- c(apply(MAPEarray,c(1,3),mean))  #Row1=unwt CV, row2= wt. CV, row3=nonoutliers, cols index p
df <- data.frame(p, Technique, MSPE, MAPE)
df$Technique <- rep(c("UCV", "WCV", "NC"),6)
RL2m4CVdf <- df
RL2m4CV <- ggplot(RL2m4CVdf, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point() + theme(legend.position = "bottom")+ggtitle("a) m=0.6")+ theme(plot.title = element_text(hjust = 0.5))
RL2m4CVa <- ggplot(RL2m4CVdf, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point() + theme(legend.position = "bottom")+ggtitle("a) m=0.6")+ theme(plot.title = element_text(hjust = 0.5))


load("LM1.Rdata")
df <- LM1
MSPEarray <- sapply(1:6, simplify="array", function(prop){sapply(1:500, FUN=TuningResFunc, p=prop, m=1, Crit="MSPE")})
MAPEarray <- sapply(1:6, simplify="array", function(prop){sapply(1:500, FUN=TuningResFunc, p=prop, m=1, Crit="MAPE")})
MSPE <- c(apply(MSPEarray,c(1,3),mean))  #Row1=unwt CV, row2= wt. CV, row3=nonoutliers, cols index p
MAPE <- c(apply(MAPEarray,c(1,3),mean))  #Row1=unwt CV, row2= wt. CV, row3=nonoutliers, cols index p
df <- data.frame(p, Technique, MSPE, MAPE)
df$Technique <- rep(c("UCV", "WCV", "NC"),6)
LMCVdf <- df
LMCV <- ggplot(LMCVdf, aes(x = p, y = MSPE, color = Technique)) + geom_line()+geom_point()+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position = "bottom")
LMCVa <- ggplot(LMCVdf, aes(x = p, y = MAPE, color = Technique)) + geom_line()+geom_point()+ theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position = "bottom")

ggsave(filename="LMCV_MSPE.eps", plot=LMCV, device="eps", width= 3, height=3, units="in", dpi=300)
ggsave(filename="LMCV_MSPE.jpeg", plot=LMCV, device="jpeg", width= 3, height=3, units="in", dpi=300)

ggsave(filename="LMCV_MAPE.eps", plot=LMCVa, device="eps", width= 3, height=3, units="in", dpi=300)
ggsave(filename="LMCV_MAPE.jpeg", plot=LMCVa, device="jpeg", width= 3, height=3, units="in", dpi=300)


##########################################################################################################
#Plot ERR by alpha
load("LM1.Rdata")
alpha <- seq(from=1, to=30, by=0.25)

df <- LM1

#for m=1, p=6
AlphERR <- sapply(1:500, FUN=ERRbyAlpha, p=6, m=1, Crit="MSPE")
MSPE <- apply(AlphERR, 1, mean)[3:119]
df1 <- data.frame(MSPE, alpha)
pl <- ggplot(df1, aes(x = alpha, y = MSPE)) + geom_line()+geom_line() +ylim(c(10,15)) +xlab(expression(alpha))
pl
ggsave(filename="ERR_Curve.eps", plot=pl, device="eps", width= 3, height=3, units="in", dpi=300)
ggsave(filename="ERR_Curve.jpeg", plot=pl, device="jpeg", width= 6, height=6, units="in", dpi=300)

############################################################################################################
#Plot ERR by alpha


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

Alphas <- c(sapply(1:500, FUN=GetAlpha, df=LM1, p=6, m=1, Crit="MSPE"))
Technique <- rep(c("UCV", "WCV"),500)
df <- data.frame(Alphas, Technique)
pl <- ggplot(df, aes(x = Technique, y = Alphas)) + geom_boxplot(outlier.shape=16, outlier.size=1)+ylim(c(0,30))+ylab(expression(alpha))
pl
ggsave(filename="Alpha_BP.eps", plot=pl, device="eps", width= 3, height=3, units="in", dpi=300)
ggsave(filename="Alpha_BP.jpeg", plot=pl, device="jpeg", width= 6, height=6, units="in", dpi=300)

