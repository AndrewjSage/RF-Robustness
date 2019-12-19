#used R version 3.5.0 (2018-04-23)

install_version("randomForestSRC", version = "2.5.1", repos = "http://cran.us.r-project.org")


library(ggplot2)
library(randomForestSRC)
library(RFLOWESS)
library(dplyr)

set.seed(03122018)
#x <- seq(from=-3, to=3, by=0.05)
x <- sort(runif(100, -3, 3))
e0 <- rnorm(length(x), 0, 0.1)
e1 <- rnorm(length(x), 0, 0.5)
c <- rbinom(length(x), 1, 0.1)
e <- e0
e[c==1] <- e1[c==1]
y <- sin(x) + e
X <- data.frame(x,y)

qplot(x, y)


x <- seq(from=-3, to=3, by=0.05)
y <- rep(0, length(x))
NEW <-data.frame(x, y)

RF <- rfsrc(y~x, data=X, forest.wt="oob", membership = T, nodesize=5)
RFpred <- predict(RF, newdata=NEW, forest.wt=TRUE, membership = T)
TrainNodes <- RF$membership
TestNodes <- RFpred$membership
Inbag <- RF$inbag
True <- sin(x)
OOBWeights <- RF$forest.wt
PredWeights <- RFpred$forest.wt
LPred <- LOWESSPred(OOBWeights, PredWeights, TRAINY=X$y, alpha=6, method="Tukey", tol=10^-6)
LMPred <- LiPred(OOBWeights, PredWeights, TRAINY=X$y, method="Huber", delta=0.10,   tol=10^-6, maxiter=100)
sum((LMPred[[1]]-True)^2)
LMPred[[3]]

Resid <- X$y-predict(RF, OOB=TRUE)$predicted
RFOOBPred <- predict(RF, OOB=TRUE)$predicted

Xdf <- data.frame(X[,1])
NEWdf <- data.frame(NEW[,1])
names(Xdf)[1] <- "x"
names(NEWdf)[1] <- "x"

#QRF <- quantregForest(x=Xdf, y=X[,2], keep.forest=TRUE, keep.inbag=TRUE)
#QRFPred <- predict(QRF, newdata=NEWdf, what=0.5)
#MedAgg <- Node_Tree_Agg(TrainNodes, TestNodes, Inbag, TRAINY=X$y)


NEW$Pred <- RFpred$predicted
NEW$Actual <- True
NEW$LPred <- LPred[[1]]
NEW$LMPred <- LMPred[[1]]
#NEW$QFR <- QRFPred
#NEW$M1 <- MedAgg[[1]]
#NEW$M2 <- MedAgg[[2]]
#NEW$M3 <- MedAgg[[3]]

library(reshape2)
NEW_long <- melt(NEW, id=c("x", "y"))  # convert to long format

p <- ggplot(data=X, aes(x=x, y=y))

#Only True and RF Pred
p + geom_point() +geom_line(data = filter(NEW_long, variable%in%c("Actual")), aes(x=x, y=value, colour=factor(variable, labels=c("Expected Response"))))+
  scale_colour_manual(breaks = c("True Response"),values=c("blue"))+ 
  labs(color = "Curve")+ theme(legend.text=element_text(size=12))+ theme(legend.title=element_text(size=12))+theme(legend.position = "bottom")+
  geom_text(aes(x=1.95, y=0.1, label="Case 79")) +
  geom_text(aes(x=2.1, y=0.4, label="Case 81")) +
  geom_text(aes(x=2.55, y=2.0, label="Case 85"))   
  
# Add xcoord of new case
p + geom_point() +geom_line(data = filter(NEW_long, variable%in%c("Actual")), aes(x=x, y=value, colour=factor(variable, labels=c("Expected Response"))))+
  scale_colour_manual(breaks = c("True Response"),values=c("blue")) +
  labs(color = "Curve")+ theme(legend.text=element_text(size=12))+ theme(legend.title=element_text(size=12))+theme(legend.position = "bottom")+
  #geom_point(aes(x=1.4, y=-1.2), shape=17, size=I(3), color="red") + coord_cartesian(ylim = c(-1.4, 2.25)) +
  geom_text(aes(x=1.4, y=-1.4, label="x=1.4"), color="black")+
  geom_vline(xintercept=1.4, color="red", linetype="dotted")+
  geom_text(aes(x=1.95, y=0.1, label="Case 79")) +
  geom_text(aes(x=2.1, y=0.4, label="Case 81")) +
  geom_text(aes(x=2.55, y=2.0, label="Case 85"))   

    
# Add predicted surface
p + geom_point() +geom_line(data = filter(NEW_long, variable%in%c( "Pred", "Actual")), aes(x=x, y=value, colour=factor(variable, labels=c("RF Estimate","True Response"))))+
  scale_colour_manual(breaks = c("True Response", "RF Estimate"),values=c("red", "blue"))+ 
  labs(color = "Curve")+ theme(legend.text=element_text(size=12))+ theme(legend.title=element_text(size=12))+theme(legend.position = "bottom") 
+
  geom_text(aes(x=1.95, y=0.1, label="Case 79")) +
  geom_text(aes(x=2.1, y=0.4, label="Case 81")) +
  geom_text(aes(x=2.55, y=2.0, label="Case 85"))   

#True, RF Pred and RF-LOWESS Predicted
p + geom_point() +geom_line(data = filter(NEW_long, variable%in%c( "Pred", "Actual", "LPred")), aes(x=x, y=value, colour=factor(variable, labels=c("RF","True", "RF-LOWESS"))))+
  scale_colour_manual(breaks = c("True", "RF", "RF-LOWESS"),values=c("red", "blue", "green"))+ 
  labs(color = "Curve")+ theme(legend.text=element_text(size=12))+ theme(legend.title=element_text(size=12))+theme(legend.position = "bottom")+
  geom_text(aes(x=1.6, y=0.1, label="A")) +
  geom_text(aes(x=1.8, y=0.4, label="B")) +
  geom_text(aes(x=2.1, y=2.0, label="C"))   


ggsave(filename="Sin.eps", device="eps", width= 6, height=4, units="in", dpi=300)


#Zoom in- True, RF Pred and RF-LOWESS Predicted
p + geom_point() +geom_line(data = filter(NEW_long, variable%in%c( "Pred", "Actual", "LPred")), aes(x=x, y=value, colour=factor(variable, labels=c("RF","True", "RF-LOWESS"))))+
  scale_colour_manual(breaks = c("True", "RF", "RF-LOWESS"),values=c("red", "blue", "green"))+ 
  labs(color = "Curve")+ theme(legend.text=element_text(size=12))+ theme(legend.title=element_text(size=12))+theme(legend.position = "bottom")+
  xlim(c(1,2))+ylim(c(0,2.2))
ggsave(filename="Sin.eps", device="eps", width= 6, height=4, units="in", dpi=300)



#True, RF Pred and RF-LOWESS, and Huber Predicted
p + geom_point() +geom_line(data = filter(NEW_long, variable%in%c( "Pred", "Actual", "LPred", "LMPred")), aes(x=x, y=value, colour=factor(variable, labels=c("RF Estimate","True Response", "RF-LOWESS Est.", "HF Estimate"))))+
  scale_colour_manual(breaks = c("True Response", "RF Estimate", "RF-LOWESS Est.", "HF Estimate"),values=c("red", "blue", "green", "purple"))+ 
  labs(color = "Curve")+ theme(legend.text=element_text(size=12))+ theme(legend.title=element_text(size=12))+
  guides(color=guide_legend(nrow=2,byrow=TRUE))+
  xlim(c(0,2))+ylim(c(0,1.5)) +theme_bw()+ geom_text(aes(x=1.5, y=0.2, label="A")) +
  geom_text(aes(x=1.7, y=0.5, label="B")) +theme(legend.position = "bottom")
ggsave(filename="Sin.eps", device="eps", width= 6, height=4, units="in", dpi=300)



#Zoomed in plot of True, RF Pred and RF-LOWESS, and Huber Predicted
p + geom_point() +geom_line(data = filter(NEW_long, variable%in%c( "Pred", "Actual", "LPred", "LMPred")), aes(x=x, y=value, colour=factor(variable, labels=c("RF Estimate","True Response", "RF-LOWESS Est.", "HF Estimate"))))+
  scale_colour_manual(breaks = c("True Response", "RF Estimate", "RF-LOWESS Est.", "HF Estimate"),values=c("red", "blue", "green", "purple"))+ 
  labs(color = "Curve")+ theme(legend.text=element_text(size=12))+ theme(legend.title=element_text(size=12))+theme(legend.position = "bottom")+
  guides(color=guide_legend(nrow=2,byrow=TRUE))+xlim(c(1,2))+ylim(c(0,2.2))
+
  geom_text(aes(x=1.95, y=0.1, label="Case 79")) +
  geom_text(aes(x=2.1, y=0.4, label="Case 81")) +
  geom_text(aes(x=2.55, y=2.0, label="Case 85"))   



#Residual plot
p2 <- ggplot(data=X, aes(x=x, y=Resid))
p2 + geom_point(size=I(1))+ylab("OOB Residual")+
  geom_text(aes(x=1.9, y=-0.4, label="Case 79")) +
  geom_text(aes(x=2.1, y=-0.3, label="Case 81")) +
  geom_text(aes(x=2.4, y=0.85, label="Case 85"))   





#p + geom_line(data = NEW_long, aes(x=x, y=value, colour=variable))

lambdas <- Compute_deltas(OOBWeights, TRAINY=X$y, alpha=6)
#For test case #89
lambdas[[1]]*PredWeights[89,]/sum(lambdas[[1]]*PredWeights[89,])

which.max(abs(NEW$True-NEW$Pred))

which(abs(NEW$True-NEW$Pred)>.25)

case <-89
NEW[case,]
Weights <- data.frame(rownames(X), round(X$x,3), round(X$y,3), round(RFOOBPred,3), round(PredWeights[case,],3), round(Resid,3), round(lambdas[[1]][1,],3), round(LPred[[2]][case,],3), round(LMPred[[2]][case,],3))
names(Weights) <- c("Case", "x", "y", "RFOOB Pred", "RFweight", "Resid", "lambda",  "LOWESS", "LM")
arrange(Weights, desc(RFweight))[1:5,]
round(PredWeights[case,],3)
round(LPred[[2]][case,],3)
round(LMPred[[2]][case,],3)

Weights[c(79,81,80,82,77),]
NEW[89,]
