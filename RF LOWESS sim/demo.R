library(devtools)
install_github("andrewjsage/RFLOWESS")

library(randomForestSRC)
library(RFLOWESS)

#generate data
set.seed(04072020)
n <- 100
x1 <- rnorm(n, 0, 1)
x2 <- rnorm(n, 0, 5)
x3 <- rbinom(n, 1, 0.5)
x4 <- rnorm(n, 10, 0.5)
e <- rnorm(n, 0, 3)
y <- x2^2 + 5*x3 + e
df <- data.frame(x1, x2, x3, x4, y)
train <- df[1:90, ]
test <- df[91:1000, ]

#grow forest and make predictions
RF <- rfsrc(y~., data=train, forest.wt="oob", membership = T, nodesize=5)
RFpred <- predict(RF, newdata=test, forest.wt=TRUE, membership = T)
OOBWeights <- RF$forest.wt   #weights for OOB predictions
PredWeights <- RFpred$forest.wt #weights for prediction of new cases
LPred <- LOWESSPred(OOBWeights, PredWeights, TRAINY=train$y, alpha=6, method="Tukey", tol=10^-6)  #RFLOWESS calculations
LPred[[1]] #Predictions
LPred[[2]] #Matrix of prediction weights
LPred[[3]] #number of iterations


#Illustration for a specific test case
lambdas <- Compute_deltas(OOBWeights, TRAINY=train$y, alpha=6)  #calculate lamda's using iterative procedure
#Predict test case j
j <- 1
w <- lambdas[[1]]*PredWeights[j,]/sum(lambdas[[1]]*PredWeights[j,]) #calculate weights of training cases for prediction of case j
LPred[[2]] #check that these match w
sum(w*train$y) #prediction
LPred[[1]]  #chec that this matches prediction on line above
