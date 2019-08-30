
#install.packages("anfis")
#install.packages("parallel")
#install.packages("anfis3")
library(anfis)
library(bigmemory.sri)
require("parallel")
if(.Platform$OS.type == "windows"){
  options(mc.cores=1)
}else{
  options(mc.cores=2) ##You could use all calling detectCores()
}
##Example domain for bidimentional sinc(x,y) function
diabetesdata <- read.csv("diabetesmodi.csv", header = FALSE)
dia_mat <- as.matrix(diabetesdata)
#dia_mat1<- read.big.matrix(dia_mat)
##Training domain patterns
X <- dia_mat[,1:2]
Y <- dia_mat[,9,drop=FALSE]
##Defining the required MembershipFunctions for the ANFIS
membershipFunction<-list(
  a1=c(new(Class="NormalizedGaussianMF",parameters=c(mu=-10,sigma=2)),
      new(Class="NormalizedGaussianMF",parameters=c(mu=-5,sigma=2)),
      new(Class="NormalizedGaussianMF",parameters=c(mu=0,sigma=2)),
      new(Class="NormalizedGaussianMF",parameters=c(mu=5,sigma=2)),
      new(Class="NormalizedGaussianMF",parameters=c(mu=10,sigma=2)),
      new(Class="NormalizedGaussianMF",parameters=c(mu=11,sigma=2)),
      new(Class="NormalizedGaussianMF",parameters=c(mu=12,sigma=2)),
      new(Class="NormalizedGaussianMF",parameters=c(mu=13,sigma=2)),
      new(Class="NormalizedGaussianMF",parameters=c(mu=14,sigma=2))),
  a2=c(new(Class="NormalizedGaussianMF",parameters=c(mu=-10,sigma=2)),
       new(Class="NormalizedGaussianMF",parameters=c(mu=-5,sigma=2)),
       new(Class="NormalizedGaussianMF",parameters=c(mu=0,sigma=2)),
       new(Class="NormalizedGaussianMF",parameters=c(mu=5,sigma=2)),
       new(Class="NormalizedGaussianMF",parameters=c(mu=10,sigma=2)),
       new(Class="NormalizedGaussianMF",parameters=c(mu=11,sigma=2)),
       new(Class="NormalizedGaussianMF",parameters=c(mu=12,sigma=2)),
       new(Class="NormalizedGaussianMF",parameters=c(mu=13,sigma=2)),
       new(Class="NormalizedGaussianMF",parameters=c(mu=14,sigma=2))))
##Creating the ANFIS network with 2 inputs and 4 MembershipFunctions in
##each input
anfis1 <- new(Class="ANFIS",X,Y,membershipFunction)
anfis1
##Check for epsilon-completeness in each input
plotMFs(anfis1)
##Training the ANFIS network.
trainOutput <- trainHybridJangOffLine(anfis1, epochs=100)
##We will use instead an already trained object to reduce example time.
#data(anfis3)
##How the training went. You can keep on training as the training error
##is still descending.
#plot(anfis3)
##Test the fit, i. e., how the MembershipFunctions partition the input space
#plotMFs(anfis3)
#print(anfis3)
#show(anfis3)


getConsequents(anfis1)[1:2,]
getErrors(anfis1) #Training errors
getTrainingType(anfis1)
names(coef(anfis1))
##An alternative to get premises and/or consequents ...
coef(anfis1)$premises[[input=1]][[mf=1]]
coef(anfis1)$consequents[1:2,]
##First five train pattern associated values for the training process
fitted(anfis1)[1:5,]
resid(anfis1)[1:5,]
summary(anfis1)
##Surface comparison between the original training set and the predicted
##ANFIS network
y <- predict(anfis1,X)
z <- matrix(y[,1],ncol=length(X),nrow=length(X))
par(mfrow=c(1,2))
persp(X,X,Z,theta = 45, phi = 15, expand = 0.8, col = "lightblue",
      ticktype="detailed",main="Goal")
  persp(X,X,z,theta = 45, phi = 15, expand = 0.8, col = "lightblue",
      ticktype="detailed",main="Fitted training Patterns", zlim=c(min(Z),max(Z)))
