library(neuralnet)
library(devtools)
library(e1071)
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')

 data(Glass, package="mlbench")

#' shuffle since flowers are in order by species!
#' # data(iris)
#Glass <- iris[sample(1:nrow(iris)),]

index <- 1:nrow(Glass)
trIdx <- sample(index, size= length(index)*0.8 )
tsIdx <- index[-trIdx]
trianSet <- Glass[trIdx, -10]
testSet <- Glass[tsIdx, -10]
TypeTr <- Glass[trIdx, 10]
TypeTs <- Glass[tsIdx, 10]

case <- class.ind(TypeTr)


nn1 <- nnet(TypeTr ~ . , data = trianSet, maxit = 200, size = 5,  rang = 0.1 )

mygrid <- expand.grid(.decay=c(0.5, 0.1), .size=c(4,5,6,8,10,12,14,16))
nnetfit <- train(TypeTr ~ . , data=trianSet, method="nnet", maxit=1000, tuneGrid=mygrid, trace=T)  
print(nnetfit)

predict(nnetfit, testSet)
CM2 <- table(true=TypeTs, predicted=predict(nnetfit, testSet))
classAgreement(CM2)

# *****************************************************************************
# ***************************      ANNs Regression  ****************************
# *****************************************************************************
###
### prepare data
###
library(mlbench)
data(BostonHousing)

# inspect the range which is 1-50
summary(BostonHousing$medv)


##
## model linear regression
##

lm.fit <- lm(medv ~ ., data=BostonHousing)

lm.predict <- predict(lm.fit)

# mean squared error: 21.89483
mean((lm.predict - BostonHousing$medv)^2) 

plot(BostonHousing$medv, lm.predict,
     main="Linear regression predictions vs actual",
     xlab="Actual")


##
## model neural network
##
require(nnet)

# scale inputs: divide by 50 to get 0-1 range
nnet.fit <- nnet(medv/50 ~ ., data=BostonHousing, size=2) 

# multiply 50 to restore original scale
nnet.predict <- predict(nnet.fit)*50 

# mean squared error: 16.40581
mean((nnet.predict - BostonHousing$medv)^2) 

plot(BostonHousing$medv, nnet.predict,
     main="Neural network predictions vs actual",
     xlab="Actual")
# *****************************************************************************
# ***************************   Tuned   ANNs Regression  **********************
# *****************************************************************************


library(mlbench)
data(BostonHousing) 
require(caret) 
set.seed(2)
mygrid <- expand.grid(.decay=c(0.5, 0.1), .size=c(4,5,6,8,10,12,14,16))
 nnetfit <- train(medv/50 ~ ., data=BostonHousing, method="nnet", maxit=1000, tuneGrid=mygrid, trace=F) #trControl = trainControl(method = "cv"), 
 print(nnetfit)

