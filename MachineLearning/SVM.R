library(e1071)
data(Glass, package="mlbench")

index <- 1:nrow(Glass)
trIdx <- sample(index, size= length(index)*0.8 )
tsIdx <- index[-trIdx]
trianSet <- Glass[trIdx, -10]
testSet <- Glass[tsIdx, -10]
TypeTr <- Glass[trIdx, 10]
TypeTs <- Glass[tsIdx, 10]

svm.model <- svm(TypeTr ~ ., data = trianSet, cost = 100, gamma = 1)
svm.pred  <- predict(svm.model, testSet)

# compute svm confusion matrix
CM <- table(pred = svm.pred, true = TypeTs)
classAgreement(CM)

#  make tuning
tuneResult <- tune(svm, trianSet, TypeTr, ranges = list(gamma = seq(0,1,0.1), cost = 2^(2:9)))


print(tuneResult) 
plot(tuneResult)

tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, testSet) 

CM_Tunned <- table(pred = tunedModelY, true = TypeTs)
classAgreement(CM_Tunned)