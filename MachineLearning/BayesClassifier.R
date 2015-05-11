 

library("klaR")
library("caret")

x = iris[,-5]
y = iris$Species

model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
predict(model$finalModel,x)

predict(model$finalModel,x)$class

table(predict(model$finalModel,x)$class,y)



