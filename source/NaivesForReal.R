install.packages('ElemStatLearn')
install.packages("klaR")
install.packages("caret")

library('ElemStatLearn')
library("klaR")
library("caret")

sub = sample(nrow(spam), floor(nrow(spam) * 0.9))
train = spam[sub,]
test = spam[-sub,]

xTrain = train[,-58]
yTrain = train$spam

xTest = test[,-58]
yTest = test$spam

model = train(xTrain,yTrain,'nb',trControl=trainControl(method='cv',number=10))

testResultTable <- table(predict(model$finalModel,xTest)$class,yTest)
testResultTable.prob <- prop.table(testResultTable)
