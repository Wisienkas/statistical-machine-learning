install.packages("klaR")
install.packages("caret")
install.packages("e1071")

library(klaR)
library(e1071)
library("caret")

# Showing Example with Iris
x <- iris[, -5]
y <- iris$Species

model = train(x = x, 
              y = y,
              method = 'nb',
              trControl=trainControl(method='cv',
                                     number=10)
              )

myPredict <- predict(model$finalMode, x)

crossTable <- table(myPredict$class, y)
