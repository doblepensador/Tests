install.packages("AER")
library(AER)

install.packages("e1071")
library(e1071)

data("Affairs")
str(Affairs)

aff <- Affairs
aff$affairs <- ifelse(aff$affairs==0, 0, 1)
unique(aff$affairs)
aff$affairs <- factor(aff$affairs, levels= c(0,1), labels=c("No", "Yes"))
str(aff)

table(aff$affairs)
prop.table(table(aff$affairs))

set.seed(123)
train <- sample(nrow(aff), 0.7*nrow(aff))
aff.train <- aff[train,]
aff.test <- aff[-train,]
table(aff.train$affairs)
table(aff.test$affairs)

# Preliminary fitting
set.seed(123)
aff.svm <- svm(affairs ~., data=aff.test)
summary(aff.svm)  
aff.svm.predict <- predict(aff.svm, newdata = aff.test)
head(aff.svm.predict)
table(aff.test$affairs, aff.svm.predict,
      dnn = c("Actual", "Predicted"))
# dnn=c("a", "b")
mean(aff.test$affairs == aff.svm.predict)

# probability=TRUE
aff.svm2 <- svm(affairs ~., data=aff.test, probability=TRUE)
aff.svm2.predict <- predict(aff.svm2, newdata = aff.test, probability=TRUE)
str(aff.svm2.predict)
attr(aff.svm2.predict, "probabilities")[1:6,]
# attr: attribute, "attr" can call specific information of a certain variable.

# Tuning: GridSearch CV
aff.svm.tuned <- tune.svm(affairs~., data = aff.train, 
                          gamma= 10^(-3:3), cost= 2^(-5:5))
summary(aff.svm.tuned)
aff.svm.tuned$best.model$gamma
aff.svm.tuned$best.model$cost

# - best parameters:
#   gamma    cost
# 0.001   0.03125

# Re-fitting with the best parameters
aff.svm.best <- svm(affairs~., data = aff.train,
                    gamma=0.001, cost=0.03125)
aff.svm.best.predict <- predict(aff.svm.best, newdata = aff.test)
table(aff.test$affairs, aff.svm.best.predict,
      dnn= c("Actual", "Predicted"))
mean(aff.test$affairs==aff.svm.best.predict)

################ Another example ################ 

iris
plot(iris)
plot(iris$Petal.Length, iris$Petal.Width, col=iris$Species)

s <- sample(150, 100)
col <- c('Petal.Length', 'Petal.Width', 'Species')
train <- iris[s,col]
test <- iris[-s,col]

svmfit <- svm(Species ~., data= train, kernel= "linear", cost= 0.1, scale=F)
summary(svmfit)
plot(svmfit, train[,col])

tuned <- tune.svm(Species~., data=train, kernel="linear", cost=(c(0.001, 0.01, 0.1, 1, 10, 100)))
summary(tuned)

p <- predict(svmfit, test, type='class')
plot(p)
table(p,test[,3], dnn=c("Actual", "Predicted"))
mean(p==test[,3])

#First, change df. into matrix, scale, and then change it back to df. 
train_matrix <- cbind(train$Petal.Length, train$Petal.Width)
scaled_data <- scale(train_matrix)
colnames(scaled_data) <- c("Petal.Length", "Petal.Width")
scaled_train_data <- data.frame(scaled_data)
scaled_train_data$Species <- train$Species
head(scaled_train_data)

library(ggplot2)
p <- ggplot(scaled_train_data, aes(x=Petal.Length, y=Petal.Width))
p+ geom_point(aes(color=Species, shape=Species), size=2) +
  geom_point(data= scaled_train_data[svmfit$index,],
             color="dimgray", shape=21, stroke=1.0, size=5)



