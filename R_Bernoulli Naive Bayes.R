install.packages("mlbench")
library(mlbench)

#Bernoulli Naive Bayes 
data("HouseVotes84")
votes <- HouseVotes84
str(votes)

library(ggplot2)
vote1 <- na.omit(votes[,1:2])
vote1$V1 <- factor(vote1$V1, levels=c('n','y'), labels = c("No", "Yes"))
ggplot(vote1, aes(x=V1, fill=Class)) +
  geom_bar(position="dodge", width=0.7) +
  labs(title="Pros and Cons for Vote 1", 
       x="Vote1", y="Number of Congressmen",
       fill="Party")

head(votes)
# Count the number of NA values
sum(is.na(votes))

naCount <- function (col, party){
  return(sum(is.na(votes[,col]) & votes$Class == party))
}
naCount(2, "democrat")
naCount(2, "republican")

yesProb <- function(col, party){
  sum.y <- sum(votes[,col]=='y' & votes$Class == party, na.rm = TRUE)
  sum.n <- sum(votes[,col]=='n' & votes$Class == party, na.rm = TRUE)
  return(sum.y/(sum.y+sum.n))
}
yesProb(2, "democrat")
yesProb(2, "republican")

set.seed(123)
for (i in 2:ncol(votes)){
  if (sum(is.na(votes[,i]))>0){
    d.na <- which(is.na(votes[,i]) & votes$Class=="democrat")
    r.na <- which(is.na(votes[,i]) & votes$Class=="republican")
    votes[d.na,i] <- ifelse(runif(naCount(i,"democrat")) < yesProb(i,"democrat"),
                            "y", "n")
    votes[r.na,i] <- ifelse(runif(naCount(i,"republican")) < yesProb(i,"republican"),
                            "y", "n")
  }
}

sum(is.na(votes))

set.seed(123)
train <- sample(nrow(votes), nrow(votes)*0.7)
votes.train <- votes[train,]
votes.test <- votes[-train,]
table(votes.train$Class)
table(votes.test$Class)

library(e1071)
votes.nb <- naiveBayes(Class ~., data = votes.train)
votes.nb

votes.nb.predict <- predict(votes.nb, newdata = votes.test[,-1])
head(votes.nb.predict)

table(votes.test$Class, votes.nb.predict, dnn=c("Actual", "Predicted"))
mean(votes.test$Class == votes.nb.predict)

# the probability for each congressman to be "domocrat" or "republican"
votes.nb.predict_2 <- predict(votes.nb, newdata = votes.test[,-1], type="raw")
head(votes.nb.predict_2)

# Cross-Validation
CV <- function (proportion, run){
  results <- NULL
  for (i in 1:run){
    train <- sample(nrow(votes), proportion*nrow(votes))
    votes.train <- votes[train,]
    votes.test <- votes[-train,]
    votes.nb <- naiveBayes(Class~., data=votes.train)
    votes.nb.predict <- predict(votes.nb, newdata = votes.test[,-1])
    results[i] <- mean(votes.test$Class==votes.nb.predict)
  }
  return(results)
}

votes.nb.cv <- CV(0.7, 50)
votes.nb.cv
summary(votes.nb.cv)










