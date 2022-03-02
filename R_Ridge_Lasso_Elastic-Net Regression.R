install.packages("glmnet")
library(glmnet)

n <- 1000
p <- 5000
true_dependent <- 15

X <- matrix(rnorm(n*p), nrow = n, ncol = p)
Y <- apply(X[,1:true_dependent], 1, sum) + rnorm(n)

train_rows <- sample(1:n, 0.66*n)
X_train <- X[train_rows,]
Y_train <- Y[train_rows]
X_test <- X[-train_rows,]
Y_test <- Y[-train_rows]

set.seed(42)

# alpha = 0 means we use "Ridge Regression."
alpha0.fit <- cv.glmnet(X_train, Y_train, type.measure = "mse", alpha=0, 
                        family= "gaussian")

alpha0.predicted <- predict(alpha0.fit, s=alpha0.fit$lambda.1se, newx = X_test)
mean((Y_test - alpha0.predicted)^2)

# alpha = 1 means we use "Lasso Regression."
alpha1.fit <- cv.glmnet(X_train, Y_train, type.measure = "mse",
                        alpha = 1, family = "gaussian")
alpha1.predicted <- predict(alpha1.fit, s=alpha1.fit$lambda.1se, newx = X_test)
mean((Y_test - alpha1.predicted)^2)

# Lasso Regression is much better with this data than Ridge Regression. 

# alpha = 0.5 for Elastic-Net Regression
alpha0.5.fit <- cv.glmnet(X_train, Y_train, type.measure = "mse",
                        alpha = 0.5, family = "gaussian")
alpha0.5.predicted <- predict(alpha0.5.fit, s=alpha0.5.fit$lambda.1se, newx = X_test)
mean((Y_test - alpha0.5.predicted)^2)

# Still, Lasso Regression is better.
# Grid-search is necessary for more accurate comparison.
list_of_fits <- list()
for (i in 0:10) {
  fit.name <- paste0("alpha", i/10)
  list_of_fits[[fit.name]] <-
    cv.glmnet(X_train, Y_train, type.measure = "mse", alpha=i/10,
              family= "gaussian")
}

results_mse <- data.frame()
for (i in 0:10) {
  fit.name <- paste0("alpha", i/10)
  predicted <- predict(list_of_fits[[fit.name]],
                       s=list_of_fits[[fit.name]]$lambda.1se, newx = X_test)
  mse <- mean((Y_test - predicted)^2)
  temp <- data.frame(alpha=i/10, mse=mse, fit.name = fit.name)
  results_mse <- rbind(results_mse, temp)
}

results_mse
# alpha       mse fit.name
# 1    0.0 15.506555   alpha0
# 2    0.1  2.620479 alpha0.1
# 3    0.2  1.726647 alpha0.2
# 4    0.3  1.466832 alpha0.3
# 5    0.4  1.296961 alpha0.4
# 6    0.5  1.298754 alpha0.5
# 7    0.6  1.232836 alpha0.6
# 8    0.7  1.203874 alpha0.7
# 9    0.8  1.221488 alpha0.8
# 10   0.9  1.183168 alpha0.9
# 11   1.0  1.189669   alpha1

# alpha = 0.9 is the best.
