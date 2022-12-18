##########################
# Ridge + Lasso +elastic #
##########################

#FINI#

#Preparation des librairies et des donnees---------------------

#Librarie
if(!require("glmnet")) {
  install.packages("glmnet")
  library("glmnet")
}

par(mfrow = c(1, 1))
#Donnees
x<-model.matrix(y~.,data)
y<-data$y
x.train.regu <- x[id.train,]
y.train.regu <- y[id.train]
x.test.regu <- x[-id.train,]
y.test.regu <- y[-id.train]

#Ridge regression----------------------------
#cv.out.ridge <- cv.glmnet(x.train.regu, y.train.regu, alpha = 0, type.measure = "mse")
#plot(cv.out.ridge)
#fit.ridge <- glmnet(x.train.regu, y.train.regu, lambda = cv.out.ridge$lambda.min, alpha = 0)
#ridge.predict <- predict(fit.ridge, s = cv.out.ridge$lambda.min, newx = x.test.regu)
#mse.ridge <- mean((ridge.predict - y.test.regu) ^ 2)#0.04147

#CV
group.outer <- rep((1:10), (n/10)+1)[1:n]
idx.test.outer <- list()
idx.test.inner <- list()
rs.data.inner <- list()
for(i in 1:10){
  index.cv <- which(group.outer==i)
  idx.test.outer[[i]] <- index.cv
  n.inner <- n - length(index.cv)
  rs.data.inner[[i]] <- sample(n.inner)
  group.inner <- rep((1:10), (n.inner/10)+1)[1:n.inner]
  idx.test.inner[[i]] <- list()
  for(j in 1:10){
    index.inner.cv <- which(group.inner==j)
    idx.test.inner[[i]][[j]] <- index.inner.cv
  }
}
err.rid.mse <-  rep(0, 10)
for(i in 1:10){
  index.outer.cv <- idx.test.outer[[i]]
  data.inner <- data[-index.outer.cv,]
  data.validation <- data[index.outer.cv,]
  # re-sampling (inner cross validation)
  alpha <- 0
  
  # validation model 
  X.train <- data.matrix(data.inner[, -ncol(data)])
  y.train <- data.inner[, ncol(data)]
  X.test <- data.matrix(data.validation[, -ncol(data)])
  y.test <- data.validation[, ncol(data)]
  cv.model <- cv.glmnet(X.train, y.train, alpha = alpha)
  model.fit <- glmnet(X.train, y.train, lambda = cv.model$lambda.min, alpha = alpha)
  err.rid.mse[i] <- mean((y.test - predict(model.fit, newx=X.test))^2)
}
boxplot(err.rid.mse)
mean(err.rid.mse) #0.419


#Lasso regression-----------------------------
cv.out.lasso <- cv.glmnet(x.train.regu, y.train.regu, alpha = 1, type.measure = "mse", family = "gaussian")
plot(cv.out.lasso)
fit.lasso <- glmnet(x.train.regu, y.train.regu, lambda = cv.out.lasso$lambda.min, alpha = 0)
lasso.predict <- predict(fit.lasso, s = cv.out.lasso$lambda.min, newx = x.test.regu)
mse.lasso <- mean((lasso.predict - y.test.regu) ^ 2)#0.0413992


#CV
err.las.mse <-  rep(0, 10)
for(i in 1:10){
  index.outer.cv <- idx.test.outer[[i]]
  data.inner <- data[-index.outer.cv,]
  data.validation <- data[index.outer.cv,]
  # re-sampling (inner cross validation)
  alpha <- 1
  
  # validation model 
  X.train <- data.inner[, -ncol(data)]
  y.train <- data.inner[, ncol(data)]
  X.test <- data.validation[, -ncol(data)]
  y.test <- data.validation[, ncol(data)]
  cv.model <- cv.glmnet(data.matrix(X.train), y.train, alpha = alpha)
  model.fit <- glmnet(data.matrix(X.train), y.train, lambda = cv.model$lambda.min, alpha = alpha)
  err.las.mse[i] <- mean((y.test - predict(model.fit, newx=data.matrix(X.test)))^2)
}

boxplot(err.las.mse)
mean(err.las.mse) #0.04185563


#elastic
cv.out.elastic <- cv.glmnet(x.train.regu, y.train.regu, alpha = .5, type.measure = "mse", family = "gaussian")
plot(cv.out.elastic)
fit.elastic <- glmnet(x.train.regu, y.train.regu, lambda = cv.out.elastic$lambda.min, alpha = .5)
elastic.predict <- predict(fit.elastic, s = cv.out.elastic$lambda.min, newx = x.test.regu)
mse.elastic <- mean((elastic.predict - y.test.regu)^2) #0.4138744


#CV
n.alpha <- 10
err.ela.mse <-  rep(0, 10)
for(i in 1:10){
  index.outer.cv <- idx.test.outer[[i]]
  data.inner <- data[-index.outer.cv,]
  data.validation <- data[index.outer.cv,]
  # re-sampling (inner cross validation)
  data.inner <- data.inner[rs.data.inner[[i]], ]
  alphas <- seq(0, 1, length.out =  n.alpha)
  ela.inner.mse <- rep(0, n.alpha)
  best.lambda.alpha <- rep(0, n.alpha)
  for(k in 1:length(alphas)){
    for(j in 1:10){ 
      index.inner.cv <- idx.test.inner[[i]][[j]] 
      data.train <- data.inner[-index.inner.cv, ]
      data.test <- data.inner[index.inner.cv, ]
      
      alpha <- alphas[k]
      print(alpha)
      X.cv.train <- data.matrix(data.inner[, -ncol(data)])
      y.cv.train <- data.inner[, ncol(data)]
      cv.model <- cv.glmnet(X.cv.train, y.cv.train, alpha=alpha)
      ela.inner.mse[k] <- ela.inner.mse[k] + cv.model$cvm[which(cv.model$lambda == cv.model$lambda.min)]
    }
  }
  idx.best <- which(ela.inner.mse == min(ela.inner.mse))
  best.lambda <- best.lambda.alpha[idx.best]
  best.alpha <- alphas[idx.best]
  # validation model 
  X.train <- data.matrix(data.inner[, -ncol(data)])
  y.train <- data.inner[, ncol(data)]
  X.test <- data.matrix(data.validation[, -ncol(data)])
  y.test <- data.validation[, ncol(data)]
  cv.model <- cv.glmnet(X.train, y.train, alpha=best.alpha)
  model.fit <- glmnet(X.train, y.train, lambda = cv.model$lambda.min, alpha = best.alpha)
  err.ela.mse[i] <- mean((y.test - predict(model.fit, newx=X.test))^2)
}
plot(x = alphas, y = ela.inner.mse/10, type='l', ylab='Cv(alpha) Error', xlab="alpha")
boxplot(err.ela.mse)
mean(err.ela.mse)

