##########
# Subset #
##########

#Packages
if(!require("leaps")) {
  install.packages("leaps")
  library("leaps")
}


#Best Subset---------------------
fitBestSub <- regsubsets(y~., data = data.train, method = 'exhaustive', nvmax = 8)
plot(fitBestSub,scale="r2") 
sumBestSub <- summary(fitBestSub)

rssBestSub<-data.frame(sumBestSub$outmat, RSS=sumBestSub$rss)
rsquareMaxBestSub <- sumBestSub$outmat[which.max(sumBestSub$adjr2),]#La ligne avec la plus grande adjr2
rsquareMaxBestSub[rsquareMaxBestSub == '*'] <- as.numeric(1)
rsquareMaxBestSub[rsquareMaxBestSub == ' '] <- as.numeric(0)
rsquareMaxBestSub <- as.numeric(rsquareMaxBestSub)#Le masque pour s??lectionner les variables
regBestSub <- data.train[c(rsquareMaxBestSub==1)]


#Luca --------------------------------
if(!require("stringi")) {
  install.packages("stringi")
  library("stringi")
}

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

# ______________________________________________________ 
# ________________ subset_selection ____________________
# ______________________________________________________
library(leaps)

predict.regsubsets =function (object ,newdata ,id ,...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object ,id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

nvar.max <- ncol(data)-1
err.sbf.mse<- rep(0, 10)
err.sbf.vopt <- rep(0,10)

err.sbb.mse<- rep(0, 10)
err.sbb.vopt <- rep(0,10)

# ________________ backward ____________________
for (i in 1:10) {
  index.outer.cv <- idx.test.outer[[i]]
  data.inner <- data[-index.outer.cv,] 
  data.validation <- data[index.outer.cv,] 
  data.inner <- data.inner[rs.data.inner[[i]], ]
  subset.backward.errors <- matrix(NA, 10,  nvar.max)
  for(j in 1:10){ 
    index.inner.cv <- idx.test.inner[[i]][[j]] 
    data.train <- data[-index.inner.cv,]
    data.test <- data[index.inner.cv, ]
    # train regsubset model for backward method
    model.backward <- regsubsets(y~., data=data.train, nvmax=nvar.max, 
                                 method="backward")
    # predict model
    for(v in 1:nvar.max){
      y.pred.backward <- predict(model.backward,data.test, id=v)
      subset.backward.errors[j,v] <- mean((y.pred.backward-data.test$cnt)^2)
    }
    # choose optimal v
    backward.mean <- rep(0,nvar.max)
    backward.sd <- rep(0,nvar.max)
    for(v in 1:nvar.max){
      backward.mean[v] <- mean(subset.backward.errors[,v])
      backward.sd[v] <- sd(subset.backward.errors[,v])/sqrt(length(subset.backward.errors[,v]))
    }
    backward.vopt <- min(which(min(backward.mean) == backward.mean))
    err.sbb.vopt[i] <- backward.vopt
  }
  # validation of our model with best model 
  data.train <- data.inner
  data.test <- data.validation
  # modeling with regsubset
  best.model.backward <- regsubsets(y~., data=data.train, nvmax=nvar.max, method="backward")
  # prediction
  y.pred.backward <- predict(best.model.backward, data.test, id=backward.vopt)
  err.sbb.mse[i] <- mean((y.pred.backward-data.test[, 9])^2)
}
mean(err.sbb.mse) #473445.8


# ________________ forward ____________________
for (i in 1:10) {
  index.outer.cv <- idx.test.outer[[i]]
  data.inner <- data[-index.outer.cv,] 
  data.validation <- data[index.outer.cv,] 
  data.inner <- data.inner[rs.data.inner[[i]], ]
  subset.forward.errors <- matrix(NA, 10,  nvar.max)
  for(j in 1:10){ 
    index.inner.cv <- idx.test.inner[[i]][[j]] 
    data.train <- data[-index.inner.cv,]
    data.test <- data[index.inner.cv, ]
    # train regsubset model for forward method
    model.forward <- regsubsets(cnt~., data=data.train, nvmax=nvar.max, 
                                method="forward")
    # predict model
    for(v in 1:nvar.max){
      y.pred.forward <- predict(model.forward,data.test, id=v)
      subset.forward.errors[j,v] <- mean((y.pred.forward-data.test$cnt)^2)
    }
    # choose optimal v
    forward.mean <- rep(0,nvar.max)
    forward.sd <- rep(0,nvar.max)
    for(v in 1:nvar.max){
      forward.mean[v] <- mean(subset.forward.errors[,v])
      forward.sd[v] <- sd(subset.forward.errors[,v])/sqrt(length(subset.forward.errors[,v]))
    }
    forward.vopt <- min(which(min(forward.mean) == forward.mean))
    err.sbb.vopt[i] <- forward.vopt
  }
  # validation of our model with best model 
  data.train <- data.inner
  data.test <- data.validation
  # modeling with regsubset
  best.model.forward <- regsubsets(cnt~., data=data.train, nvmax=nvar.max, method="forward")
  # prediction
  y.pred.forward <- predict(best.model.forward, data.test, id=forward.vopt)
  err.sbf.mse[i] <- mean((y.pred.forward-data.test$cnt)^2)
}
mean(err.sbf.mse) #397463



