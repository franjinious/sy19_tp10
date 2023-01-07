#######
# SVM #
#######

#Preparation des indexes-----------------
group.outer <- rep((1:10), (n/10)+1)[1:n]#生成4000个1~10的列表
idx.test.outer <- list()
idx.test.inner <- list()
rs.data.inner <- list()
for(i in 1:10){
  index.cv <- which(group.outer==i)
  idx.test.outer[[i]] <- index.cv #取出所有1~10分别对应的位置
  n.inner <- n - length(index.cv)
  rs.data.inner[[i]] <- sample(n.inner)
  group.inner <- rep((1:10), (n.inner/10)+1)[1:n.inner]
  idx.test.inner[[i]] <- list()
  for(j in 1:10){
    index.inner.cv <- which(group.inner==j)
    idx.test.inner[[i]][[j]] <- index.inner.cv
  }
}

#FINI#

#Package
if(!require("kernlab")) {
  install.packages("kernlab")
  library("kernlab")
}
#CV
CC<-c(0.001,0.01,0.1,1,100)#,1000)
epsilon <- seq(0, 1, 0.1)
N<-length(CC)
M<-5 # nombre de r??p??titions de la validation crois??e


#SVM lineaire-----------------------------------
#Cross validation pour la valeur de C
errLin<-matrix(0,N,M)

set.seed(69)
for(k in 1:M){
  for(i in 1:N){
    errLin[i,k]<-cross(ksvm(y ~.,data = data.train, type="eps-svr",kernel="vanilladot",C=CC[i],cross = 5))
  }
}
Err<-rowMeans(errLin)
plot(CC,Err,type="b",log="x",xlab="C pour linear kernel",ylab="CV error")
which.min(Err)

svmLinFit <- ksvm(y ~.,data = data.train, type="eps-svr",
                  kernel="vanilladot",C=CC[which.min(Err)])
svmLinPre <- predict(svmLinFit, newdata = x.test)
mean((svmLinPre-y.test)^2)#0.04289

#CV
err.svmlin.mse <-  rep(0, 10)
for(i in 1:10){
  index.outer.cv <- idx.test.outer[[i]] #每次取出一组数
  data.inner <- data[-index.outer.cv,] #用于训练模型
  data.validation <- data[index.outer.cv,] #用于测试
  
  
  y.test.svmLin <- data.validation[, ncol(data)]
  svmfit<-ksvm(y~.,data=data.inner,scaled=TRUE,type="eps-svr", kernel="vanilladot",C=0.01)
  yhat<-predict(svmfit,newdata=data.validation) 
  err.svmlin.mse[i] <- mean((y.test.svmLin - yhat)^2)
}
boxplot(err.svmlin.mse)
mean(err.svmlin.mse) # 0.0427


#SVM laplacien--------------------------------
#Cross validation pour la valeur de C
CC<-c(0.001,0.01,0.1,1,10,100, 1000, 10000)
N<-length(CC)
M<-5 # nombre de r??p??titions de la validation crois??e
errLap<-matrix(0,N,M)

set.seed(69)
for(k in 1:M){
  for(i in 1:N){
    errLap[i,k]<-cross(ksvm(y ~.,data = data.train, type="eps-svr",kernel="laplacedot",C=CC[i],cross = M))
  }
}
ErrLap<-rowMeans(errLap)
plot(CC,ErrLap,type="b",log="x",xlab="C pour laplace kernel",ylab="CV error")
which.min(ErrLap)
svmLapFit <- ksvm(y ~.,data = data.train, type="eps-svr",kernel="laplacedot",C=100)
svmLapPre <- predict(svmLapFit, newdata = x.test)
mean((svmLapPre-y.test)^2)#0.008838603


#CV
set.seed(69)
err.svmlap.mse <-  rep(0, 10)
for(i in 1:10){
  index.outer.cv <- idx.test.outer[[i]]
  data.inner <- data[-index.outer.cv,]
  data.validation <- data[index.outer.cv,]
  
  data.inner <- data.inner[rs.data.inner[[i]], ]
  y.test.svmLap <- data.validation[, ncol(data)]
  svmfit<-ksvm(y~.,data=data.inner,type="eps-svr", kernel="laplacedot",C=100)
  yhat<-predict(svmfit,newdata=data.validation) 
  err.svmlap.mse[i] <- mean((y.test.svmLap - yhat)^2)
}
boxplot(err.svmlap.mse)
mean(err.svmlap.mse) # 0.007916768


#SVM Gaussian--------------------------------
#Cross validation pour la valeur de C
#CC<-c(0.001,0.01,0.1,1,10,100)
#N<-length(CC)
#M<-5 # nombre de r??p??titions de la validation crois??e
errGau<-matrix(0,N,M)

set.seed(69)
for(k in 1:M){
  for(i in 1:N){
    errGau[i,k]<-cross(ksvm(y ~.,data = data.train, type="eps-svr",kernel="rbfdot",C=CC[i],cross = M))
  }
}
ErrGau<-rowMeans(errGau)
plot(CC,ErrGau,type="b",log="x",xlab="C",ylab="CV error")
which.min(ErrGau)
svmGauFit <- ksvm(y ~.,data = data.train, type="eps-svr",kernel="rbfdot",C=100)#CC[which.min(ErrGau)])
svmGauPre <- predict(svmGauFit, newdata = x.test)
mean((svmGauPre-y.test)^2)#0.007565551 0.00703(C=100)


#CV
set.seed(69)
err.svmgau.mse <-  rep(0, 10)
for(i in 1:10){
    index.outer.cv <- idx.test.outer[[i]]
    data.inner <- data[-index.outer.cv,]
    data.validation <- data[index.outer.cv,]
    
    data.inner <- data.inner[rs.data.inner[[i]], ]
    y.test.svmgau <- data.validation[, ncol(data)]
    svmfit<-ksvm(y~.,data=data.inner,type="eps-svr", kernel="rbfdot",C=100, epsilon=0.2)
    yhat<-predict(svmfit,newdata=data.validation) 
    err.svmgau.mse[i] <- mean((y.test.svmgau - yhat)^2)
    
  }
boxplot(err.svmgau.mse)
mean(err.svmgau.mse) # 0.006989074


#test avec epsilon--------------------------
set.seed(69)
err.svmgau.mse1 <-  rep(0, 10)
for(i in 1:10){
    index.outer.cv <- idx.test.outer[[i]]
    data.inner <- data[-index.outer.cv,]
    data.validation <- data[index.outer.cv,]
    
    data.inner <- data.inner[rs.data.inner[[i]], ]
    y.test.svmgau <- data.validation[, ncol(data)]
    svmfit<-ksvm(y~.,data=data.inner,type="eps-svr", kernel="rbfdot",C=100, epsilon = 0.2)
    yhat<-predict(svmfit,newdata=data.validation) 
    err.svmgau.mse[i] <- mean((y.test.svmgau - yhat)^2)
    
}
boxplot(err.svmgau.mse1)
mean(err.svmgau.mse1) # 0.006989074


#CARET------------------
if(!require("caret")) {
  install.packages("caret")
  library("caret")
}
models <- getModelInfo("svmRadial", regex = FALSE)[[1]]
preProcValues = preProcess(data.train, method = c("center", "scale")) 
processData = predict(preProcValues,data.train)
x = processData[,-9]
y = processData[, 9]
set.seed(69)
models$grid(x,y,3)


set.seed(69)
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
svm1 <- train(y ~ .,
              data = data.train,
              method = "svmRadial",
              preProcess = c("center", "scale"),
              trControl = trainControl(method = "cv", number = 5))
pre1 <- predict(svm1, newdata = data.test)
err1 <- mean((pre1 - data.test[, 9])^2)
#View the model
plot(svm1, type = 'b', col = 'blue', xlab = "K(Scale)")

set.seed(69)
err.svmCaret.mse <-  rep(0, 10)
for(i in 1:10){
  index.outer.cv <- idx.test.outer[[i]]
  data.inner <- data[-index.outer.cv,]
  data.validation <- data[index.outer.cv,]
  
  data.inner <- data.inner[rs.data.inner[[i]], ]
  y.test.svmgau <- data.validation[, ncol(data)]
  svm1 <- train(y ~., data = data.train, method = "svmRadial", trControl = train_control)

  #svmfit<-ksvm(y~.,data=data.inner,type="eps-svr", kernel="rbfdot",C=100)
  yhat<-predict(svm1,newdata=data.validation) 
  err.svmCaret.mse[i] <- mean((y.test.svmgau - yhat)^2)
}
boxplot(err.svmCaret.mse)
mean(err.svmCaret.mse) # 0.006989074


#hzf-------------------------
library('kernlab')

ksvm.fit <-  ksvm(y~., data=data.train,type="eps-svr", C=30, epsilon=0.1)
#ksvm(x=x.train[,1:9], y=x.train$y, type=)
ksvm.pred <- predict(ksvm.fit, data.test)
sqrt(mean((data.test$y - ksvm.pred)^2))


cost <- c(2^(1:7))
epsilon = seq(0,1,0.1)

err <- rep(0, 11)
#  matrix(nrow=length(cost), ncol=length(epsilon))

  for (j in 1:length(epsilon)) {
    err[j] <- cross(ksvm(y~., data=data.train,type="eps-svr", cross = 5, C=100,epsilon=epsilon[j]))
  }



which.min(err)
epsilon[2] 

