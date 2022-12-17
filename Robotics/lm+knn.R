##########
# LM+KNN #
##########

#FINI#


#LM---------------------------------
lm <- lm(formula = y ~., data = data.train)
summary(lm)
lm.res.std <- rstandard(lm)
plot(x = y.train, y = lm.res.std)#Les residus standards ne sont pas ideals
abline(0, 0)
lm.pre <- predict(lm, newdata = data.test)
lm.mse <- mean((lm.pre - y.test) ^ 2)#0.041399197

#KNN-------------------------------
# Load r packages (Install if missing)
if(!require("FNN")) {
  install.packages("FNN")
  library("FNN")
}

#Le k maximum ainsi que les residus pour les donnees avec ou sans scaling
k.max <- 20
#res.noscale <- array(0, 20)
#res.scale <- array(0, 20)


#KNN sans scaling
knn.res.noscale <- sapply(1:k.max, function(local_k){
  reg.knn.noscale <- knn.reg(train = x.train, test = x.test, y = y.train, k = local_k)
  res <- mean((reg.knn.noscale$pred - y.test) ^ 2)
  return(res)
})
which.min(knn.res.noscale)
min(knn.res.noscale)


#KNN avec scaling
knn.res.scale <- sapply(1:k.max, function(local_k){
  reg.knn.scale <- knn.reg(train = x.train.scale, test = x.test.scale, y = y.train, k = local_k)
  res1 <- mean((reg.knn.scale$pred - y.test) ^ 2)
  return(res1)
})
which.min(knn.res.scale)
min(knn.res.scale)


plot(c(1:k.max), knn.res.noscale, ylim = range(knn.res.noscale), col = "blue", type = "b", xlab = "k", ylab = "MSE")
lines(1:k.max, knn.res.scale, col = "red", type = "b") 
legend( x = "topright", legend = c("KNN non-scaled","KNN scaled"), col = c("blue","red"), lwd = 1, lty = c(1,1), pch = c(NA,NA) )

#Cross validation-----------------------
set.seed(69)
K<-10
folds=sample(1:K,n,replace=TRUE)
#CV sans scaling
CV.noscale<-rep(0,10)
for(i in (1:10)){
  for(local_k in (1:K)){
    knn.noscale<-knn.reg(train = data[folds!=local_k, -9], test = data[folds==local_k,-9], 
                         y = data[folds!=local_k, 9], k = i)
    #pred<-predict(reg,newdata=data[folds==k,])
    CV.noscale[i]<-CV.noscale[i]+ sum((data[folds==local_k, 9]- knn.noscale$pred)^2)
  }
  CV.noscale[i]<-CV.noscale[i]/n
}
plot(1:K, CV.noscale, type = 'b', col = 'blue')
which.min(CV.noscale)
CV.noscale[which.min(CV.noscale)]

#CV avec scaling
set.seed(69)
CV.scale<-rep(0,10)
for(i in (1:10)){
  for(local_k in (1:K)){
    knn.scale<-knn.reg(train = data.scale[folds!=local_k, -9], test = data.scale[folds==local_k,-9], 
                       y = data.scale[folds!=local_k, 9], k = i)
    #pred<-predict(reg,newdata=data[folds==k,])
    CV.scale[i]<-CV.scale[i]+ sum((data.scale[folds==local_k, 9]- knn.scale$pred)^2)
  }
  CV.scale[i]<-CV.scale[i]/n
}
plot(1:K, CV.scale, type = 'b', col = 'red')
which.min(CV.scale)
CV.scale[which.min(CV.scale)]

data.frame("scale" = c("NON","OUI"), "k" = c(which.min(CV.noscale), which.min(CV.scale)), 
           "MSE" = c(CV.noscale[which.min(CV.noscale)], CV.scale[which.min(CV.scale)]))
cat("On voit que KNN fonctionne mieux que le modele lineaire")#0.01608



#caret------------------------
if(!require("caret")) {
  install.packages("caret")
  library("caret")
}
set.seed(69)
knnFit.noscale <- train(x.train, y.train,
                        method = 'knn',
                        tuneLength = 20,
                        trControl = trainControl(method = 'cv'))
knnFit.scale <- train(x.train.scale, as.numeric(y.train.scale),
                      method = 'knn',
                      tuneLength = 20,
                      trControl = trainControl(method = 'cv'))

trellis.par.set(caretTheme())
par(mfrow = c(1, 2)) 
plot(knnFit.noscale, type = 'b', col = 'blue', xlab = "K(Scale)")
plot(knnFit.scale, type = 'b', col = 'red', xlab = "K(Non Scale)")
legend( x = "topright", legend = c("KNN non-scaled","KNN scaled"), col = c("blue","red"), lwd = 1, lty = c(1,1), pch = c(NA,NA) )


which.min(knnFit.scale$results[, 2])
which.min(knnFit.noscale$results[, 2])


reg.knn.noscale <- knn.reg(train = x.train, test = x.test, y = y.train, k = 7)
res.noscale <- mean((reg.knn.noscale$pred - y.test) ^ 2) #0.016931

reg.knn.scale <- knn.reg(train = x.train.scale, test = x.test.scale, y = y.train, k = 7)
res.scale <- mean((reg.knn.scale$pred - y.test) ^ 2)#0.0168115

rbind(c("res.noscale", 'res.scale'), c(res.noscale, res.scale))


#Inutile pour l'instant----------------------------
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

set.seed(69)
KfoldsOuter <- 30 
erreurOuter <- rep(0, KfoldsOuter)
kOuter <- rep(0, KfoldsOuter)
foldsOuter <- sample(1:KfoldsOuter, n, replace=TRUE) 

for (i in 1:KfoldsOuter){
  print(i)
  x.train <- data[foldsOuter != i, -9]
  y.train <- data[foldsOuter != i, 9]
  x.test <- data[foldsOuter == i, -9]
  y.test <- data[foldsOuter == i, 9]
  
  fit <- knn.reg(train = x.train, test = x.test, y = y.train, k=KfoldsOuter)
  
  erreurOuter[i]<- mean((fit$pred - y.test) ^ 2)
}
print(erreurOuter) # 0.07113402 0.07581967 0.05456349 0.08437500 0.07614679 0.06315789 0.06868687 0.06360792 0.06122449 0.07142857
print(mean(erreurOuter)) # 0.06901447
print(sd(erreurOuter)) # 0.008670506
which.min(erreurOuter)
plot(erreurOuter, type = 'b')
