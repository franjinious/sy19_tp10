#################
# Decision Tree #
#################

#FINI#

#TREE----------------------------

#Package
if(!require("rpart")) {
  install.packages("rpart")
  library("rpart")
}

#Preparation des folds
K <- 10
folds <- sample(1:K, n, replace = TRUE)
CV.DT <- rep(0, 20)

#Fit Decision Tree initial
for (i in (1:20)) {
  for (k in 1:K) {
    fitDT<-rpart(y~.,data=data,subset=which(folds != k),method="anova")
    preDT <- predict(fitDT, newdata = data[folds == k,])
    CV.DT[i]<-CV.DT[i]+ sum((data[folds==k, 9]- preDT)^2)
  }
  CV.DT[i] <- CV.DT[i] / n #0.04723086 0.04701533
}
which.min(CV.DT)
min(CV.DT)


#Fit Decision Tree avec CV

fitDT<-rpart(y~.,data=data,subset=id.train,method="anova",
            control = rpart.control(xval = 10, minbucket = 2,))

printcp(fitDT)
plotcp(fitDT)
preDT <- predict(fitDT, newdata = data.test)
errDT <- mean((y.test - preDT)^2)#0.0472833

#Choix de l'erreur minimale
i.min<-which.min(fitDT$cptable[,4])
cp.opt1<-fitDT$cptable[i.min,1]

#Prune
pruned_tree <- prune(fitDT, cp=cp.opt1)

preDT.prune <- predict(pruned_tree, newdata = data.test)
errDT.prune <- mean((y.test - preDT)^2)#0.0472833

#Graphe de l'arbre
if(!require("rpart.plot")) {
  install.packages("rpart.plot")
  library("rpart.plot")
}
rpart.plot(fitDT, box.palette="RdBu", shadow.col="gray", fallen.leaves=FALSE)# Arbre initial
rpart.plot(pruned_tree, box.palette="RdBu", shadow.col="gray", fallen.leaves=FALSE)# Arbre elague

#Bagging

##FAUX
if(!require("randomForest")) {
  install.packages("randomForest")
  library("randomForest")
}

K <- 10
folds <- sample(1:K, n, replace = TRUE)
CV <- rep(0, 20)

for (i in (1:20)) {
  for (k in 1:K) {
    fitRF<-randomForest(y~.,data=data,subset=which(folds != k),mtry = p)
    preRF <- predict(fitRF, newdata = data[folds == k,])#直接做结果为0.02511314
    CV[i]<-CV[i]+ sum((data[folds==k, 9]- preDT)^2)
  }
  CV[i] <- CV[i] / n #0.04723086
}
##Fin FAUX


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

err.randomForest.mse <- rep(0,10)
err.gam.mse <- rep(0,10)

p.sequences <- seq(1,10)

set.seed(69)
#Modele avec validation croisee interne
for (i in 1:10) {
  index.outer.cv <- idx.test.outer[[i]]
  data.inner <- data[-index.outer.cv,]
  data.validation <- data[index.outer.cv,]
  data.train.RF <- data.inner[-index.inner.cv, ]
  data.test.RF <- data.inner[index.inner.cv, ]
  
  fitRF<-randomForest(y~.,data=data.train.RF,mtry = p)
  preRF <- predict(fitRF, newdata = data.test.RF)
  err.randomForest.mse[i] = mean((data.test.RF[, 9] - preRF)^2)
}

mean(err.randomForest.mse)#0.02532425

