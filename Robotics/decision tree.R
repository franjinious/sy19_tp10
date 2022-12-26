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
err.tree.mse <- rep(0,10)

for (i in 1:10){
  index.outer.cv <- idx.test.outer[[i]]
  data.inner <- data[-index.outer.cv,]
  data.validation <- data[index.outer.cv,]
  
  
  data.train.tree <- data.inner
  data.test.tree <- data.validation
  
  model.tree <- rpart(y~., data=data.train.tree, method="anova", control = rpart.control(xval = 10, minbucket = 2))
  pred <- predict(model.tree, newdata=data.test.tree)
  err.tree.mse[i] = mean((data.test.tree$y -pred)^2)
  
}

#Prune
err.tree.mse.prune <- rep(0,10)

for (i in 1:10){
  index.outer.cv <- idx.test.outer[[i]]
  data.inner <- data[-index.outer.cv,]
  data.validation <- data[index.outer.cv,]
  
  
  data.train.tree <- data.inner
  data.test.tree <- data.validation
  
  model.tree <- rpart(y~., data=data.train.tree, method="anova", control = rpart.control(xval = 10, minbucket = 2))
  pred <- predict(model.tree, newdata=data.test.tree)
  err.tree.mse[i] = mean((data.test.tree$y -pred)^2)
  i.min<-which.min(model.tree$cptable[,4])
  cp.opt1<-model.tree$cptable[i.min,1]
  
  pruned_tree <- prune(model.tree, cp=cp.opt1)
  preDT.prune <- predict(pruned_tree, newdata = data.test.tree)
  err.tree.mse.prune[i] <- mean((data.test.tree[, 9] - preDT.prune)^2)#0.0472833
}
boxplot(err.tree.mse, err.tree.mse.prune)
#Graphe de l'arbre
if(!require("rpart.plot")) {
  install.packages("rpart.plot")
  library("rpart.plot")
}
rpart.plot(fitDT, box.palette="RdBu", shadow.col="gray", fallen.leaves=FALSE)# Arbre initial
rpart.plot(pruned_tree, box.palette="RdBu", shadow.col="gray", fallen.leaves=FALSE)# Arbre elague

#Bagging

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

