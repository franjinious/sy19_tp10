setwd("~/Travail/UTC/GI05/SY19/TD/td10/sy19_tp10")
#phonemes <- read.table('data/phoneme_train.txt')
#robotics <- read.table('data/robotics_train.txt')
communities <- read.csv('data/communities_train.csv')


#enlever les données non prédictives (données par le communities.names)
communities <- subset(communities, select = -c(county, community, communityname, fold))

#supprimer les colonnes qui ont un taux de NA supérieur à 50%
na_count <-sapply(communities, function(y) sum(length(which(is.na(y)))))
communities <- subset(communities, select = -c(which(na_count >= 500)))
#remplacer le NA restant dans OtherPerCap par la moyenne de cette colonne : 0.28 d'après communities.names
#on appelle ça l'imputation de valeurs manquantes. En en faisant, on crée un biais. Donc en faire un ou deux ça va, en faire plus c'est pas génial haha.
#ici, c'est un très bon compromis
communities$OtherPerCap[is.na(communities$OtherPerCap)] <- 0.28

communities.X <- subset(communities, select=-c(ViolentCrimesPerPop))
communities.y <- communities$ViolentCrimesPerPop
#calculer la corrélation sans la variable nominale state
cor.communities.X <- round(cor(subset(communities.X, select=-c(state))), 2)
#les variables sont assez corrélées entre elles


#split train/test
n<-nrow(communities)
train<-sample(1:n,round(2*n/3))
communities.train<- communities[train,]
communities.test<-communities[-train,]
communities.X.train<-communities.X[train,]
communities.X.test<- communities.X[-train,]
communities.y.train<-communities.y[train]
communities.y.test<-communities.y[-train]

#Backward subset selection
library(leaps)
reg.fit<-regsubsets(ViolentCrimesPerPop~.,data=communities,method='backward',nvmax=15)
plot(reg.fit,scale="adjr2")
plot(reg.fit,scale="bic")
coef(reg.fit, 10)

#Régression linéaire
fit.lm <- lm(communities.y.train~., data=communities.X.train)
fit.lm$xlevels[["state"]] <- union(fit.lm$xlevels[["state"]], levels(communities.X.test$state))
summary(fit.lm)

pred_lm <-  predict(fit.lm, newdata=communities.X.test)
err.lm <- mean((communities.y.test-pred_lm)^2)
err.lm
#err de 0.164
plot(communities.y.train, fitted(fit.lm))
abline(0,1)
#on est okay

#Régression linéaire avec 10 variables les plus significatives
#fit.lm <- lm(communities.y.train~racepctblack+PctKids2Par+state+PctRecImmig10+NumStreet+PctWorkMom+PctRecImmig10+MalePctDivorce+PctFam2Par+PctIlleg, data=communities.X.train)
fit.lm <- lm(communities.y.train~racepctblack+pctUrban+MalePctDivorce+PctKids2Par+PctPersDenseHous+PctHousOccup+RentLowQ+MedRent+MedOwnCostPctIncNoMtg+NumStreet, data=communities.X.train)
summary(fit.lm)

pred_lm <-  predict(fit.lm, newdata=communities.X.test)
err.lm.10vars <- mean((communities.y.test-pred_lm)^2)
err.lm.10vars
#err de 0.020
plot(communities.y.train, fitted(fit.lm))
abline(0,1)
#on est encore mieux !


#GAMs avec les 10 meileures variables
library(gam)
library(splines)
fit.gam<- gam(communities.y.train~ +racepctblack+pctUrban+MalePctDivorce+PctKids2Par+PctPersDenseHous+PctHousOccup+RentLowQ+MedRent+MedOwnCostPctIncNoMtg+NumStreet, data=communities.X.train)
plot(fit.gam, col="red", residuals=TRUE)
pred_gam <- predict(fit.gam, newdata=communities.X.test)
err.gam <-  mean((communities.y.test-pred_gam)^2)
err.gam
#err de 0.020
#peut-être peut on améliorer ce modèle de GAM avec des splines ?


#Ridge regression and lasso
library(glmnet)
x<- model.matrix(ViolentCrimesPerPop~., communities)
y<-communities.y
n<-nrow(x)
ntrain=2*n/3
ntest=n-ntrain
train<-sample(1:ntrain)
xtrain<-x[train,]
ytrain<-y[train]
xtest<-x[-train,]
ytest<-y[-train]

cv.out.ridge<-cv.glmnet(xtrain,ytrain,alpha=0)
cv.out.lasso<-cv.glmnet(xtrain,ytrain,alpha=1)
plot(cv.out.ridge)
plot(cv.out.lasso)

fit.ridge<-glmnet(xtrain,ytrain,lambda=cv.out.ridge$lambda.min,alpha=0)
ridge.pred<-predict(fit.ridge,s=cv.out.ridge$lambda.min,newx=xtest)
err.ridge <- mean((ytest - ridge.pred)^2)
err.ridge
#err de 0.018

fit.lasso<-glmnet(xtrain,ytrain,lambda=cv.out.lasso$lambda.min,alpha=1)
lasso.pred<-predict(fit.lasso,s=cv.out.lasso$lambda.min,newx=xtest)
err.lasso <- mean((ytest - lasso.pred)^2)
err.lasso
#err de 0.018
#lequel est le meilleur ? on voit que beaucoup de paramètres lambda dans le lasso sont mis à 0. Permet de simplifier le modèle


#Arbre de décision
#avec "state"
library(rpart)
fit.tree <-rpart(communities.y.train~.,data=communities.X.train,method="anova")
library(rpart.plot)
rpart.plot(fit.tree, box.palette="RdBu", shadow.col="gray",fallen.leaves=FALSE)
predict <- predict(fit.tree, newdata=communities.X.test)
err.tree<- mean((communities.y.test - predict)^2)
err.tree

printcp(fit.tree)
plotcp(fit.tree)

i.min <- which.min(fit.tree$cptable[,4])
cp.opt1 <- fit.tree$cptable[i.min]

pruned_tree <- prune(fit.tree, cp= cp.opt1)
rpart.plot(pruned_tree, box.palette="RdBu", shadow.col = "gray")
predict <- predict(pruned_tree, newdata=communities.X.test)
err.pruned_tree <- mean((communities.y.test - predict)^2)
err.pruned_tree
#0.02476843

#Random Forest
library(randomForest)
fit.RF<-randomForest(communities.y.train~.,data=communities.X.train,mtry=5,importance=TRUE)
pred.RF<- predict(fit.RF,newdata=communities.X.test,type="response")
err.RF<- mean((communities.y.test - pred.RF)^2)
err.RF
varImpPlot(fit.RF, n.var = 10)

#SVM
library(kernlab)
CC<-c(0.001, 0.01,0.1,1,10)
N<-length(CC)
err<-rep(0,N)
for(i in 1:N){
  err[i]<-cross(ksvm(communities.y.train~.,data=communities.X.train,,type="eps-svr",kernel="vanilladot",C=CC[i],cross=5))
}
plot(CC,err,type="b",log="x",xlab="C",ylab="CV error")
#meilleur valeur de C : 0.01

svmfit<-ksvm(communities.y.train~.,data=communities.X.train,,type="eps-svr",kernel="vanilladot",C=0.01)
pred.svm<-predict(svmfit,newdata=communities.X.test)
err.svm <- mean((pred.svm - communities.y.test)^2)
err.svm
#err de 0.024

#Réseaux de neurones
#suppression du facteur state
communities <- subset(communities, select=-state)
communities.train <- subset(communities.train, select=-state)
communities.test <- subset(communities.test, select=-state)
library(nnet)
set.seed(42)
K<-20
folds=sample(1:K,n,replace=TRUE)
#CV sans scaling
CV.NN<-rep(0,K)
for(i in (1:20)){
  for(local_k in (1:K)){
    fitNN<-nnet(ViolentCrimesPerPop ~., data = communities[folds!= local_k, ], size = i, linout = TRUE)
    pred<-predict(fitNN,newdata=communities[folds==local_k,])
    CV.NN[i]<-CV.NN[i]+ sum((communities[folds==local_k, "ViolentCrimesPerPop"]- pred)^2)
  }
  CV.NN[i]<-CV.NN[i]/n
}
plot(1:K, CV.NN, type = 'b', col = 'blue')
which.min(CV.NN)

fitNN<-nnet(ViolentCrimesPerPop ~., data = communities.train, size = 5, linout = TRUE)
predNN<-predict(fitNN,newdata=communities.test)
mean((predNN - communities.y.test) ^ 2)
#err de 0.0399 ou au dessus. Pas génial ...



#ACP //on ne peut pas utiliser l'attribut "state" ici
pca<-prcomp(subset(communities.X, select=-c(state)))
lambda<-pca$sdev^2
plot(cumsum(lambda)/sum(lambda),type="l",xlab="q", ylab="proportion of explained variance")
summary(pca)
#20 premières composantes principales pour 90% de variance expliquée. On passe de 128 variables explicatives à 20 composantes
#https://stats.stackexchange.com/questions/72839/how-to-use-r-prcomp-results-for-prediction
pca$x<-pca$x[,1:20]
plot(pca$x)

#Boxplot de tous les MSE
errors <- as.data.frame(cbind(err.lm, err.lm.10vars, err.gam, err.ridge, err.lasso, err.pruned_tree, err.RF, err.svm))
boxplot(errors)

#finalData <- as.data.frame(cbind(err.lin.mse, CV.noscale, err.las.mse, err.rid.mse, err.ela.mse, err.splinesnaturel.mse,err.gam.mse, err.tree.mse, err.tree.mse.prune, err.randomForest.mse, err.svmlin.mse, err.svmlap.mse, err.svmgau.mse))

#colnames(finalData) <- c("Linear model", "KNN", "Lasso", "Ridge", "ElasticNet", "NaturalSpline", "GAM", "Decision Tree", "Decision Tree with pruning", "RandomForest", "SVM Linear", "SVM Laplace", "SVM Gaussian")

#boxplot(finalData, cex.axis=0.4, ylab = "MSE par Cross Validation")
