##########################
# Ridge + Lasso +elastic #
##########################

#Preparation des librairies et des donnees---------------------

#Librarie
if(!require("glmnet")) {
  install.packages("glmnet")
  library("glmnet")
}

#Donnees
x<-model.matrix(y~.,data)
y<-data$y
x.train.regu <- x[id.train,]
y.train.regu <- y[id.train]
x.test.regu <- x[-id.train,]
y.test.regu <- y[-id.train]

#Ridge regression----------------------------
cv.out.ridge <- cv.glmnet(x.train.regu, y.train.regu, alpha = 0, type.measure = "mse")
plot(cv.out.ridge)
fit.ridge <- glmnet(x.train.regu, y.train.regu, lambda = cv.out.ridge$lambda.min, alpha = 0)
ridge.predict <- predict(fit.ridge, s = cv.out.ridge$lambda.min, newx = x.test.regu)
mse.ridge <- mean((ridge.predict - y.test.regu) ^ 2)#0.04147

#Lasso regression-----------------------------
cv.out.lasso <- cv.glmnet(x.train.regu, y.train.regu, alpha = 1, type.measure = "mse", family = "gaussian")
plot(cv.out.lasso)
fit.lasso <- glmnet(x.train.regu, y.train.regu, lambda = cv.out.lasso$lambda.min, alpha = 0)
lasso.predict <- predict(fit.lasso, s = cv.out.lasso$lambda.min, newx = x.test.regu)
mse.lasso <- mean((lasso.predict - y.test.regu) ^ 2)#0.0413992

#elastic
cv.out.elastic <- cv.glmnet(x.train.regu, y.train.regu, alpha = .5, type.measure = "mse", family = "gaussian")
plot(cv.out.elastic)
fit.elastic <- glmnet(x.train.regu, y.train.regu, lambda = cv.out.elastic$lambda.min, alpha = .5)
elastic.predict <- predict(fit.elastic, s = cv.out.elastic$lambda.min, newx = x.test.regu)
mse.elastic <- mean((elastic.predict - y.test.regu)^2) #0.4138744
