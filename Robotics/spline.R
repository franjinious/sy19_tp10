###########
# Splines #
###########

#PRESQUE FINI#

#prepararion des folds
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

err.splinesnaturel.mse <- rep(0,10)
err.gam.mse <- rep(0,10)

p.sequences <- seq(1,10)

set.seed(69)
#gam------------------------------------
#Packages
if(!require("mgcv")) {
  install.packages("mgcv")
  library("mgcv")
}

K<-10
folds=sample(1:K,n,replace=TRUE)
CV<-rep(0,10)
for(i in (1:10)){
  for(k in (1:K)){
    fit<-knn.reg(train = x.train.scale[folds!=k,], test = x.test.scale[folds==k,], y = y.train.scale[folds==k, ], k = local_k)
    pred<-predict(fit,newdata=x.test[folds==k,])
    CV[i]<-CV[i]+ sum((y.train[folds==k,]- pred)^2)
  }
  CV[i]<-CV[i]/n
}
fit.gam <- gam(formula = y ~ s(X1)+s(X2)+s(X3)+s(X4)+s(X5)+s(X6)+s(X7)+s(X8), 
               data = data.train, family = gaussian, method = "GCV")
pre.gam <- predict(fit.gam, newdata = x.test)
mean((pre.gam - y.test)^2)#0.0392
vis.gam(fit.gam, plot.type = "contour")
plot(fit.gam, select = 2)


#natural spline----------------------------
if(!require("stringi")) {
  install.packages("stringi")
  library("stringi")
}
if(!require("splines")) {
  install.packages("splines")
  library("splines")
}

#fit.spline <- smooth.spline(as.vector(x.train), y.train, cv = FALSE)



#Modele avec validation croisee interne
for (i in 1:10) {
  index.outer.cv <- idx.test.outer[[i]]
  data.inner <- data[-index.outer.cv,]
  data.validation <- data[index.outer.cv,]
  data.inner <- data.inner[rs.data.inner[[i]], ]
  
  sp.pmin <- rep(0, length(p.sequences))
  for(p in 1:length(p.sequences)){
    for(j in 1:10){
      index.inner.cv <- idx.test.inner[[i]][[j]] 
      data.train.ns <- data.inner[-index.inner.cv, ]
      data.test.ns <- data.inner[index.inner.cv, ]
      model.spline <- lm(y ~ns(X1, p.sequences[p])+ns(X2,p.sequences[p])+ns(X3, p.sequences[p])
                         +ns(X4, p.sequences[p])+ns(X5, p.sequences[p])+ns(X6, p.sequences[p])
                         +ns(X7, p.sequences[p])+ns(X8, p.sequences[p]), data=data.train.ns) 
      model.pred <- predict(model.spline,newdata=data.test.ns)
      sp.pmin[p] <-  sp.pmin[p] + mean((data.test.ns[, 9] -model.pred)^2)
    }
  }
  idx.pmin <- which(min(sp.pmin) == sp.pmin)
  best.pmin <- p.sequences[idx.pmin]
  
  data.train.ns <- data.inner
  data.test.ns <- data.validation
  
  naturalspline <- lm(y ~ ns(X1, p.sequences[p])+ns(X2,p.sequences[p])+ns(X3, p.sequences[p])
                      +ns(X4, p.sequences[p])+ns(X5, p.sequences[p])+ns(X6, p.sequences[p])
                      +ns(X7, p.sequences[p])+ns(X8, p.sequences[p]), data=data.train.ns)
  pred <- predict(naturalspline, newdata=data.test.ns)
  err.splinesnaturel.mse[i] = mean((data.test.ns[, 9] -pred)^2)
}

mean(err.splinesnaturel.mse)#0.1033425

#Smoothing Spline------------------------
err.smoothspline.mse <- rep(0, 10)

#Modele avec validation croisee interne
for (i in 1:10) {
  index.outer.cv <- idx.test.outer[[i]]
  data.inner <- data[-index.outer.cv,]
  data.validation <- data[index.outer.cv,]
  data.inner <- data.inner[rs.data.inner[[i]], ]
  
  sp.pmin <- rep(0, length(p.sequences))
  for(p in 1:length(p.sequences)){
    for(j in 1:10){
      index.inner.cv <- idx.test.inner[[i]][[j]] 
      data.train.sp <- data.inner[-index.inner.cv, ]
      data.test.sp <- data.inner[index.inner.cv, ]
      model.spline <- lm(y ~s(X1, p.sequences[p])+s(X2,p.sequences[p])+s(X3, p.sequences[p])
                         +s(X4, p.sequences[p])+s(X5, p.sequences[p])+s(X6, p.sequences[p])
                         +s(X7, p.sequences[p])+s(X8, p.sequences[p]), data=data.train.sp) 
      model.pred <- predict(model.spline,newdata=data.test.sp)
      sp.pmin[p] <-  sp.pmin[p] + mean((data.test.sp -model.pred)^2)
    }
  }
  idx.pmin <- which(min(sp.pmin) == sp.pmin)
  best.pmin <- p.sequences[idx.pmin]
  
  data.train.sp <- data.inner
  data.test.sp <- data.validation
  
  smoothspline <- smooth.spline()
    
  #lm(y ~ s(as.vector(X1), p.sequences[p])+s(X2,p.sequences[p])+s(X3, p.sequences[p])
                  #    +s(X4, p.sequences[p])+s(X5, p.sequences[p])+s(X6, p.sequences[p])
                  #   +s(X7, p.sequences[p])+s(X8, p.sequences[p]), data=data.train.sp)
  pred <- predict(naturalspline, newdata=data.test.sp)
  err.splinesnaturel.mse[i] = mean((y.test -pred)^2)
}

mean(err.splinesnaturel.mse)#0.1033425

tmp <- smooth.spline(x = x.train, y = y.train, cv = FALSE, df = 10)

