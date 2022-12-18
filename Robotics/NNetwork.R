##################
# Neural Network #
##################
if(!require("nnet")) {
  install.packages("nnet")
  library("nnet")
}

set.seed(69)
K<-20
folds=sample(1:K,n,replace=TRUE)
#CV sans scaling
CV.NN<-rep(0,K)
for(i in (1:20)){
  for(local_k in (1:K)){
    fitNN<-nnet(y ~., data = data[folds!= local_k, ], size = i, linout = TRUE)
    pred<-predict(fitNN,newdata=data[folds==local_k,])
    CV.NN[i]<-CV.NN[i]+ sum((data[folds==local_k, 9]- pred)^2)
  }
  CV.NN[i]<-CV.NN[i]/n
}
plot(1:K, CV.NN, type = 'b', col = 'blue')
which.min(CV.NN)

fitNN<-nnet(y ~., data = data.train, size = which.min(CV.NN), linout = TRUE)
predNN<-predict(fitNN,newdata=data.test)
mean((predNN - y.test) ^ 2)#0.01121872

