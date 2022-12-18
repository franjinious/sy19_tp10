#######
# SVM #
#######

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
plot(CC,Err,type="b",log="x",xlab="C",ylab="CV error")
which.min(Err)

svmLinFit <- ksvm(y ~.,data = data.train, type="eps-svr",
                  kernel="vanilladot",C=CC[which.min(Err)])
svmLinPre <- predict(svmLinFit, newdata = x.test)
mean((svmLinPre-y.test)^2)#0.04289

#SVM laplacien--------------------------------
#Cross validation pour la valeur de C
#CC<-c(0.001,0.01,0.1,1,10,100)
#N<-length(CC)
#M<-5 # nombre de r??p??titions de la validation crois??e
errLap<-matrix(0,N,M)

set.seed(69)
for(k in 1:M){
  for(i in 1:N){
    errLap[i,k]<-cross(ksvm(y ~.,data = data.train, type="eps-svr",kernel="laplacedot",C=CC[i],cross = M))
  }
}
ErrLap<-rowMeans(errLap)
plot(CC,ErrLap,type="b",log="x",xlab="C",ylab="CV error")
which.min(ErrLap)
svmLapFit <- ksvm(y ~.,data = data.train, type="eps-svr",kernel="laplacedot",C=CC[which.min(ErrLap)])
svmLapPre <- predict(svmLapFit, newdata = x.test)
mean((svmLapPre-y.test)^2)#0.008838603


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

