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
