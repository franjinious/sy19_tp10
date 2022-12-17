setwd("D:/нд╣╣/SY19/TD/TD10")

data <- read.table("robotics_train.txt", header = TRUE)
data.scale <- scale(data)
data1 <- read.table("phoneme_train.txt", header = TRUE)
data2 <- read.csv("communities_train.csv")
#Robotics


#Partition des donnees
set.seed(69)
n <- nrow(data)
p <- ncol(data) - 1
percentage <- 2/3
set.seed(69)
ntrain <- as.integer(n * percentage)
ntest <- n - ntrain
id.train <- sample(n, ntrain)
data.train <- data[id.train, ]
data.train.scale <- scale(data[id.train, ])

data.test <- data[-id.train, ]

y.train <- data.train[, 9]
x.train <- data.train[, -9]
y.test <- data.test[, 9]
x.test <- data.test[, -9]

y.train.scale <- scale(data.train[, 9])
x.train.scale <- scale(data.train[, -9])
y.test.scale <- scale(data.test[, 9])
x.test.scale <- scale(data.test[, -9])
#data.scale <- scale(data)

#Correlation entre les variables
# Load r packages (Install if missing)
if(!require("corrplot")) {
  install.packages("corrplot")
  library("corrplot")
}

mcor <- cor(data[,c(1:9)])
corrplot(mcor, type="upper",method = "number",order="hclust", tl.col="black", tl.srt=45)#Les variables ne sont pas fortement correle

panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  text(0.5, 0.5, txt)
}
pairs(data[, c(1:9)], lower.panel = panel.cor)



#PCA
if(!require("FactoMineR")) {
  install.packages("FactoMineR")
  library("FactoMineR")
}

if(!require("factoextra")) {
  install.packages("factoextra")
  library("factoextra")
}

res.pca <- PCA(data, ncp=p, quali.sup = p+1, graph = FALSE)
print(res.pca)
fviz_eig(res.pca, ncp = 15, addlabels = TRUE, ylim = c(0, 60))#On voit que la variance sont expliquee par toutes les variables

#pca <- princomp(data[,1:p]) 
#x.pca <- cbind(as.data.frame(pca$scores[,1:15]),y)

