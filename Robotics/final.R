if(!require("kernlab")) {
  install.packages("kernlab")
  library("kernlab")
}

data <- read.table("robotics_train.txt", header = TRUE)
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

model.robotics <- ksvm(y~.,data=data.train,type="eps-svr", kernel="rbfdot",C=100)

prediction_robotics <- function(dataset) {
  pre <- predict(model.robotics, newdata = dataset)
  print(mean((dataset$y - pre)^2))
  return(pre)
}

test <- prediction_robotics(data.test)

save(
  "model.robotics",
  "prediction_robotics",
  file = "env.Rdata"
)
