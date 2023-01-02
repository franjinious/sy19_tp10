if(!require("kernlab")) {
  install.packages("kernlab")
  library("kernlab")
}

data_robo <- read.table("robotics_train.txt", header = TRUE)
set.seed(69)

model.robotics <- ksvm(y~.,data=data_robo,type="eps-svr", kernel="rbfdot",C=100)

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
