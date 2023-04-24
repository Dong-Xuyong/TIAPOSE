source("Split.R")

ts = split()

model=c("naive","ctree","cv.glmnet","rpart","kknn","ksvm","mlp","mlpe", "randomForest",
        "xgboost", "cubist", "lm", "mr", "mars", "pcr", "plsr", "cppls", "rvm")


Rminer_metrics = model_rminer(model, ts)

# Base Predictions






df_metrics[order(df_metrics$RMSE),]









