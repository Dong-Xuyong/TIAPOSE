source("Split.R")

ts = split(type=0)

model=c("naive","ctree","cv.glmnet","rpart","kknn","ksvm","mlp","mlpe", "randomForest",
        "xgboost", "cubist", "lm", "mr", "mars", "pcr", "plsr", "cppls", "rvm")


Rminer_metrics = model_rminer(model, ts)

# Base Predictions

Rminer_metrics[order(Rminer_metrics$RMSE),]


f_ts = split_ts()

f_model_n = c("HW", "auto.arima", "ets", "nnetar")

df_metrics = data.frame(matrix(ncol = 2, nrow = length(f_model_n)))
colnames(df_metrics) = c("Model", "RMSE")

for(i in 1:length(f_model_n))
{
  set.seed(24)
  
  cat("i:",i,"model:",f_model_n[i], "\n")
  P = f_models[[i]](f_ts$TR, h=7)
  RMSE = round(mmetric(f_ts$Y,P,metric="RMSE"),1)
  df_metrics[i, 1] = f_model_n[i]
  df_metrics[i, 2] = RMSE
}
df_metrics


f_metrics = model_forecasting(model_n = f_model_n, f_ts)

HW = c(HW = HoltWinters(f_ts$TR), AR = auto.arima(f_ts$TR))
F=forecast(HW$HW,h=7)

init= nrow(ts$S)-140
ml_models[[1]](ts$TR, init, 140)

ml_models[[1]]
