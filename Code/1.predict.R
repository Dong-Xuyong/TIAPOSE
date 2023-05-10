source("Split.R")
library(tictoc)

tic()

type = 0 # (BUD) else (STELLA)
NP=140
lags=c(1:7)
#lags=(1:14)
#lags=c(1,2,3,7)

#ts = split(type=0, NP, lags)

## Rminer Defeaut Predictions
model=c("naive","ctree","cv.glmnet","rpart","kknn","ksvm","mlp","mlpe", "randomForest",
        "xgboost", "cubist", "lm", "mr", "mars", "pcr", "plsr", "cppls", "rvm")

metrics = model_rminer(model, ts, NP)
#Rminer_metrics[order(Rminer_metrics$RMSE),]





# Rminer Fintuned Predictions
#Rminer_f_metrics = model_r_rminer(model, ts, NP)

### Finetune the model
#ml_models$xgboost()
#search=list(search=mparheuristic(model[i]))
#M=fit(y~.,data=ts$TR, model=model[i],search=search)

# Forecasting Predictions
f_ts = split_ts(type=1,H=140,K=7)
f_model_n = c("HW", "auto.arima", "ets", "nnetar")
metrics = model_f(f_ts,f_model_n, h=7)

rank_metrics = metrics[order(metrics$RMSE),]


write.csv(rank_metrics, "metrics/bud_split_f.csv")
### Finetune the model


## (Weekly naive) Rolling and Growing window
type=1
metrics_g = weekly_naive_model_f_rg(type,mode="incremental", Runs=20, K=7, Test=7)

metrics_wn = weekly_naive(type, mse = metrics_g)

loss_percen = round((median(metrics_wn) - median(metrics_g$ev2)) / median(metrics_wn), 2)
loss_percen


ml_model_n=c("naive","ctree","cv.glmnet","rpart","kknn","ksvm","mlp","mlpe", "randomForest",
             "xgboost", "cubist", "lm", "mr", "mars", "pcr", "plsr", "cppls", "rvm")

f_model_n = c("HW", "auto.arima", "ets", "nnetar")

lags=1:7 
#lags=1:14

#lags=c(1,2,3,7)

metrics = model_f_rg(type=1, f_model_n = f_model_n, ml_model_n = ml_model_n, mode="incremental", timelags=lags)
rank_metrics = metrics[order(metrics$MSE),]
rank_metrics

write.csv(rank_metrics, "metrics/bud_g_1_7.csv")


stella_metrics = read.csv("metrics/bud_g.csv")
bud_metrics = read.csv("metrics/stella_g_1_7.csv")
stella_metrics
bud_metrics
toc()