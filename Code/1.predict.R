source("Split.R")
library(tictoc)

tic()

#type = 1 (BUD) else (STELLA)
#NP=7
#lags=c(1:7)
#ts = split(type=0, NP, lags)

## Rminer Defeaut Predictions
#model=c("naive","ctree","cv.glmnet","rpart","kknn","ksvm","mlp","mlpe", "randomForest",
#        "xgboost", "cubist", "lm", "mr", "mars", "pcr", "plsr", "cppls", "rvm")

#Rminer_metrics = model_rminer(model, ts, NP)
#Rminer_metrics[order(Rminer_metrics$RMSE),]



# Rminer Fintuned Predictions
#Rminer_f_metrics = model_r_rminer(model, ts, NP)

### Finetune the model
#ml_models$xgboost()
#search=list(search=mparheuristic(model[i]))
#M=fit(y~.,data=ts$TR, model=model[i],search=search)

# Forecasting Predictions
#f_ts = split_ts(type=0,H=7,K=7)
#f_model_n = c("HW", "auto.arima", "ets", "nnetar")
#f_metrics = model_f(f_ts,f_model_n, h=7)

### Finetune the model


## (Weekly naive) Rolling and Growing window
#type=1
#metrics_g = model_f_rg(type,mode="incremental", Runs=20, K=7, Test=7)
#metrics_r = model_f_rg(type,mode='rolling', Runs=20, K=7, Test=7)
#weekly_naive(type, mse = metrics_g)

ml_model_n=c("naive","ctree","cv.glmnet","rpart","kknn","ksvm","mlp","mlpe", "randomForest",
             "xgboost", "cubist", "lm", "mr", "mars", "pcr", "plsr", "cppls", "rvm")

f_model_n = c("HW", "auto.arima", "ets", "nnetar")
metrics = model_f_rg(f_model_n = f_model_n, ml_model_n = ml_model_n)
rank_metrics[order(metrics$MSE),]

toc()