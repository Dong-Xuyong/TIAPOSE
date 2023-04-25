source("Split.R")

#type = 1 (BUD) else (STELLA)
NP=7
ts = split(type=0, NP=7)

# Rminer Defeaut Predictions
model=c("naive","ctree","cv.glmnet","rpart","kknn","ksvm","mlp","mlpe", "randomForest",
        "xgboost", "cubist", "lm", "mr", "mars", "pcr", "plsr", "cppls", "rvm")

Rminer_metrics = model_rminer(model, ts, NP)
Rminer_metrics[order(Rminer_metrics$RMSE),]





# Rminer Fintuned Predictions
Rminer_f_metrics = model_r_rminer(model, ts)

### Finetune the model


# Forecasting Predictions
f_ts = split_ts()
f_model_n = c("HW", "auto.arima", "ets", "nnetar")
f_metrics = model_f(f_ts,f_model_n)

### Finetune the model


## Rolling and Growing window
metrics_g = model_f_rg()
metrics_r = model_f_rg(mode='rolling')

cat("Growing Window median NMAE values for HW and mlpe:\n")
cat("Holt-Winters",median(metrics_g$ev),"\n")
cat("mlpe",median(metrics_g$ev2),"\n")

cat("Growing Window median NMAE values for HW and mlpe:\n")
cat("Holt-Winters",median(metrics_r$ev),"\n")
cat("mlpe",median(metrics_r$ev2),"\n")


