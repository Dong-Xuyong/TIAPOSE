source("Split.R")
library(tictoc)

tic()

type = 0 # (BUD) else (STELLA)
NP=140
lags=c(1:7)
#lags=(1:14)
#lags=c(1,2,3,7)
mode="incremental"
K=7

ml_model_n=c("naive","ctree","cv.glmnet","rpart","kknn","ksvm","mlp","mlpe", "randomForest",
             "xgboost", "cubist", "lm", "mr", "mars", "pcr", "plsr", "cppls", "rvm")

f_model_n=c("HW", "auto.arima", "ets", "nnetar")

# Run by each model
best_g_stella_model="lm"
best_g_bud_model="lm"
best_split_bud_model="mars"
best_split_stella_model="HW"

P_stella = select_split_model(model=best_split_stella_model, type=0, K=K, lags=lags)
P_G_stella = select_model(best_g_stella_model, type=0,mode=mode, K=K, lags=lags)

P_bud = select_split_model(model=best_split_bud_model, type=1, K=K, lags=lags)
P_G_bud = select_model(best_g_bud_model, type=1,mode=mode, K=K, lags=lags)

#pipeline forecast and ml models
metrics = model_f_ml(type=type, f_model_n, ml_model_n, lags=lags, NP=NP)

#pipeline G/R forecast and ml models
metrics_GW = model_f_rg(type=type, f_model_n = f_model_n, ml_model_n = ml_model_n, mode=mode, lags=lags)



#Save and read files
write.csv(metrics_GW, "metrics/bud_g_1_7.csv")
write.csv(metrics, "metrics/bud_split_1_7.csv")

stella_metrics = read.csv("metrics/stella_g_1_7.csv")
bud_metrics = read.csv("metrics/bud_g_1_7.csv")

stella_metrics
bud_metrics


#input for the interface
bud_model="ets"
stella_model="pcr"
week=1
input = model(week=week, bud_model=bud_model, stella_model=stella_model)
toc()
