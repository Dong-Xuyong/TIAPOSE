library(openxlsx)
library(rminer)
library(forecast)
df = read.xlsx(xlsxFile = "bebidas.xlsx", sheet=1, skipEmptyRows = FALSE,colNames = TRUE,detectDates = TRUE)



# Falta os splits de Growing Window e Rolling window
split_ts = function(type=1, H=7, K=7){
  
  if(type==1){
    TS=df$BUD
  } else {
    TS=df$STELLA
  }
  
  L=length(TS)
  LTR=L - H
  
  TR = ts(TS[1:LTR],frequency=K)

  Y=TS[(LTR+1):L]
  
  return(list(TR=TR, Y=Y))
}



# SPlit the main dataset
split = function(type=1, NPRED=140, lags=7){
  
  
  if(type==1){
    DS=df$BUD
  } else {
    DS=df$STELLA
  }
  
  S = CasesSeries(DS,c(1:lags))
  srange=diff(range(S))
  
  
  H=holdout(S$y,ratio=NPRED,mode="order")
  
  return(list(S=S, TR = S[H$tr,], TS = S[H$ts,]))
}

model_rminer = function(model, ts, NP=140){
  
  df_metrics = data.frame(matrix(ncol = 2, nrow = length(model)))
  colnames(df_metrics) = c("Model", "RMSE")
  for(i in 1:length(model))
  {
    set.seed(24)
    
    cat("i:",i,"model:",model[i], "\n")
    search=list(search=mparheuristic(model[i]))
    M=fit(y~.,data=ts$TR, model=model[i],search=search,fdebug=TRUE)
    init = nrow(ts$S) -NP + 1
    P=lforecast(M,ts$S,init, NP)
    
    
    RMSE = round(mmetric(ts$TS$y,P,metric="RMSE"),1)
    df_metrics[i, 1] = model[i]
    df_metrics[i, 2] = RMSE
  }
  
  return(df_metrics)
}

f_models = list(
  "HW" = function(x, h) {return(forecast(HoltWinters(x), h = h)$mean[1:h])},
  "ets" = function(x, h) {return(forecast(ets(x), h = h)$mean[1:h])},
  "arima" = function(x, h) {return(forecast(auto.arima(x), h = h)$mean[1:h])},
  "nnectar" = function(x, h) {return(forecast(nnetar(x,P=1,repeats=3), h = h)$mean[1:h])}
)

ml_models = list(
  "naive" = function(S, x, init, NP) { return(lforecast(fit(y~., x, model="naive"), S, init, NP)) },
  "ctree" = function(S, x, init, NP) { return(lforecast(fit(y~., x, model="ctree"), S, init, NP)) },
  "cv.glmnet" = function(S, x, init, NP) { return(lforecast(fit(y~., x, model="cv.glmnet"), S, init, NP)) },
  "rpart" = function(S, x, init, NP) { return(lforecast(fit(y~., x, model="rpart"), S, init, NP)) },
  "kknn" = function(S, x, init, NP) { return(lforecast(fit(y~., x, model="kknn"), S, init, NP)) },
  "ksvm" = function(S, x, init, NP) { return(lforecast(fit(y~., x, model="ksvm"), S, init, NP)) },
  "mlp" = function(S, x, init, NP) { return(lforecast(fit(y~., x, model="mlp"), S, init, NP)) },
  "mlpe" = function(S, x, init, NP) { return(lforecast(fit(y~., x, model="mlpe"), S, init, NP)) },
  "randomForest" = function(S, x, init, NP) { return(lforecast(fit(y~., x, model="randomForest"), S, init, NP)) },
  "xgboost" = function(S, x, init, NP) { return(lforecast(fit(y~., x, model="xgboost"), S, init, NP)) },
  "cubist" = function(S, x, init, NP) { return(lforecast(fit(y~., x, model="cubist"), S, init, NP)) },
  "lm" = function(S, x, init, NP) { return(lforecast(fit(y~., x, model="lm"), S, init, NP)) },
  "mr" = function(S, x, init, NP) { return(lforecast(fit(y~., x, model="mr"), S, init, NP)) },
  "mars" = function(S, x, init, NP) { return(lforecast(fit(y~., x, model="mars"), S, init, NP)) },
  "pcr" = function(S, x, init, NP) { return(lforecast(fit(y~., x, model="pcr"), S, init, NP)) },
  "plsr" = function(S, x, init, NP) { return(lforecast(fit(y~., x, model="plsr"), S, init, NP)) },
  "cppls" = function(S, x, init, NP) { return(lforecast(fit(y~., x, model="cppls"), S, init, NP)) },
  "rvm" = function(S, x, init, NP) { return(lforecast(fit(y~., x, model="rvm"), S, init, NP)) }
)
 


