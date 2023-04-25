library(openxlsx)
library(rminer)
library(forecast)
df = read.xlsx(xlsxFile = "bebidas.xlsx", sheet=1, skipEmptyRows = FALSE,colNames = TRUE,detectDates = TRUE)

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


# SPlit the df
split = function(type=1, NP=140, lags=7){
  
  
  if(type==1){
    DS=df$BUD
  } else {
    DS=df$STELLA
  }
  
  S = CasesSeries(DS,c(1:lags))
  srange=diff(range(S))
  
  
  H=holdout(S$y,ratio=NP,mode="order")
  
  return(list(S=S, TR = S[H$tr,], TS = S[H$ts,]))
}

# Get RMSE from rminer models
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
    cat("RMSE:" ,RMSE, "\n")
    df_metrics[i, 1] = model[i]
    df_metrics[i, 2] = RMSE
  }
  
  return(df_metrics)
}


model_r_rminer = function(model, ts, NP=140){
  
  df_metrics = data.frame(matrix(ncol = 2, nrow = length(model)))
  colnames(df_metrics) = c("Model", "RMSE")
  init=nrow(ts$S) - NP + 1
  
  for(i in 1:length(model))
  {
    set.seed(24)
    
    cat("i:",i,"model:",model[i], "\n")
    P=ml_models[[i]](ts$S, ts$TR, init, NP)
    
    
    RMSE = round(mmetric(ts$TS$y,P,metric="RMSE"),1)
    cat("RMSE:" ,RMSE, "\n")
    df_metrics[i, 1] = model[i]
    df_metrics[i, 2] = RMSE
  }
  
  return(df_metrics)
}

# split time series
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

#time series (G/R Windows)
model_f_rg = function(type=1,mode="incremental", Runs=84, K=7, Test=7){
  if(type==1){
    TS=df$BUD
  } else {
    TS=df$STELLA
  }
  
  L=length(TS) # 730
  W=(L-Test)-(Runs-1)*S #Window 142 || 140+84*7 = 728
  
  timelags=c(1,7,8) 
  DS=CasesSeries(TS,timelags)
  
  SR=diff(range(TS))
  
  for(b in 1:Runs)  
  {
    H=holdout(TS,ratio=Test,mode=mode,iter=b,window=W,increment=S)   
    trinit=H$tr[1]
    dtr=ts(TS[H$tr],frequency=K)
    M=suppressWarnings(HoltWinters(dtr))
    Pred=forecast(M,h=length(H$ts))$mean[1:Test]
    ev[b]=mmetric(y=TS[H$ts],x=Pred,metric="NMAE",val=YR)
    
    
    H2=holdout(DS$y,ratio=Test,mode=mode,iter=b,window=W2,increment=S)   
    
    M2=fit(y~.,D[H2$tr,],model="mlpe")
    Pred2=lforecast(M2,D,start=(length(H2$tr)+1),Test)
    ev2[b]=mmetric(y=TS[H$ts],x=Pred2,metric="NMAE",val=YR)
    
    cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
        "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
        "nmae:",ev[b],",",ev2[b],"\n")
    mgraph(TS[H$ts],Pred,graph="REG",Grid=10,col=c("black","blue","red"),leg=list(pos="topleft",leg=c("target","HW pred.","mlpe")))
    lines(Pred2,pch=19,cex=0.5,type="b",col="red")
  }
  
  return(list(ev=ev, ev2=ev2))
}




#get RMSE from forecasting models
model_f = function(ts, model_n){
  df_metrics = data.frame(matrix(ncol = 2, nrow = length(model_n)))
  colnames(df_metrics) = c("Model", "RMSE")
  
  for(i in 1:length(model_n))
  {
    set.seed(24)
    
    cat("i:",i,"model:",model_n[i], "\n")
    P = f_models[[i]](ts$TR, h=7)
    RMSE = round(mmetric(ts$Y,P,metric="RMSE"),1)
    df_metrics[i, 1] = f_model_n[i]
    df_metrics[i, 2] = RMSE
  }
  return(df_metrics)
}



 


