library(openxlsx)
library(rminer)
library(forecast)
library(stats)

df = read.xlsx(xlsxFile = "bebidas.xlsx", sheet=1, skipEmptyRows = FALSE,colNames = TRUE,detectDates = TRUE)
P_lstm = read.csv("LSTM_pred.csv", header = FALSE)
length(P_lstm$V1)

weekly_naive= function(type=1){
  if(type==1){
    DS=df$BUD
  } else {
    DS=df$STELLA
  }
  
  L=length(DS)
  ev=vector(length=20)
  ini = L - (length(ev)+1) * 7 + 1
  
  for(i in 1:length(ev)){
    aux = ini + (i*7) - 7
    P = DS[(aux-7):(aux-1)]
    Y = DS[aux:(aux+6)]

    ev[i] = mmetric(P,Y,metric="MSE")
  }
  med_ev = mean(ev)

  return(list(ev = ev,med_ev= med_ev))
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


# SPlit the df
split = function(type=1, NP=140, lags){
  
  
  if(type==1){
    DS=df$BUD
  } else {
    DS=df$STELLA
  }
  
  S = CasesSeries(DS,lags)
  srange=diff(range(S))
  
  
  H=holdout(S$y,ratio=NP,mode="order")
  
  return(list(S=S, TR = S[H$tr,], TS = S[H$ts,]))
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
    P=round(lforecast(M,ts$S,init, NP),0)
    P[P < 0] = 0
    
    RMSE = round(mmetric(ts$TS$y,P,metric="RMSE"),1)
    cat("RMSE:" ,RMSE, "\n")
    df_metrics[i, 1] = model[i]
    df_metrics[i, 2] = RMSE
  }
  
  return(df_metrics)
}


#get RMSE from forecasting models
model_f = function(ts, model_n,h){
  df_metrics = data.frame(matrix(ncol = 2, nrow = length(model_n)))
  colnames(df_metrics) = c("Model", "RMSE")
  
  for(i in 1:length(model_n))
  {
    set.seed(24)
    
    cat("i:",i,"model:",model_n[i], "\n")
    P = f_models[[i]](ts$TR, h=7)
    P[P < 0] = 0
    RMSE = round(mmetric(ts$Y,P,metric="RMSE"),1)
    df_metrics[i, 1] = f_model_n[i]
    df_metrics[i, 2] = RMSE
  }
  return(df_metrics)
}

#cat metrics forecast and ml models
model_f_ml = function(f_model_n, ml_model_n, type=1, NP=140, lags=1:7){
  

  
  ts = split(type=type, NP=NP, lags=lags)
  f_ts = split_ts(type=type,H=NP,K=7)
  
  
  n_ml_model = length(ml_model_n)
  n_f_model = length(f_model_n)
  n_total_model = n_ml_model + n_f_model
  
  df_metrics = data.frame(matrix(ncol = 2, nrow = n_total_model))
  colnames(df_metrics) = c("Model", "RMSE")
  
  ml_metrics = model_rminer(model = ml_model_n, ts=ts, NP=NP)
  f_metrics = model_f(ts=f_ts, model_n = f_model_n, h=NP)
  cat_metrics = rbind(ml_metrics, f_metrics)

  ordered_metrics = cat_metrics[order(cat_metrics$RMSE), ]
  return(ordered_metrics)
}



#pipeline G/R forecast and ml models
model_f_rg = function(f_model_n, ml_model_n, type=1,mode="incremental", Runs=20, K=7, Test=7, lags=1:7){
  
  if(type==1){
    TS=df$BUD
  } else {
    TS=df$STELLA
  }
  S=K
  L=length(TS) # 730
  W=(L-Test)-(Runs-1)*S 
  ev=vector(length = Runs)
  ev2=vector(length = Runs)
  

  DS=CasesSeries(TS,lags)
  W2=W-max(lags)
  
  n_ml_model = length(ml_model_n)
  n_f_model = length(f_model_n)
  n_total_model = n_ml_model + n_f_model + 1
  
  df_metrics = data.frame(matrix(ncol = 2, nrow = n_total_model))
  colnames(df_metrics) = c("Model", "MSE")
  
  
  
  
  for(i in 1:n_f_model)
  {
    set.seed(24)
    
    cat("i:",i,"model:",f_model_n[i], "\n")
    
    for(b in 1:Runs)  
    {
      
      H=holdout(TS,ratio=Test,mode=mode,iter=b,window=W,increment=S)   
      trinit=H$tr[1]
      dtr=ts(TS[H$tr],frequency=K)
      P=round(suppressWarnings(f_models[[i]](x = dtr, h = Test)),0)
      P[P < 0] = 0
      ev[b]=mmetric(y=TS[H$ts],x=P,metric="MSE")
      
      
      mgraph(TS[H$ts],P,graph="REG",Grid=10,col=c("black","blue","red"),leg=list(pos="topleft",leg=c("target",f_model_n[i])))
    }
    
    MSE = mean(ev)
    cat("MSE:", MSE, "\n")
    df_metrics[i, 1] = f_model_n[i]
    df_metrics[i, 2] = MSE
  }
  
  
  for(i in 1:n_ml_model)
  {
    set.seed(24)
    
    cat("i:",i,"model:",ml_model_n[i], "\n")
    
    for(b in 1:Runs)
    {
      
      H=holdout(DS$y,ratio=Test,mode=mode,iter=b,window=W2,increment=S)   
      P = round(suppressWarnings(ml_models[[i]](S = DS,x= DS[H$tr,], init = (length(H$tr)+1), NP=Test)),0)
      P[P < 0] = 0
      ev2[b]=mmetric(y=TS[H$ts],x=P,metric="MSE")
      
      
      mgraph(TS[H$ts],P,graph="REG",Grid=10,col=c("black","blue","red"),leg=list(pos="topleft",leg=c("target",ml_model_n[i])))
    }
    
    MSE = mean(ev2)
    cat("MSE:", MSE, "\n")
    df_metrics[(n_f_model + i), 1] = ml_model_n[i]
    df_metrics[(n_f_model + i), 2] = MSE
  }
  
  ev_weekly_naive = weekly_naive(type=type)
  df_metrics[n_total_model, 1] = "weekly_naive"
  df_metrics[n_total_model, 2] = ev_weekly_naive$med_ev
  
  df_metrics = df_metrics[order(df_metrics$MSE),]
  
  return(df_metrics)
}



select_split_model = function(model, type=1, NP=140, lags=1:7, K=7){
  
  f_model_n = c("HW", "auto.arima", "ets", "nnetar")
  
  if(type==1){
    TS=df$BUD
  } else {
    TS=df$STELLA
  }
  
  
  set.seed(24)
  
  #Check if is ML or Forecasting model
  if(model %in% f_model_n) {
    f_ts = split_ts(type=type,H=NP,K=7)
    P=round(suppressWarnings(f_models[[model]](x = f_ts$TR, h = NP)),0)
    P[P < 0] = 0
    ev = round(mmetric(f_ts$Y,P,metric="RMSE"),1)
    Pred = P
    
  }else{
    ts = split(type=type, NP=NP, lags=lags)
    P = round(suppressWarnings(ml_models[[model]](S = ts$S,x= ts$TR, init = (length(ts$TR)+1), NP=NP)), 0)
    P[P < 0] = 0
    ev = round(mmetric(ts$Y,P,metric="RMSE"),1)
    Pred = P
  }
  
  
  
  
  #print(Pred)
  
  # Print all the predictions
  mgraph(tail(TS, 140),Pred,graph="REG",Grid=10,col=c("black","blue","red"),leg=list(pos="topleft",leg=c("target",model)))
  
  
  return(list(ev = ev, Pred= Pred))
}

#A = select_split_model(model = "ets")


# Select the model to Run with G/R
select_model = function(model, type=1,mode="incremental", Runs=20, K=7, Test=7, lags=1:7){
  
  f_model_n = c("HW", "auto.arima", "ets", "nnetar")
  
  if(type==1){
    TS=df$BUD
  } else {
    TS=df$STELLA
  }
  S=K
  L=length(TS) # 730
  W=(L-Test)-(Runs-1)*S 
  ev=vector(length = Runs)
  ev2=vector(length = Runs)
  Pred = c()
  
  DS=CasesSeries(TS,lags)
  W2=W-max(lags)

  

  set.seed(24)
  
  #Check if is ML or Forecasting model
  if(model %in% f_model_n) {
    for(b in 1:Runs)  
    {
      
      H=holdout(TS,ratio=Test,mode=mode,iter=b,window=W,increment=S)   
      trinit=H$tr[1]
      dtr=ts(TS[H$tr],frequency=K)
      P=round(suppressWarnings(f_models[[model]](x = dtr, h = Test)),0)
      P[P < 0] = 0
      ev[b]=mmetric(y=TS[H$ts],x=P,metric="MSE")
      Pred = c(Pred, P)
    }
  }else{
    for(b in 1:Runs)
    {
      H=holdout(DS$y,ratio=Test,mode=mode,iter=b,window=W2,increment=S)   
      P = round(suppressWarnings(ml_models[[model]](S = DS,x= DS[H$tr,], init = (length(H$tr)+1), NP=Test)), 0)
      P[P < 0] = 0
      Pred = c(Pred, P)
      ev[b]=mmetric(y=TS[H$ts],x=P,metric="MSE")
    }
    
  }
  
  
  
  
  #print(Pred)
  #print(TS[H$ts])
  
  # Print all the predictions
  mgraph(tail(TS, 140),Pred,graph="REG",Grid=10,col=c("black","blue","red"),leg=list(pos="topleft",leg=c("target",model)))

  #print(b)
  return(list(ev = ev, Pred= Pred))
}



# Input for the interface
model = function(week=1, bud_model="ets", stella_model="pcr"){
  
  if(bud_model == "lstm"){
    bud_pred = P_lstm$V1
    stella_pred = P_lstm$V2
  } else {
    bud_pred = select_model(model=bud_model, type=1)$Pred
    stella_pred = select_model(model=stella_model, type=0)$Pred
  }

  week_end=tail(df$DIA_SEMANA, 140)
  week_end[week_end < 6] <- 0
  week_end[week_end > 5] <- 1
  
  start= 7*(week-1)+1
  end = week*7
  
  week_end = week_end[start:end]
  bud=bud_pred[start:end]
  stella=stella_pred[start:end]
  drink_input=c()
  week_input=c()
  
  for(i in 1:length(bud)){
    drink_input = c(drink_input, bud[i])
    drink_input = c(drink_input, stella[i])
  }
  
  drink_input[drink_input < 0] = 0
  
  
  #plot(bud_pred$ev, type = "o", xlab = "Week", ylab = "MSE")
  #plot(stella_pred$ev, type = "o", xlab = "Week", ylab = "MSE")
  
  return(list(drink_input=drink_input, week_end=week_end))
}

P_lstm$V1
