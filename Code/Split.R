library(openxlsx)
library(rminer)
library(forecast)
library(stats)

df = read.xlsx(xlsxFile = "bebidas.xlsx", sheet=1, skipEmptyRows = FALSE,colNames = TRUE,detectDates = TRUE)


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

  ##mgraph(ev,mse$ev,graph="REG",Grid=10,col=c("black","blue","red"),leg=list(pos="topleft",leg=c("weekly Naive","HW pred.","mlpe")))
  #lines(mse$ev2,pch=19,cex=0.5,type="b",col="red")
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


#get RMSE from forecasting models
model_f = function(ts, model_n,h){
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
weekly_naive_model_f_rg = function(type=1,mode="incremental", Runs=20, K=7, Test=7){
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
  
  timelags=1:7 
  DS=CasesSeries(TS,timelags)
  W2=W-max(timelags)
  
  for(b in 1:Runs)  
  {
    
    H=holdout(TS,ratio=Test,mode=mode,iter=b,window=W,increment=S)   
    trinit=H$tr[1]
    dtr=ts(TS[H$tr],frequency=K)
    Pred=suppressWarnings(f_models$HW(x = dtr, h = Test))
    ev[b]=mmetric(y=TS[H$ts],x=Pred,metric="MSE")
    
    
    H2=holdout(DS$y,ratio=Test,mode=mode,iter=b,window=W2,increment=S)   
    Pred2 =ml_models$mlpe(S = DS,x= DS[H2$tr,], init = (length(H2$tr)+1), NP=Test)
    ev2[b]=mmetric(y=TS[H$ts],x=Pred2,metric="MSE")
    
    #cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
    #    "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
    #    "nmae:",ev[b],",",ev2[b],"\n")
    #mgraph(TS[H$ts],Pred,graph="REG",Grid=10,col=c("black","blue","red"),leg=list(pos="topleft",leg=c("target","HW pred.","mlpe")))
    lines(Pred2,pch=19,cex=0.5,type="b",col="red")
  }
  
  return(list(ev=ev, ev2=ev2))
}



model_f_rg = function(f_model_n, ml_model_n, type=1,mode="incremental", Runs=20, K=7, Test=7, timelags=1:7){
  
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
  

  DS=CasesSeries(TS,timelags)
  W2=W-max(timelags)
  
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
      P=suppressWarnings(f_models[[i]](x = dtr, h = Test))
      ev[b]=mmetric(y=TS[H$ts],x=P,metric="MSE")
      
      
      #mgraph(TS[H$ts],P,graph="REG",Grid=10,col=c("black","blue","red"),leg=list(pos="topleft",leg=c("target",f_model_n[i])))
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
      P = suppressWarnings(ml_models[[i]](S = DS,x= DS[H$tr,], init = (length(H$tr)+1), NP=Test))
      ev2[b]=mmetric(y=TS[H$ts],x=P,metric="MSE")
      
      
      #mgraph(TS[H$ts],P,graph="REG",Grid=10,col=c("black","blue","red"),leg=list(pos="topleft",leg=c("target",ml_model_n[i])))
    }
    
    MSE = mean(ev2)
    cat("MSE:", MSE, "\n")
    df_metrics[(n_f_model + i), 1] = ml_model_n[i]
    df_metrics[(n_f_model + i), 2] = MSE
  }
  
  ev_weekly_naive = weekly_naive(type=type)
  df_metrics[n_total_model, 1] = "weekly_naive"
  df_metrics[n_total_model, 2] = ev_weekly_naive$med_ev
  
  return(df_metrics)
}


best_model = function(model, type=1,mode="incremental", Runs=20, K=7, Test=7, timelags=1:7){
  
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
  
  DS=CasesSeries(TS,timelags)
  W2=W-max(timelags)

  

  set.seed(24)
  
  #Check if is ML or Forecasting model
  if(model %in% f_model_n) {
    for(b in 1:Runs)  
    {
      
      H=holdout(TS,ratio=Test,mode=mode,iter=b,window=W,increment=S)   
      trinit=H$tr[1]
      dtr=ts(TS[H$tr],frequency=K)
      P=round(suppressWarnings(f_models[[model]](x = dtr, h = Test)),0)
      ev[b]=mmetric(y=TS[H$ts],x=P,metric="MSE")
      Pred = c(Pred, P)
    }
  }else{
    for(b in 1:Runs)
    {
      H=holdout(DS$y,ratio=Test,mode=mode,iter=b,window=W2,increment=S)   
      P = round(suppressWarnings(ml_models[[model]](S = DS,x= DS[H$tr,], init = (length(H$tr)+1), NP=Test)), 0)
      
      Pred = c(Pred, P)
      ev[b]=mmetric(y=TS[H$ts],x=P,metric="MSE")
    }
    
  }
  
  
  
  
  print(Pred)
  print(TS[H$ts])
  
  # Print all the predictions
  #mgraph(tail(TS, 140),Pred,graph="REG",Grid=10,col=c("black","blue","red"),leg=list(pos="topleft",leg=c("target",model)))

  print(b)
  return(list(ev = ev, Pred= Pred))
}


model = function(week=1, bud_model="ets", stella_model="pcr"){
  bud_pred = best_model(model=bud_model, type=1)
  stella_pred = best_model(model=stella_model, type=0)
  week_end=tail(df$DIA_SEMANA, 140)
  week_end[week_end < 6] <- 0
  week_end[week_end > 5] <- 1
  
  start= (7*(week-1)+1)
  end = week*7
  
  week_end = week_end[start:end]
  bud=bud_pred$Pred[start:end]
  stella=stella_pred$Pred[start:end]
  drink_input=c()
  week_input=c()
  
  for(i in 1:length(bud)){
    drink_input = c(drink_input, bud[i])
    drink_input = c(drink_input, stella[i])
  }
  
  drink_input[drink_input < 0] = 0
  
  
  ##plot(bud_pred$ev, type = "o", xlab = "Week", ylab = "MSE")
  ##plot(stella_pred$ev, type = "o", xlab = "Week", ylab = "MSE")
  
  return(list(drink_input=drink_input, week_end=week_end))
}

#input = model(week = 2, bud_model="ets", stella_model="pcr")

#input

#modelos = c("naive","ctree","cv.glmnet","rpart","kknn","ksvm","mlp","mlpe", "randomForest",
            #"xgboost", "cubist", "lm", "mr", "mars", "pcr", "plsr", "cppls", "rvm", "HW", "auto.arima", "ets", "nnetar")

#input = model(week = 2, bud_model="ets", stella_model="pcr")

