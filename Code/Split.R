library(openxlsx)
library(rminer)
library(forecast)
df = read.xlsx(xlsxFile = "bebidas.xlsx", sheet=1, skipEmptyRows = FALSE,colNames = TRUE,detectDates = TRUE)



# Falta os splits de Growing Window e Rolling window
split_ts = function(type=1, H=140, K=7){
  
  if(type==1){
    TS=df$BUD
  } else {
    TS=df$STELLA
  }
  
  L=length(TS)
  LTR=L - H
  
  TR = ts(TS[1:LTR],frequency=K)

  Y=TS[(LTR+1):L]
  
  return(TR=TR, Y=Y)
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

model_rminer = function(model, ts){
  
  df_metrics = data.frame(matrix(ncol = 2, nrow = length(model)))
  colnames(df_metrics) = c("Model", "RMSE")
  NP = 140
  for(i in 1:length(model))
  {
    set.seed(24)
    
    cat("i:",i,"model:",model[i], "\n")
    search=list(search=mparheuristic(model[i]))
    M=fit(y~.,data=ts$TR, model=model[i],search=search,fdebug=TRUE)
    init = length(ts$S) -NP + 1
    P=lforecast(M,ts$S,init, NP)
    
    
    RMSE = round(mmetric(ts$TS$y,P,metric="RMSE"),1)
    df_metrics[i, 1] = model[i]
    df_metrics[i, 2] = RMSE
  }
  
  return(df_metrics)
}
