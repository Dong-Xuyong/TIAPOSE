library(openxlsx)
library(rminer)
library(forecast)
df = read.xlsx(xlsxFile = "bebidas.xlsx", sheet=1, skipEmptyRows = FALSE,colNames = TRUE,detectDates = TRUE)


# Falta os splits de Growing Window e Rolling window

split_BUD_ts = function(){
  NPRED = 140
  K = 52
  bud = df$BUD
  H=holdout(bud,ratio=NPRED,mode="order")
  
  train_set = ts(bud[H$tr],frequency=K,start=c(2019,1,2), end=c(2020,8,13))
  test_set = 
  return(list(train_set = ))
}



split_BUD = function(){
  NPRED = 140
  lags = 7
  
  S=CasesSeries(df$BUD,c(1:lags))
  srange=diff(range(S))
  
  
  H=holdout(S$y,ratio=NPRED,mode="order")
  
  
  return(list(train_set = S[H$tr,], test_set = S[H$ts,]))
}

split_STELLA = function(){
  NPRED = 140
  lags = 7
  
  S=CasesSeries(df$STELLA,c(1:lags))
  srange=diff(range(S))
  
  
  H=holdout(S$y,ratio=NPRED,mode="order")
  
  train_set = S[H$tr,]
  test_set = S[H$ts,]
  
  return(list(train_set = S[H$tr,], test_set = S[H$ts,]))
}