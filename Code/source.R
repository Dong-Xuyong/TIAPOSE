library(openxlsx)
library(rminer)

load_tr_ts_BUD = function(){
  df = read.xlsx(xlsxFile = "bebidas.xlsx", sheet=1, skipEmptyRows = FALSE,colNames = TRUE,detectDates = TRUE)
  S = df$BUD
  srange=diff(range(S))
  NPRED = 140
  
  lags = 7
  S=CasesSeries(S,c(1:lags))
  
  N=nrow(S)
  
  NTR=N-NPRED
  TR = 1:NTR
  TS =(NTR+1):N
  return(list(w=df, srange=srange, TR=S[TR,], TS=S[TS,]))
}

metrics=function(Y,P,srange,...)
{
  cat("MAE:",mmetric(Y,P,metric="MAE"),"\n")
  cat("RMSE:",mmetric(Y,P,metric="RMSE"),"\n")
  cat("RRSE:",mmetric(Y,P,metric="RRSE"),"\n")
  cat("NMAE:",mmetric(Y,P,metric="NMAE",val=srange),"\n")
}