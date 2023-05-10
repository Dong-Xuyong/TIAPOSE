library(openxlsx)
library(rminer)
library(forecast)
library(vars)

df = read.xlsx(xlsxFile = "bebidas.xlsx", sheet=1, skipEmptyRows = FALSE,colNames = TRUE,detectDates = TRUE)


#Split the data

K=12
LTS=K*20
stella = df$STELLA
bud = df$BUD
temp_max = df$TEMP_MAX
precip = df$PRECIPITACAO

H = holdout(stella, ratio=LTS, mode="order")
cdata = cbind(stella, bud, temp_max, precip)
mtr=ts(cdata[H$tr,], frequency = K)
Y=cdata[H$ts,][,1:2]



# Hyperparameters
LAGMAX = 4*K
vselect=VARselect(mtr, lag.max = LAGMAX, type="const")
omin=as.numeric(vselect$selection[3])
omax=as.numeric(vselect$selection[1])
stop=FALSE
pvalueref=0.1
o=omin

#Fit VAR model
while(!stop){
  mvar=VAR(mtr, p=o, type="const")
  st=serial.test(mvar,lags.pt=LAGMAX,type="PT.asymptotic")
  pvalue=as.numeric(st$serial$p.value)
  cat("order:",o,"pvalue:",pvalue,"\n")
  if(pvalue>pvalueref) stop=TRUE
  else if((o+1)==omax) {stop=TRUE;o=o+1}
  else o=o+1
}

mvar=VAR(mtr, p=o, type="const")


# Predict
X_ts = cbind(temp_max[H$ts], precip[H$ts])
F1 = forecast(mvar, h=LTS, xreg=X_ts)
Pred1 = as.numeric(F1$forecast$stella$mean)
Pred2 = as.numeric(F1$forecast$bud$mean)



A = mmetric(y=Y[,1],x=Pred1,metric="RMSE")
B = mmetric(y=Y[,2],x=Pred2,metric="RMSE")

cat("RMSE STELLA: ", A, "\n")
cat("RMSE Bud: ", B, "\n")

# Autoarima
marimax1=auto.arima(mtr[,1],xreg=mtr[,2:4])
marimax2=auto.arima(mtr[,2],xreg=mtr[,c(1,3,4)])
?auto.arima

marima1=auto.arima(mtr[,1]) # 
marima2=auto.arima(mtr[,2]) # 
xreg1=as.numeric(forecast(marima1,h=LTS)$mean)
xreg2=as.numeric(forecast(marima2,h=LTS)$mean)


F21=forecast(marimax1,h=LTS,xreg=xreg2)
F22=forecast(marimax2,h=LTS,xreg=xreg1)

Pred21=as.numeric(F21$mean)
Pred22=as.numeric(F22$mean)

fshow(Y,Pred21,Pred22,"ARIMAX","prod","rw")
mpause()


D1=CasesSeries(mtr[,1],c(1,2,3,4))
D2=CasesSeries(mtr[,2],c(1,2,3,4))


D1F=cbind(xlag1=D2$lag1,D1)
D2F=cbind(xlag1=D1$lag1,D2)


N1=fit(y~.,D1F,model="mlpe")
N2=fit(y~.,D2F,model="mlpe")

# get iterative predictions:
xex1=D1F[nrow(D1F),] # last example
xex2=D2F[nrow(D2F),] # last example

Pred31=vector(length=LTS)
Pred32=vector(length=LTS)
# note: the following code performs a multi-step ahead forecasting 
# but is not very general, it only works for the adopted: xreg lag1 + c(1,2,3,4) input time lags
for(i in 1:LTS)
{ 
  p1=predict(N1,xex1)
  p2=predict(N2,xex2)
  Pred31[i]=p1
  Pred32[i]=p2
  if(i<LTS) # update xexamples
  {
    xex1[1]=p2 # external var
    # slide xexample
    xex1[2: (length(xex1)-2) ] = xex1[3:(length(xex1)-1) ] # 4 lags + y
    xex1[length(xex1)-1]=p1 # prediction
    
    xex2[1]=p1 # external var
    xex2[2: (length(xex2)-2) ] = xex2[3:(length(xex2)-1) ] # 4 lags + y
    xex2[length(xex2)-1]=p2 # prediction
  }
}

fshow(Y,Pred31,Pred32,"2MLP","prod","rw")
