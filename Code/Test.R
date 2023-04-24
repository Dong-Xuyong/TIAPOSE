library(openxlsx)
library(rminer)
library(forecast)
df = read.xlsx(xlsxFile = "bebidas.xlsx", sheet=1, skipEmptyRows = FALSE,colNames = TRUE,detectDates = TRUE)
TS = df$BUD
K=12
H=140
L=length(df$BUD)
LTR=L-H

TR = ts(TS[1:LTR],frequency=K)
plot(TR)
# target predictions:
Y=TS[(LTR+1):L]

# holt winters forecasting method:
print("model> HoltWinters")
HW=HoltWinters(TR)
print(HW)
plot(HW)
print("show holt winters forecasts:")
# forecasts, from 1 to H ahead:
F=forecast(HW,h=H)
print(F)
Pred=F$mean[1:H] # HolWinters format
mgraph(Y,Pred,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","HW pred.")))

cat("RMSE:",mmetric(Y,Pred,metric="RMSE"),"\n")


print("model> auto.arima")
AR=auto.arima(TR)
print(AR) # ARIMA(3,0,1)(2,1,0)[12] 
print("show ARIMA forecasts:")
# forecasts, from 1 to H ahead:
F1=forecast(AR,h=H)
print(F1)
Pred1=F1$mean[1:H]
mgraph(Y,Pred1,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target"," ARIMA pred.")))
cat("RMSE:",mmetric(Y,Pred1,metric="RMSE"),"\n")

print("model> nnetar")
NN1=nnetar(TR,P=1,repeats=3)
print(NN1)
F3=forecast(NN1,h=H)
Pred3=F3$mean[1:H] # HolWinters format
mgraph(Y,Pred3,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","NN1 pred.")))
cat("RMSE:",mmetric(Y,Pred3,metric="RMSE"),"\n")

ETS=ets(TR)
F4=forecast(ETS,h=H)
Pred4=F4$mean[1:H] # HolWinters format
mgraph(Y,Pred4,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","ets pred.")))
cat("RMSE:",mmetric(Y,Pred4,metric="RMSE"),"\n")


