library(openxlsx)
library(rminer)

df = read.xlsx(xlsxFile = "bebidas.xlsx", sheet=1, skipEmptyRows = FALSE,colNames = TRUE,detectDates = TRUE)

class(df[,1])
summary(df)


S = df$BUD


NPRED = 14

lags = 7
DS=CasesSeries(S,c(1:lags))
srange=diff(range(S))

N=nrow(DS) # number of D examples
NTR=N-NPRED
TR=1:NTR # training row elements of D (oldest elements), excluding last NPRED rows
TS=(NTR+1):N

# fit a random forest
mpause("fit a random forest (randomForest):")
RF=fit(y~.,DS[TR,],model="randomForest",search="heuristic")

PRF=predict(RF,DS[TS,])

Y=DS[TS,]$y # real observed values

cat("RF predictions:\n")
print(PRF)
cat("MAE:",mmetric(Y,PRF,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,PRF,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,PRF,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,PRF,metric="RRSE"),"\n")


