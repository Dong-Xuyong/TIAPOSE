source("Split.R")

ts = split_BUD()

# AUTOML


d=ts$train_set
names(d)[ncol(d)]="y" # change output name
inputs=ncol(d)-1
metric="MSE"

# consult the help of mparheuristic for more automl and ensemble examples:
#
# automatic machine learining (automl) with 5 distinct models and "SE" ensemble.
# the single models are tuned with 10 internal hyperparameter searches, 
# except ksvm that uses 13 searches via "UD".
# fit performs an internal validation 
sm=mparheuristic(model="automl3",n=NA, task="regr", inputs= inputs)
method=c("kfold",3,24)
search=list(search=sm,smethod="auto",method=method,metric=metric,convex=0)
M=fit(y~.,data=d,model="AE",search=search,fdebug=TRUE)
P=predict(M,d)
# show leaderboard:
cat("> leaderboard models:",M@mpar$LB$model,"\n")
cat(">  validation values:",round(M@mpar$LB$eval,4),"\n")
cat("best model is:",M@model,"\n")
cat(metric,"=",round(mmetric(d$y,P,metric=metric),2),"\n")















s=list(smethod="grid",search=mparheuristic("mlpe",n=5),convex=0,metric="REG",
       method=c("holdout",2/3,24))
#print(s)
M=fit(y~.,ts$train_set,model="mlpe",search=s,fdebug=TRUE)
P=predict(M, ts$test_set)
#print(M@mpar)
#mgraph(ts$test_set$y ,P,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","pred.")))
print(mmetric(ts$test_set$y ,P,"MSE"))


s$smethod="2L";s$convex=0;s$search=list(size=c(4,8,12))
#print(s)
M=fit(y~.,ts$train_set,model="mlpe",search=s,fdebug=TRUE)
P=predict(M, ts$test_set)
#print(M@mpar)
#mgraph(ts$test_set$y ,P,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","pred.")))
print(mmetric(ts$test_set$y ,P,"MSE"))
