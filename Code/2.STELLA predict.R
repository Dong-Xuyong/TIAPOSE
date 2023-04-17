source("Split.R")

ts = split_STELLA()

model=c("naive","ctree","cv.glmnet","rpart","kknn","ksvm","mlp","mlpe",
        "randomForest","xgboost", "cubist", "lm", "mr", "mars", "pcr", "plsr", "cppls", "rvm")




# Base Predictions

for(i in 1:length(model))
{
  set.seed(24)
  cat("i:",i,"model:",model[i], "\n")
  search=list(search=mparheuristic(model[i]))
  M=fit(y~.,data=ts$train_set,model=model[i],search=search,fdebug=TRUE)
  P=predict(M,ts$test_set)
  cat("predicted MSE:",round(mmetric(ts$test_set$y,P,metric="MSE"),1),"\n")
#  cat("predicted NMAE:",round(mmetric(ts$test_set$y,P,metric="NMAE"),1),"\n")
#  cat("predicted RMSE:",round(mmetric(ts$test_set$y,P,metric="RMSE"),1),"\n")
#  cat("predicted RRSE:",round(mmetric(ts$test_set$y,P,metric="RRSE"),1),"\n")
}

#Grid Search

# xgboost
set.seed(24)

M5=fit(y~.,ts$train_set,model="xgboost",nrounds=12)
P5=predict(M5,ts$test_set)
cat(mmetric(ts$test_set$y,P5,"MSE"))

# hyperparamters eta, max_depth, min_child_weight


# ksvm 
s=list(smethod="grid",search=list(sigma=2^c(-15,-5,3),C=2^c(-5,0,15)),
       metric="MSE",method=c("kfold",3,24))
#print(s
set.seed(24)
M=fit(y~.,ts$train_set,model="ksvm",search=s,fdebug=TRUE)
#print(M@mpar)
P=predict(M, ts$test_set)
#mgraph(ts$test_set$y ,P,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","pred.")))
print(mmetric(ts$test_set$y ,P,"MSE"))



# random Forest
set.seed(24)
s=list(smethod="grid",search=list(mtry=c(3,5,7),ntree=c(500,700,1000)),
       convex=0,metric="MSE",method=c("kfold",3,24))
#print(s)
M=fit(y~.,ts$train_set,model="randomForest",search=s,fdebug=TRUE)

P=predict(M, ts$test_set)
#mgraph(ts$test_set$y ,P,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","pred.")))
print(mmetric(ts$test_set$y ,P,"MSE"))
#print(M@mpar)

# rpart
set.seed(24)
s=list(search=mparheuristic("rpart",n=40,lower=0.01,upper=0.9),method=c("kfold",3,24))
M=fit(y~.,ts$train_set,model="rpart",search=s,fdebug=TRUE)
P=predict(M, ts$test_set)
#mgraph(ts$test_set$y ,P,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","pred.")))
print(mmetric(ts$test_set$y ,P,"MSE"))
#print(M@mpar)

# ctree
set.seed(24)
mint=c("kfold",3,24)
s=list(search=mparheuristic("ctree",n=32,lower=0.1,upper=9),method=mint)
M=fit(y~.,ts$train_set,model="ctree",search=s,fdebug=TRUE)
P=predict(M, ts$test_set)
#print(M@mpar)
#mgraph(ts$test_set$y ,P,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","pred.")))
print(mmetric(ts$test_set$y ,P,"MSE"))



#MLPE
set.seed(24)
M=fit(y~.,ts$train_set,model="mlpe",size=4, decay=0.4,maxit=100,rang=0.9) 
P=predict(M, ts$test_set)
#print(M@mpar)
#mgraph(ts$test_set$y ,P,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","pred.")))
print(mmetric(ts$test_set$y ,P,"MSE"))








