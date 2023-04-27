


model=c("naive","ctree","cv.glmnet","rpart","kknn","ksvm","mlp","mlpe",
        "randomForest","xgboost","cubist","lm","mr","mars","pcr","plsr","cppls","rvm")
# note: in this example, default values are considered for the hyperparameters.
# better results can be achieved by tuning hyperparameters via improved usage
# of the search argument (via mparheuristic function or written code)
data(iris)
ir2=iris[,1:4] # predict iris "Petal.Width"
names(ir2)[ncol(ir2)]="y" # change output name
inputs=ncol(ir2)-1
ho=holdout(ir2$y,2/3,seed=123) # 2/3 for training and 1/3 for testing
Y=ir2[ho$ts,ncol(ir2)]
for(i in 1:length(model))
{
  cat("i:",i,"model:",model[i],"\n")
  search=list(search=mparheuristic(model[i])) # rminer default values
  M=fit(y~.,data=ir2[ho$tr,],model=model[i],search=search,fdebug=TRUE)
  P=predict(M,ir2[ho$ts,])
  cat("predicted MAE:",round(mmetric(Y,P,metric="MAE"),1),"\n")
}