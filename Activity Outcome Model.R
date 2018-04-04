load("modeldata.Rdata")
library(ISLR)
library(MASS)

summary(modeldata)

str(modeldata)
modeldata$activity_difficulty_id<-as.factor(modeldata$activity_difficulty_id)
modeldata$gametype_id<-as.factor(modeldata$gametype_id)
modeldata$cumulative.avg_cum_score<-as.numeric(modeldata$cumulative.avg_cum_score)

data<-modeldata

summary(data)

##Train and Test split

set.seed(1)
train<-sample(nrow(data), nrow(data)*(7/10))
test<-(1:nrow(data))[-train]
testdata=data[test,]
traindata=data[train,]

#Logistic Regression
glm.fit<-glm(activityplay_outcome~.-member_id-activity_play_id,family = binomial,data = data,subset = train)
summary(glm.fit)

glm.probs<-predict(glm.fit,type = "response")

contrasts(data$activityplay_outcome)

## Training
glm.pred=rep("cancel",188666)
glm.pred[glm.probs>0.5]="succeed"

table(glm.pred,traindata$activityplay_outcome)
mean(glm.pred!=traindata$activityplay_outcome)
mean(glm.pred==traindata$activityplay_outcome)
### Classification Accuracy: 80.97%
### True Positive Rate: 92.96%

## Testing 
glm.probs.test<-predict(glm.fit,newdata = testdata,type = "response")
glm.predict<-predict(glm.fit,newdata = testdata)
glm.pred.test=rep("cancel",80858)
glm.pred.test[glm.probs.test>0.5]="succeed"

table(glm.pred.test,testdata$activityplay_outcome)
mean(glm.pred.test!=testdata$activityplay_outcome)
mean(glm.pred.test==testdata$activityplay_outcome)

### Classification Accuracy: 80.63%
### True Positive Rate: 92.64%


#LASSO
library(glmnet)

mtx=model.matrix(activityplay_outcome~.-member_id-activity_play_id,data = data)

model.lasso=glmnet(mtx[train,],traindata$activityplay_outcome,alpha=1,family="binomial")

summary(model.lasso)

set.seed(1)
cv.out=cv.glmnet(mtx[train,],traindata$activityplay_outcome,alpha=1,nfolds=5,family="binomial",type.measure = "class")
best.lambda=cv.out$lambda.min
best.lambda #0.0002169162

par(mfrow=c(1,1))
plot(cv.out)

lasso.coef=predict(model.lasso,s=best.lambda,type="coefficients")
lasso.coef.data=as.data.frame(as.matrix(lasso.coef))
colnames(lasso.coef.data)[1]="Coef_est"
lasso.coef.data$Coefficient=row.names(lasso.coef.data)
row.names(lasso.coef.data)=1:nrow(lasso.coef.data)
lasso.coef.data=lasso.coef.data[c(2,1)]

rm<-which(lasso.coef.data$Coef_est==0)
lasso.coef.result<-lasso.coef.data[-rm,]

save(lasso.coef.result,file="lasso.coef2.RData")
write.csv(lasso.coef.result,file="lasso.coef2.csv")
###Fail to reduce dimension


##class (default threshold=0.5)
lasso.pred=predict(model.lasso,s=best.lambda,newx = mtx[test,],type = "class")
lasso.train=predict(model.lasso,s=best.lambda,newx = mtx[train,],type = "class")

##probability
lasso.pred.prob=predict(model.lasso,s=best.lambda,newx = mtx[test,],type = "response")
lasso.train.prob=predict(model.lasso,s=best.lambda,newx = mtx[train,],type = "response")


lasso.pred.test=rep("cancel",80858)
lasso.pred.test[lasso.pred.prob>0.5]="succeed"

table(lasso.pred.test,testdata$activityplay_outcome)
mean(lasso.pred.test==testdata$activityplay_outcome) 

## Classification Accuracy: 80.5944%
## True Positive Rate: 92.6986%

str(data)
#Random Forest
library(randomForest)
set.seed(1)
RF<-randomForest(Churn~.-Record,data = modeldata,subset = train,
                 ntree=100,mtry=7, importance=TRUE)
RF
RF_pred<-predict(RF,newdata = testdata)
table(RF_pred,testdata$Churn)

## Classification Accuracy: 83.6095%
## True Positive Rate: 92.8594%

importance(RF)
var_imp<-as.data.frame(importance(RF))
varImpPlot(RF)

write.csv(var_imp,file = "var_imp.csv")
