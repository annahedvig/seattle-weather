####################################################################################################
# Data science class project: Predict seattle weather
# V.A. Suchar
# 03/27/2018
####################################################################################################

#---------------------------------------------------------------------------------------------------
# 1. Data and packages
#---------------------------------------------------------------------------------------------------
# you will have to install the packages first!
library(nnet)
library(pROC)
library(class)
library(e1071)
library(rpart)
library(rattle)
library(randomForest)
library(caret)
library(neuralnet)
library(nnet)

rain=read.csv("C:/Users/vasiles/OneDrive/CS Data Science/seattleWeather_formated.csv") 
# change pathway to your data location!

# "1"- it rained "0"- didn't rain

#---------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------
# 2. Exploratory data analysis
#---------------------------------------------------------------------------------------------------

# NOTE: plots are saved as jpegs. if you just want to see it, mark out the jpeg() and dev.off() command lines
jpeg("C:/Users/vasiles/OneDrive/CS Data Science/rain_daily.jpeg", quality=100, height=960, width=960)
plot(seq(1, length(rain$DATE), 1), rain$PRCP, type="l", ylab="Precipitation (inches)", xlab="Daily values (1948-2017") # hard to tell what is going on
dev.off()

jpeg("C:/Users/vasiles/OneDrive/CS Data Science/rain_monthly.jpeg", quality=100, height=960, width=960)
plot(rain$MONTH, rain$PRCP, xlab="Month", ylab="Precipitation (inches)") # definetly we can see that there ia a monthly pattern - use month as a predictor
dev.off()

jpeg("C:/Users/vasiles/OneDrive/CS Data Science/rain_yearly.jpeg", quality=100, height=960, width=960)
plot(rain$YEAR, rain$PRCP) # all years are fairly similar, with few exceptionally rainy days
dev.off()

year_total=vector()
year=seq(1948, 2017, 1)
for (i in 1:length(year)){
  sub=rain[rain$YEAR==year[i],]
  year_total[i]=sum(na.omit(sub$PRCP))
}

jpeg("C:/Users/vasiles/OneDrive/CS Data Science/rain_totals_yearly.jpeg", quality=100, height=960, width=960)
plot(year, year_total, type="l", xlab="Year", ylab="Total precipitation (inches/year")
dev.off()

#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------------
# 3. FIND THE BEST MACHINE LEARNING MODEL
#---------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------
# Format data for ML
# Create the train and test datasets
#---------------------------------------------------------------------------------------------------
rain.ml=data.frame(cbind(rain$RAIN_today, rain$RAIN_yesterday, rain$PRCP_yesterday, rain$TMAX_yesterday, rain$TMIN_yesterday, rain$MONTH))
colnames(rain.ml)=c("rain_today", "rain_yesterday", "prcp_yesterday", "tmax_yesterday", "tmin_yesterday", "month")
rain.ml$rain_today=as.factor(rain.ml$rain_today)
rain.ml$rain_yesterday=as.factor(rain.ml$rain_yesterday)
rain.ml$month=as.factor(rain.ml$month)

set.seed(1234)
train.index=sample(c(1:nrow(rain.ml)), 0.6*nrow(rain.ml))
data.train=rain.ml[train.index,]
data.test=rain.ml[-train.index,]
x.test=data.test[,-1]
y.test=data.test[,1]

#---------------------------------------------------------------------------------------------------
# Logistic classification
#---------------------------------------------------------------------------------------------------
# Run test
multinom=multinom(rain_today ~.,data=data.train)
summary(multinom)

#Predict Output
predicted.multinom= predict(multinom,x.test)

# AUC and ROC
op.multinom=data.frame(cbind(y.test, predicted.multinom))
colnames(op.multinom)=c("obs", "pred")

roc.multinom <- multiclass.roc(op.multinom$obs, op.multinom$pred, percent=TRUE)
auc(roc.multinom)

# Plot the AUC
jpeg("C:/Users/vasiles/OneDrive/CS Data Science/logistic_AUC.jpeg", quality=100, height=960, width=960)
plot.roc(roc.multinom[['rocs']][[1]], main=paste0("Logistic regression AUC = ", round(auc(roc.multinom), 2), "%"),lwd=3)
dev.off()


op.multinom.summary=matrix(NA, ncol=2,nrow=2)
colnames(op.multinom.summary)=c("1-rain", "2-no rain")

#Calculate and plot the  observed vs predicted
for (i in 1:2){
  sub.obs=op.multinom[op.multinom$obs==i,]
  for (j in 1:2){
    op.multinom.summary[j,i]=nrow(sub.obs[sub.obs$pred==j,])
  }
}

grays=c(gray(0.1), gray(0.7))
jpeg("C:/Users/vasiles/OneDrive/CS Data Science/Logistic_obs_vs_pred.jpeg", quality=100, height=960, width=960)
barplot(op.multinom.summary, col=grays, ylim=c(0,7000), xlab="Rain", ylab="Observed", main="Logistic Regression")
legend("topright", title="Predicted", c( "1-rain", "2-no rain"), col=grays, lty=1, lwd=3, box.lty=0)
dev.off()

#---------------------------------------------------------------------------------------------------
# Naive Bayes
#---------------------------------------------------------------------------------------------------
nb=naiveBayes(rain_today~.,data=data.train)
summary(nb)

#Predict Output
predicted.nb= predict(nb,x.test)

op.nb=data.frame(cbind(y.test, predicted.nb))
colnames(op.nb)=c("obs", "pred")

roc.nb <- multiclass.roc(op.nb$obs, op.nb$pred, percent=TRUE)
auc(roc.nb)

jpeg("C:/Users/vasiles/OneDrive/CS Data Science/NB_AUC.jpeg", quality=100, height=960, width=960)
plot.roc(roc.nb[['rocs']][[1]], main=paste0("Naive Bayes AUC = ", round(auc(roc.nb), 2), "%"),lwd=3)
dev.off()

op.multinom.summary=matrix(NA, ncol=2,nrow=2)
colnames(op.multinom.summary)=c("1-rain", "2-no rain")


# Observed vs predicted
for (i in 1:2){
  sub.obs=op.multinom[op.multinom$obs==i,]
  for (j in 1:2){
    op.multinom.summary[j,i]=nrow(sub.obs[sub.obs$pred==j,])
  }
}

grays=c(gray(0.1), gray(0.7))
jpeg("C:/Users/vasiles/OneDrive/CS Data Science/NB_obs_vs_pred.jpeg", quality=100, height=960, width=960)
barplot(op.multinom.summary, col=grays, ylim=c(0,7000), xlab="Rain", ylab="Observed", main="Naive Bayes")
legend("topright", title="Predicted", c( "1-rain", "2-no rain"), col=grays, lty=1, lwd=3, box.lty=0)
dev.off()

#---------------------------------------------------------------------------------------------------
#Decision tree
#---------------------------------------------------------------------------------------------------
# grow tree 
dt <- rpart(rain_today~.,data=data.train, method="class")
predicted.dt= predict(dt,x.test, type="vector")

fancyRpartPlot(dt, palettes="Greys")
# Full-grown tree with 8 splits using 6 different variables 
# (Not running the line below - do it to see the tree)
# fancyRpartPlot(rt)

# you can prune thre tree if needed:
# printcp(dt)
# Get the optimal CP programmatically...
# min.xerror <- dt$cptable[which.min(dt$cptable[,"xerror"]),"CP"]
# min.xerror
# ...and use it to prune the tree
# dt.pruned <- prune(dt,cp = min.xerror) 
# Plot the pruned tree
# fancyRpartPlot(dt.pruned)


op.dt=data.frame(cbind(y.test, predicted.dt))
colnames(op.dt)=c("obs", "pred")

roc.dt <- multiclass.roc(op.dt$obs, op.dt$pred, percent=TRUE)
auc(roc.dt)

jpeg("C:/Users/vasiles/OneDrive/CS Data Science/decision_tree_AUC.jpeg", quality=100, height=960, width=960)
plot.roc(roc.dt[['rocs']][[1]], main=paste0("Decision Tree AUC = ", round(auc(roc.dt), 2), "%"),lwd=3)
dev.off()

op.multinom.summary=matrix(NA, ncol=2,nrow=2)
colnames(op.multinom.summary)=c("1-rain", "2-no rain")


# Observed vs predicted
for (i in 1:2){
  sub.obs=op.multinom[op.multinom$obs==i,]
  for (j in 1:2){
    op.multinom.summary[j,i]=nrow(sub.obs[sub.obs$pred==j,])
  }
}

grays=c(gray(0.1), gray(0.7))
jpeg("C:/Users/vasiles/OneDrive/CS Data Science/decision_tree_obs_vs_pred.jpeg", quality=100, height=960, width=960)
barplot(op.multinom.summary, col=grays, ylim=c(0,7000), xlab="Rain", ylab="Observed", main="Decision Tree")
legend("topright", title="Predicted", c( "1-rain", "2-no rain"), col=grays, lty=1, lwd=3, box.lty=0)
dev.off()

#---------------------------------------------------------------------------------------------------
# Random Forest (decission tree and bagging are from the same family)
#---------------------------------------------------------------------------------------------------
data.train=na.exclude(data.train)

dt=randomForest(rain_today~.,data=data.train, ntree=1000)
#varImpPlot(dt)
var.importance=as.data.frame(sort(importance(dt)[,1],decreasing = TRUE),optional = T)

#Predict Output
predicted.dt= predict(dt,x.test)

op.dt=data.frame(cbind(y.test[-1], predicted.dt[-1]))
colnames(op.dt)=c( "obs", "pred")

roc.dt <- multiclass.roc(op.dt$obs, op.dt$pred, percent=TRUE)
auc(roc.dt)

jpeg("C:/Users/vasiles/OneDrive/CS Data Science/RF_AUC.jpeg", quality=100, height=960, width=960)
plot.roc(roc.dt[['rocs']][[1]], main=paste0("Random Forest AUC = ", round(auc(roc.dt), 2), "%"),lwd=3)
dev.off()

op.multinom.summary=matrix(NA, ncol=2,nrow=2)
colnames(op.multinom.summary)=c("1-rain", "2-no rain")


# Observed vs predicted
for (i in 1:2){
  sub.obs=op.multinom[op.multinom$obs==i,]
  for (j in 1:2){
    op.multinom.summary[j,i]=nrow(sub.obs[sub.obs$pred==j,])
  }
}

grays=c(gray(0.1), gray(0.7))
jpeg("C:/Users/vasiles/OneDrive/CS Data Science/RF_obs_vs_pred.jpeg", quality=100, height=960, width=960)
barplot(op.multinom.summary, col=grays, ylim=c(0,7000), xlab="Rain", ylab="Observed", main="Random Forest")
legend("topright", title="Predicted", c( "1-rain", "2-no rain"), col=grays, lty=1, lwd=3, box.lty=0)
dev.off()
#---------------------------------------------------------------------------------------------------
# SVM
#---------------------------------------------------------------------------------------------------

svm=svm(rain_today~.,data=data.train, kernel="sigmoid")
#summary(svm)


#Predict Output
predicted.svm= predict(svm,x.test)

op.svm=data.frame(cbind(y.test, predicted.svm))
colnames(op.svm)=c("obs", "pred")

roc.svm <- multiclass.roc(op.svm$obs, op.svm$pred, percent=TRUE)
auc(roc.svm)

jpeg("C:/Users/vasiles/OneDrive/CS Data Science/SVM_AUC.jpeg", quality=100, height=960, width=960)
plot.roc(roc.svm[['rocs']][[1]], main=paste0("SVM AUC = ", round(auc(roc.svm), 2), "%"),lwd=3)
dev.off()

op.multinom.summary=matrix(NA, ncol=2,nrow=2)
colnames(op.multinom.summary)=c("1-rain", "2-no rain")


# Observed vs predicted
for (i in 1:2){
  sub.obs=op.multinom[op.multinom$obs==i,]
  for (j in 1:2){
    op.multinom.summary[j,i]=nrow(sub.obs[sub.obs$pred==j,])
  }
}

grays=c(gray(0.1), gray(0.7))
jpeg("C:/Users/vasiles/OneDrive/CS Data Science/SVM_obs_vs_pred.jpeg", quality=100, height=960, width=960)
barplot(op.multinom.summary, col=grays, ylim=c(0,7000), xlab="Rain", ylab="Observed", main="SVM")
legend("topright", title="Predicted", c( "1-rain", "2-no rain"), col=grays, lty=1, lwd=3, box.lty=0)
dev.off()

#---------------------------------------------------------------------------------------------------
# Gradient Boosting Classifier
#---------------------------------------------------------------------------------------------------

# Fitting model
fitControl <- trainControl( method = "repeatedcv", number = 10, repeats = 10)
gbm<- train(rain_today~., data =data.train , method = "gbm", trControl = fitControl,verbose = FALSE)

summary(gbm)


#Predict Output
predicted.gbm=predict(gbm,x.test,type= "raw")

op.gbm=data.frame(cbind(y.test, predicted.gbm))
colnames(op.gbm)=c("obs", "pred")

roc.gbm <- multiclass.roc(op.gbm$obs, op.gbm$pred, percent=TRUE)
auc(roc.gbm)

jpeg("C:/Users/vasiles/OneDrive/CS Data Science/GB_AUC.jpeg", quality=100, height=960, width=960)
plot.roc(roc.gbm[['rocs']][[1]], main=paste0("Gradient Boosting AUC = ", round(auc(roc.gbm), 2), "%"),lwd=3)
dev.off()

op.multinom.summary=matrix(NA, ncol=2,nrow=2)
colnames(op.multinom.summary)=c("1-rain", "2-no rain")


# Observed vs predicted
for (i in 1:2){
  sub.obs=op.multinom[op.multinom$obs==i,]
  for (j in 1:2){
    op.multinom.summary[j,i]=nrow(sub.obs[sub.obs$pred==j,])
  }
}

grays=c(gray(0.1), gray(0.7))
jpeg("C:/Users/vasiles/OneDrive/CS Data Science/GB_obs_vs_pred.jpeg", quality=100, height=960, width=960)
barplot(op.multinom.summary, col=grays, ylim=c(0,7000), xlab="Rain", ylab="Observed", main="Gradient Boosting")
legend("topright", title="Predicted", c( "1-rain", "2-no rain"), col=grays, lty=1, lwd=3, box.lty=0)
dev.off()


#---------------------------------------------------------------------------------------------------
# Neural Networks
#---------------------------------------------------------------------------------------------------
library(neuralnet)
library(nnet)


n <- names(data.train)
f <- as.formula(paste("rain_today~", paste(n[!n %in% "rain_today"], collapse = " + ")))
nn <- nnet(f,data=data.train, size = 15)

#Predict Output
predicted.nn = predict(nn,x.test, type="class")

op.nn=data.frame(cbind(y.test, as.numeric(predicted.nn)))
colnames(op.nn)=c("obs", "pred")



roc.nn <- multiclass.roc(op.nn$obs, op.nn$pred, percent=TRUE)
auc(roc.nn)

jpeg("C:/Users/vasiles/OneDrive/CS Data Science/NN_AUC.jpeg", quality=100, height=960, width=960)
plot.roc(roc.nn[['rocs']][[1]], main=paste0("Neural Networks AUC = ", round(auc(roc.nn), 2), "%"),lwd=3)
dev.off()

op.multinom.summary=matrix(NA, ncol=2,nrow=2)
colnames(op.multinom.summary)=c("1-rain", "2-no rain")


# Observed vs predicted
for (i in 1:2){
  sub.obs=op.multinom[op.multinom$obs==i,]
  for (j in 1:2){
    op.multinom.summary[j,i]=nrow(sub.obs[sub.obs$pred==j,])
  }
}

grays=c(gray(0.1), gray(0.7))
jpeg("C:/Users/vasiles/OneDrive/CS Data Science/NN_obs_vs_pred.jpeg", quality=100, height=960, width=960)
barplot(op.multinom.summary, col=grays, ylim=c(0,7000), xlab="Rain", ylab="Observed", main="Neural Networks")
legend("topright", title="Predicted", c( "1-rain", "2-no rain"), col=grays, lty=1, lwd=3, box.lty=0)
dev.off()

#---------------------------------------------------------------------------------------------------
# END
#---------------------------------------------------------------------------------------------------
