#Library
library(caret)


# Split training and testing
set.seed(280)
split<-createDataPartition(y = advertising$sales,  p = 0.7, list = FALSE)

training <- advertising[split,]
summary(training)
testing <- advertising[-split,]
summary(testing)

# Training the model
lmFit<-train(sales ~., data = training, method = "lm")
summary(lmFit)

# Variable Importance
varImp(lmFit)
plot(varImp(lmFit))

# K-fold cross-validation
ctrl<-trainControl(method = "cv",number = 10)
lmCVFit<-train(sales ~ ., data = advertising, method = "lm", trControl = ctrl, metric="Rsquared")
summary(lmCVFit)

# Check residuals
residuals<-resid(lmFit)
predictedValues<-predict(lmFit)
plot(training$sales,residuals)
abline(0,0)
plot(training$sales,predictedValues)

# Evaluation
predictedVal<-predict(lmFit,testing)
modelvalues<-data.frame(obs = testing$sales, pred=predictedVal)
defaultSummary(modelvalues)

#RMSE  Rsquared       MAE 
#1.4312229 0.9299591 1.1601849 
