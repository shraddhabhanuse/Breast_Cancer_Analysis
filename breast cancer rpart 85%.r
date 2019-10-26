data1=read.csv("D:/Projects/On Going/KNN/wisc_bc_data.csv",sep = ",",header = TRUE)
data=data1[,-c(1)]
names(data)
data$diagnosis=factor(data$diagnosis,
                      levels = c("M","B"),
                      labels = c(1,0))
y=data$diagnosis
y=as.data.frame(y)
library(caTools)
set.seed(123)
sample=sample.split(data$diagnosis,SplitRatio = 0.80)
train_set=subset(data,sample==TRUE)
test_set=subset(data,sample==FALSE)

library(rpart)
model=rpart(diagnosis~.,train_set)
summary(model)

library(rattle)
library(rpart.plot)
fancyRpartPlot(model)

pred=predict(model,test_set,type = "class")

library(caret)
cm=confusionMatrix(as.factor(test_set$diagnosis),as.factor(pred))
cm