data1=read.csv("D:/Projects/Completed/1.8.Breast Cancer Analysis/wisc_bc_data.csv")

normalize<-function(x){
  return ((x-min(x)) / (max(x)-min(x)))
}

View(data1)

data=as.data.frame(lapply(data1[3:31], normalize))
View(data)

wbcd_train=data[1:469,]
wbcd_test=data[470:569,]

wbcd_train_labels=data1[1:469,2]
View(wbcd_train_labels)
wbcd_test_labels=data1[470:569,2]
View(wbcd_test_labels)
library(class)
wbcd_test_pred=knn(train = wbcd_train,test = wbcd_test,
                   cl=wbcd_train_labels,k=21)#21*21=469
View(wbcd_test_pred)

library(caret)
cm=confusionMatrix(as.factor(wbcd_test_labels),as.factor(wbcd_test_pred))
cm
library(gmodels)
CrossTable(x=wbcd_test_pred,y=wbcd_test_labels,chisq = FALSE)
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred,chisq = FALSE)
#there is 2 False Negative prediction and this is dangrous so to improve our model we will scale using different method and try different value of k

data=as.data.frame(scale(data1[3:31]))
View(data)

wbcd_train=data[1:469,]
wbcd_test=data[470:569,]

wbcd_train_labels=data1[1:469,2]
View(wbcd_train_labels)
wbcd_test_labels=data1[470:569,2]
View(wbcd_test_labels)
library(class)
wbcd_test_pred=knn(train = wbcd_train,test = wbcd_test,
                   cl=wbcd_train_labels,k=21)#21*21=469
View(wbcd_test_pred)

library(gmodels)
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred,chisq = FALSE)

#after rescaling Z szore standardisation we can see that accuracy is decrease by 3 % so we will not consider this scaling
