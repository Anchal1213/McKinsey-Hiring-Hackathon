setwd("C:/Users/user/Desktop/analytics vidhya/McKinsey")
train<-read.csv("train.csv",header=T)
test<-read.csv("test.csv",header=T)
test$Email_Status<-NA
total<-rbind(train,test)

summary(test)
library(ggplot2)
ggplot(total,aes(Customer_Location))+geom_bar()
levels(total$Customer_Location)[levels(total$Customer_Location) %in% c("","A","H")]<-"others"

summary(total$Total_Past_Communications)
boxplot(total$Total_Past_Communications)
table(total$Total_Past_Communications)
total$Total_Past_Communications[is.na(total$Total_Past_Communications)]<-median(total$Total_Past_Communications,na.rm=T)

summary(total$Total_Links)
table(total$Total_Links)
total$Total_Links[is.na(total$Total_Links)]<-mean(total$Total_Links,na.rm=T)

summary(total$Total_Images)
table(total$Total_Images)
total$Total_Images[is.na(total$Total_Images)]<-median(total$Total_Images,na.rm=T)

total$Email_Type<-as.factor(total$Email_Type)
total$Email_Source_Type<-as.factor(total$Email_Source_Type)
total$Email_Campaign_Type<-as.factor(total$Email_Campaign_Type)
total$Time_Email_sent_Category<-as.factor(total$Time_Email_sent_Category)
total$Word_Count<-log(total$Word_Count)

library(dummies)
names(total)
X_All<-dummy.data.frame(total, names=c("Customer_Location","Email_Type","Email_Source_Type","Email_Campaign_Type","Time_Email_Sent_Category"))
X_All$Email_ID<-NULL
X_All<-X_All[,-(6:12)]
X_train<-X_All[1:nrow(train),]
X_test<- X_All[-(1:nrow(train)),]

y=X_train$Email_Status
X_train$Email_Status<-NULL
X_test$Email_Status<-NULL

#xgb
library(xgboost)
model_xg<-xgb.cv(data=data.matrix(X_train),label = y,booster="gbtree",objective="multi:softmax",num_class=3,nrounds=100,eta=0.02,max_depth=8, nfold=5,subsample=0.6, colsample_bytree=0.85, min_child_weight=1, eval_metric="merror")
model_xg<-xgboost(data=data.matrix(X_train),label = y,booster="gbtree",objective="reg:linear",nrounds=350,eta=0.02,max_depth=8, importance=T,subsample=0.6, colsample_bytree=0.85, min_child_weight=1, eval_metric="auc")
names<-dimnames(data.matrix(X_train))[[2]]
abc<-xgb.importance(names,model=model_xg)
xgb.plot.importance(abc)
pred1<-predict(model_xg,data.matrix(X_test))
mydata=cbind("Email_id"=test$Email_ID,"Email_Status"=pred1)
write.csv(mydata,"new_submit.csv",row.names = F)

