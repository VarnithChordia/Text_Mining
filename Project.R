library(tidyverse)
library(ggplot2)
library(e1071)
library(randomForest)
library(caret)
library(ggRandomForests)
library(missForest)
library(glmnet)
library(dplyr)
library(stringr)
library(corrplot)


train_data<-read.csv("E:/Rutgers/2nd-Sem/Machine_Learning_Project/train.csv")
str(train_data)


##Missing_value_Graph
apply(train_data,2,function(x)sum(is.na(x)))

x<-as.data.frame(apply(train_data,2,function(x)sum(is.na(x))))
colnames(x)[1]<-"Missing_Value"
x$variables<-rownames(x)
x<-x %>% filter(Missing_Value>0)

View(x)

ggplot(x,mapping=aes(variables,Missing_Value))+geom_col() + theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    axis.text.x = element_text(vjust = 0, 
        angle = 20))+labs(title = "Variable vs Missing Value")+coord_flip() + theme(plot.title = element_text(hjust = 0.5))


##Split Train-Test and Data Cleaning

train_data$GarageYrBlt<-as.numeric(as.character(train_data$GarageYrBlt))

train_data$GarageYrBlt[is.na(train_data$GarageYrBlt)]<-train_data$YearBuilt[is.na(train_data$GarageYrBlt)]


train_data$YrSold<-as.factor(train_data$YrSold)
train_data$MSSubClass<-as.factor(train_data$MSSubClass)
train_data$MoSold<-as.factor(train_data$MoSold)
train_data$OverallQual<-as.factor(train_data$OverallQual)
train_data$OverallCond<-as.factor(train_data$OverallCond)
train_data$YearBuilt<-as.factor(train_data$YearBuilt)
train_data$GarageYrBlt<-as.factor(train_data$GarageYrBlt)
train_data$YearRemodAdd<-as.factor(train_data$YearRemodAdd)




set.seed(42)
trainIndex <- createDataPartition(train_data$Id, p = 0.7, list = FALSE, times = 1)
train <-train_data[trainIndex,]
test  <- train_data[-trainIndex,]





##Removing Variables where more than 50% values are missing

train<-train %>% select(-MiscFeature,-Fence,-PoolQC,-Alley,-Id)

test<-test %>% select(-MiscFeature,-Fence,-PoolQC,-Alley,-Id)





##Missing Value treatment


w<-as.data.frame(apply(train,2,function(x)sum(is.na(x))))


colnames(w)[1]<-"Missing_Value"
w$Variables<-rownames(w)
w<-w %>% filter(Missing_Value!=0)
rownames(w)<-w$Variables
Missing_train<-train[colnames(train) %in% colnames(t(w))]
Present_train<-train[!colnames(train) %in% colnames(t(w))]


imputed.data<-missForest(Missing_train)
imputed.data<-cbind(Present_train,imputed.data$ximp)

imputed.data$GarageYrBlt<-as.factor(imputed.data$GarageYrBlt)






w<-as.data.frame(apply(test,2,function(x)sum(is.na(x))))
colnames(w)[1]<-"Missing_Value"
w$Variables<-rownames(w)
w<-w %>% filter(Missing_Value!=0)
rownames(w)<-w$Variables
Missing_test<-test[colnames(test) %in% colnames(t(w))]
Present_test<-test[!colnames(test) %in% colnames(t(w))]



imputed.data.test<-missForest(Missing_test)
imputed.data.test<-cbind(Present_test,imputed.data.test$ximp)




##Numeric_Data
numeric_train_data<-imputed.data[sapply(imputed.data,is.numeric)]
x<-summary(numeric_train_data)
x<-as.data.frame(x)
a<-as.data.frame(apply(numeric_train_data,2,function(x)quantile(x,c(.01,.99),na.rm=T)))


##Visualization




apply(numeric_train_data,2,function(x)ggplot(numeric_train_data,mapping=aes(x,SalePrice))+geom_point()+geom_smooth(method=lm))

ggplot(immputed.data,mapping=aes(Gr))

ggplot(imputed.data,mapping=aes(SalePrice))+geom_histogram()+geom_line(stat = "density")
ggplot(imputed.data,mapping=aes(log(GrLivArea+1)))+geom_histogram()


ggplot(train_data, aes(x=SalePrice)) + 
  geom_histogram( aes(y=..density..),
                  alpha=.5, bins = 75, 
                  colour="black", 
                  fill="white") +
  geom_line(aes(y=..density..), color='blue', lwd = 1, stat = 'density')+
  stat_function(fun=dnorm,color = 'red',args=list(mean=mean(train_data$SalePrice),sd=sd(train_data$SalePrice)))+
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  annotate('text', label = paste('skewness =', signif(skewness(train$SalePrice),4)),
           x=500000,y=7.5e-06) + theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1)) +labs(title = "Density Plot of SalePrice")+theme(plot.title = element_text(hjust = 0.5))




ggplot(train_data, aes(x=log(SalePrice+1))) + 
  geom_histogram( aes(y=..density..),
                  alpha=.5, bins = 75, 
                  colour="black", 
                  fill="white") +
  geom_line(aes(y=..density..), color='blue', lwd = 1, stat = 'density')+
  stat_function(fun=dnorm,color = 'red',args=list(mean=mean(log(train_data$SalePrice+1)),sd=sd(log(train_data$SalePrice+1))))+
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  annotate('text', label = paste('skewness =', signif(skewness(log(train$SalePrice+1)),4))) + 
  theme(plot.subtitle = element_text(vjust = 1), 
                                       plot.caption = element_text(vjust = 1)) +labs(title = "Density Plot of SalePrice")+theme(plot.title = element_text(hjust = 0.5))



t<-train_data %>% group_by(Neighborhood) %>% summarize(mean_price=mean(SalePrice))

t$Neighborhood<-factor(t$Neighborhood,levels=t$Neighborhood[order(t$mean_price,decreasing = T)])

ggplot(t,aes(Neighborhood,mean_price))+geom_col() + theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    axis.text.x = element_text(vjust = 0, 
        angle = 30)) + scale_y_continuous(labels=scales::comma)+
  labs(title = "Mean Price of Neighbourhood")+theme(plot.title = element_text(hjust = 0.5))


t<-train_data %>% group_by(OverallQual) %>% summarize(mean_price=mean(SalePrice),counter=count(OverallQual))

ggplot(t,aes(OverallQual,mean_price))+geom_col()+ theme(plot.subtitle = element_text(vjust = 1), 
                                                          plot.caption = element_text(vjust = 1), 
                                                          axis.text.x = element_text(vjust = 0, 
                                                                                     angle = 30)) + scale_y_continuous(labels=scales::comma)+
  labs(title = "Mean Price of OverallQual")+theme(plot.title = element_text(hjust = 0.5))



ggplot(imputed.data,mapping=aes(log(GrLivArea+1),log(Saleprice+1)))+geom_point()+geom_smooth(method="lm")
ggplot(imputed.data,mapping=aes(GrLivArea,Saleprice))+geom_point()+geom_smooth(method="lm")

ggplot(imputed.data,mapping=aes(PoolArea,Saleprice))+geom_point()+geom_smooth(method="lm")
ggplot(imputed.data,mapping=aes(PoolArea,Saleprice))+geom_point()+geom_smooth(method="lm")

##Correlation_Matrix


x<-as.data.frame(cor(na.omit(numeric_train_data)))
write.csv(x,"E:/Rutgers/2nd-Sem/Machine_Learning_Project/x.csv")
corrplot(cor(na.omit(numeric_train_data)),method="circle")
corrplot(cor(na.omit(numeric_train_data)),method="color")



##Creating Dummy Variables

dummy_train<-dummyVars(" ~ .",data=imputed.data)
dummy_train<-data.frame(predict(dummy_train,newdata=imputed.data))

dummy_test<-dummyVars(" ~ .",data=imputed.data.test)
dummy_test<-data.frame(predict(dummy_test,newdata=imputed.data.test))

##Modelling Techniques




#Linear Regression
lmod<-lm(log(SalePrice+1)~.,data=imputed.data)
model_summary<-step(lmod,direction = "both",k=2)
summary(lmod)
summary(model_summary)

fitted<-model_summary$fitted.values
esiduals<-model_summary$residuals

lm_final<-as.data.frame(cbind(fitted,residuals))

ggplot(lm_final,aes(fitted,residuals))+geom_point()+ggtitle("Residuals Vs Fitted")  + theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    plot.title = element_text(hjust = 0.5))

plot(model_summary$fitted.values,model_summary$residuals)


sqrt(mean((log(imputed.data$SalePrice+1)-model_summary$fitted.values)^2))

imputed.data.test<-imputed.data.test[!imputed.data.test$Condition2 %in% c("PosA","RRNn"),]
imputed.data.test<-imputed.data.test[!imputed.data.test$YearBuilt %in% c("1875","1906","1911"),]
imputed.data.test<-imputed.data.test[!imputed.data.test$RoofMatl %in% c("Metal"),]
imputed.data.test<-imputed.data.test[!imputed.data.test$Exterior1st %in% c("CBlock"),]
imputed.data.test<-imputed.data.test[!imputed.data.test$Heating %in% c("Floor"),]
imputed.data.test<-imputed.data.test[!imputed.data.test$HeatingQC %in% c("Po"),]
imputed.data.test<-imputed.data.test[!imputed.data.test$Functional %in% c("Sev"),]

Fitted_Values<-predict(model_summary,imputed.data.test[-c(65)],type="response")

colnames(imputed.data.test)

lm_final<-as.data.frame(cbind(log(imputed.data.test$SalePrice+1),Fitted_Values))
colnames(lm_final)[1]<-"True_Value"
View(lm_final)



ggplot(lm_final,aes(Fitted_Values,True_Value))+geom_point()+geom_smooth()

sqrt(mean((log(imputed.data.test$SalePrice+1)-Fitted_Values)^2))


##Lasso

dummy_test<-dummyVars(" ~ .",data=imputed.data.test)
dummy_test<-data.frame(predict(dummy_test,newdata=imputed.data.test))


dummy_train$SalePrice<-log(imputed.data$SalePrice+1)

cv.lasso <- cv.glmnet(as.matrix(dummy_train[,-c(535)]), dummy_train$SalePrice, family='gaussian', alpha=1, parallel=TRUE, standardize=TRUE, type.measure='mse')
plot(cv.lasso)

lasso_final<-glmnet(x, y, alpha=1,lambda =cv.lasso$lambda.min , standardize=T)


results <-predict(cv.lasso, as.matrix(dummy_test[,-c(545)]), type="response")

cbind(log(dummy_test$SalePrice+1),results)



A<-as.data.frame(as.matrix(lasso_final$beta))
colnames(A)<-"Beta"
A$Variables<-rownames(A)

A<-A %>% filter(Beta!=0) %>% arrange(desc(abs(Beta))) %>% slice(1:10)
ggplot(A,mapping=aes(Variables,Beta))+geom_col()+coord_flip() + theme(plot.subtitle = element_text(hjust = .5), 
    plot.caption = element_text(hjust = .5)) +labs(title = "Top-10 Lasso Variables") 

 
##Ridge


cv.ridge <- cv.glmnet(as.matrix(dummy_train[,-c(535)]), dummy_train$SalePrice, family='gaussian', alpha=0, parallel=TRUE, standardize=TRUE, type.measure='mse',nfolds=10)
plot(cv.ridge)
title(main="Ridgre Regression")


colnames(imputed.data.test)

##SVR


b<-tune(svm,log(SalePrice+1)~.,type="nu-regression",kernel="radial",data=imputed.data,ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
a<-svm(log(SalePrice+1)~.,data=imputed.data,type="nu-regression",kernel="radial",cost=10)

plot(b)

results<-predict(a,imputed.data.test,type="response")

sqrt(mean((log(imputed.data.test$SalePrice+1)-results)^2))



##Random Forests

##imputed.data.test
imputed.data.test$YearBuilt<-as.numeric(as.character(imputed.data.test$YearBuilt))
imputed.data.test$GarageYrBlt<-as.numeric(as.character(imputed.data.test$GarageYrBlt))
imputed.data.test$YearRemodAdd<-as.numeric(as.character(imputed.data.test$YearRemodAdd))

imputed.data.test$YearBuiltType<-ifelse(imputed.data.test$YearBuilt>=1872 & imputed.data.test$YearBuilt<1950,"Vintage",ifelse(imputed.data.test$YearBuilt>=1950 & imputed.data.test$YearBuilt<1989,"PreModern","Modern_House"))


imputed.data.test$GarageYrBltType<-ifelse(imputed.data.test$GarageYrBlt>=1872 & imputed.data.test$GarageYrBlt<1960,"Vintage_G",ifelse(imputed.data.test$GarageYrBlt>=1950 & imputed.data.test$GarageYrBlt<1989,"PreModern_G","Modern_House_G"))

imputed.data.test$YearRemodAddtype<-ifelse(imputed.data.test$YearRemodAdd>=1872 & imputed.data.test$YearRemodAdd<1960,"Vintage_M",
                                    ifelse(imputed.data.test$YearRemodAdd>=1950 & imputed.data.test$GarageYrBlt<1989,
                                           "PreModern_M","Modern_House_M"))

imputed.data.test<-imputed.data.test %>% select(-GarageYrBlt,-YearBuilt,-YearRemodAdd)

imputed.data.test$YearBuiltType<-as.factor(imputed.data.test$YearBuiltType)
imputed.data.test$GarageYrBltType<-as.factor(imputed.data.test$GarageYrBltType)
imputed.data.test$YearRemodAddtype<-as.factor(imputed.data.test$YearRemodAddtype)




#impute.data-Train
imputed.data$YearBuilt<-as.numeric(as.character((imputed.data$YearBuilt)))
imputed.data$GarageYrBlt<-as.numeric(as.character((imputed.data$GarageYrBlt)))
imputed.data$YearRemodAdd<-as.numeric(as.character((imputed.data$YearRemodAdd)))

imputed.data$YearBuiltType<-ifelse(imputed.data$YearBuilt>=1872 & imputed.data$YearBuilt<1950,"Vintage",ifelse(imputed.data$YearBuilt>=1950 & imputed.data$YearBuilt<1989,"PreModern","Modern_House"))


imputed.data$GarageYrBltType<-ifelse(imputed.data$GarageYrBlt>=1872 & imputed.data$GarageYrBlt<1960,"Vintage_G",ifelse(imputed.data$GarageYrBlt>=1950 & imputed.data$GarageYrBlt<1989,"PreModern_G","Modern_House_G"))

imputed.data$YearRemodAddtype<-ifelse(imputed.data$YearRemodAdd>=1872 & imputed.data$YearRemodAdd<1960,"Vintage_M",
                                      ifelse(imputed.data$YearRemodAdd>=1950 & imputed.data$GarageYrBlt<1989,
                                             "PreModern_M","Modern_House_M"))

imputed.data<-imputed.data %>% select(-GarageYrBlt,-YearBuilt,-YearRemodAdd)

imputed.data$YearBuiltType<-as.factor(imputed.data$YearBuiltType)
imputed.data$GarageYrBltType<-as.factor(imputed.data$GarageYrBltType)
imputed.data$YearRemodAddtype<-as.factor(imputed.data$YearRemodAddtype)




set.seed(123)


rf1<-randomForest(log(SalePrice+1)~.,data=imputed.data,proximity=TRUE,importance=T)


Rf_results<-predict(rf1,imputed.data.test[,-62],type="response")

sqrt(mean((log(imputed.data$SalePrice+1)-rf1$predicted)^2))

sqrt(mean((log(imputed.data.test$SalePrice+1)-Rf_results)^2))



plot(gg_error(rf1)) + theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1)) +labs(title = "Out of Bag-Error")+theme(plot.title = element_text(hjust = 0.5))

a<-as.data.frame(rf1$importance)

a$Variables<-rownames(a)


a<-a %>% arrange(desc(IncNodePurity)) %>% group_by(Variables) %>% head(n=20)

a$Variables<-factor(a$Variables,levels=a$Variables[order(a$IncNodePurity)])


ggplot(a,aes(Variables,IncNodePurity))+geom_col()+coord_flip()+
  ggtitle("Variable Importance")+ theme(plot.title = element_text(hjust = 0.5))+scale_x_discrete(labels = scales::comma)


