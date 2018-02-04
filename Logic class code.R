setwd("G:\\MY Project\\Jigsaw\\R\\Logistic Reg\\case study")

#Equation of Logistic Models

p(y)=1/(1+exp(-a-bx))

1-p/p=exp(-a-bx)

ln((1-p)/p)=-a-bx

ln(p/(1-p))=a+bx

#-------Importing the data---------

goodbad<-read.csv("GOODBAD.csv", header = TRUE)

str(goodbad)
dim(goodbad)
summary(goodbad)

#1=good, 0=bad
table(goodbad$Good.Bad)

plot(goodbad$Good.Bad)

#Checking for missing values

colSums(is.na(goodbad))# no na is there

# sample the dataset

sampling<-sort(sample(nrow(goodbad), nrow(goodbad)*.7))

length(sampling)
train<-goodbad[sampling,]
test<-goodbad[-sampling,]

nrow(train)
nrow(test)

#Table of y for the train dataset i.e % of 0 and 1

table(train$Good.Bad)/700
table(test$Good.Bad)/300


# check for account status
table(train$Good.Bad,train$Check_Account_Status)

#check for history

table(train$Good.Bad, train$CreditHistory)

#Logistic Regression

attach(train)

model1<-glm(Good.Bad~ Check_Account_Status+CreditHistory, data = train, family = "binomial")

summary(model1)

#Null deviance: 1221.7  on 999  degrees of freedom: It is a variance od dep variable excluding the independent variable
#Residual deviance: 1053.1  on 992  degrees of freedom: It is a variance od dep variable including the independent variable
#AIC: 1069.1 : lesser the AIC  better is the model

# now add duration

model2<-glm(Good.Bad~ Check_Account_Status+CreditHistory + Duration, data = train, family = "binomial")

summary(model2)

confint(model2)

head(model2$fitted.values)

#Finding Predicted Values
train$predict<-model2$fitted.values

#predict(model2,data=train,type="response")

#Confusion Matrix

train$pred_bkt<-ifelse(train$predict>0.72, "G", "B")

table(train$pred_bkt, train$Good.Bad)

#Plotting ROC Curve

# The prediction function of the ROCR library basically creates a structure to validate 
#our predictions qirh actual values

pred<-prediction(train$predict, train$Good.Bad)

# "tpr" and "fpr" are arguments of the "performance" function indicating that the plot is 
#between the true positive rate and the false positive rate.tpr=tp/o, fpr=fp/p
#instal package ROCR
library(ROCR)
?performance

pref<-performance(pred,"tpr", "fpr")
plot(pref, col="red")
abline(0,1, lty = 8, col = "grey")

# The prediction function of the ROCR library basically creates a structure to validate 
#our predictions qirh actual values

?abline

#How to choose cutoff's?
#use @ to access the slots

cutoff<-data.frame(cut=pref@alpha.values[[1]], fpr=pref@y.values[[1]], tpr=pref@x.values[[1]])
head(cutoff)

cutoff<-cutoff[order(cutoff$tpr, decreasing=TRUE),]
head(cutoff)

#AUC area uder curve


#Gives best fitted model
#To choose a good model
?step
reduced<-step(model2,direction="backward")
# based on the above code,CreditHistory had low AIC, so run a model with only tht variable

model3<-glm(data=train,Good.Bad ~ CreditHistory,
              family=binomial)
summary(model3)