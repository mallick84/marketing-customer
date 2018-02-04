#Equation of Logistic Models
p(y)=1/(1+exp(-a-bx))

1-p/p=exp(-a-bx)

ln((1-p)/p)=-a-bx

ln(p/(1-p))=a+bx

#-------Importing the data---------
goodbad<-read.csv("GOODBAD.csv")
str(goodbad)
#1=good, 0=bad
table(goodbad$Good.Bad)

plot(goodbad$Good.Bad)
dim(goodbad)
#Checking for missing values
colSums(is.na(goodbad))

sampling<-sort(sample(nrow(goodbad), nrow(goodbad)*.7))

length(sampling)

#Select training sample
train<-goodbad[sampling,]
test<-goodbad[-sampling,]
nrow(train)
nrow(test)

#Table of y for the train dataset
table(train$Good.Bad)/700
table(test$Good.Bad)/300

table(train$Good.Bad,train$Check_Account_Status)
#Logistic Regression

myresult<-glm(data=train,Good.Bad ~ Check_Account_Status+CreditHistory,
              family=binomial)
summary(myresult)

#odds=exp(0.4800)
#p/1-p=exp(0.4800)
#p=odds/1+odds
exp(0.681595)
1.616074

1.616074/(1+1.616074)
exp(0.681595)/(1+exp(0.681595)) 
# it gives the chances of a person being good when there is a unit change in A12.


#p=1.75/(1+1.75)

#Iteration 2 : Adding Duration
myresult<-glm(data=train,Good.Bad ~ Check_Account_Status+CreditHistory+Duration,
              family=binomial)
summary(myresult)

confint(myresult)

#Finding Predicted Values
predicted <- myresult$fitted.values
head(predicted)

head(train$Good.Bad)

#predict(myresult,data=train,type="response")

#Confusion Matrix
predbkt<-ifelse(predicted>0.5,'G','B')

table(predbkt,train$Good.Bad)

table(train$Good.Bad)
#Plotting ROC Curve
library(ROCR)
?performance
# The prediction function of the ROCR library basically creates a structure to validate 
#our predictions qirh actual values

pred<-prediction(predicted,train$Good.Bad)

# "tpr" and "fpr" are arguments of the "performance" function indicating that the plot is 
#between the true positive rate and the false positive rate.
perf<-performance(pred,"tpr","fpr") #tpr=TP/P fpr=FP/N
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

True Positives : 338/489
False Positives : 58/211

#How to choose cutoff's?
#use @ to access the slots
cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])
head(cutoffs)
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
head(cutoffs)


auc<-performance(pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc

#Gives best fitted model
#To choose a good model
?step
reduced<-step(myresult,direction="backward")
# based on the above code,CreditHistory had low AIC, so run a model with only tht variable

myresult<-glm(data=train,Good.Bad ~ CreditHistory,
              family=binomial)
summary(myresult)
