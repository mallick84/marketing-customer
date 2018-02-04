setwd("G:\\MY Project\\Jigsaw\\R\\Logistic Reg\\case study")
getwd()
library(gains)
library(dplyr)
library(irr)
library(caret)

sales<-read.csv("DirectMarketing.csv", header = TRUE)

# Direct Marketer who wants to come up with a process to identify good customers, identify customer id's who are considered good according to his definition
# changing target varible in to binary variable.

sales%>%mutate(Target=ifelse(AmountSpent>mean(AmountSpent),1,0))->sales
sales%>%select(-AmountSpent)->sales

str(sales)
summary(sales) #History missing value is present.

levels(sales)
class(sales)


#Minimal Data Prep
#dealing with missing value.

sales$history1<-ifelse(is.na(sales$History), "Missing", as.character(sales$History))
sales$history1<-as.factor(sales$history1)

#change int to factor in catalogs and children

sales$Catalogs<-as.factor(sales$Catalogs)
sales$Children<-as.factor(sales$Children)


sales<-sales[,-8]
dim(sales)

#splitting data into test and train.

set.seed(200)
index<-sample(nrow(sales), 0.7*nrow(sales), replace = FALSE)
train<-sales[index,]
test<-sales[-index,]

#Build the first model using all the variables 

attach(train)
sales_mod<-glm(Target~., data = train[,-9], family = "binomial")

summary(sales_mod)



#doing stepwise logistic reg so let R choose the perfect combination

step(sales_mod,direction = "both" )

#iterition no.2

sales_mod1<-glm(formula = Target ~ Age + Location + Salary + Children + Catalogs + 
                  history1, family = "binomial", data = train[, -9])

summary(sales_mod1)

# creating dummy variable for only the significance P-value for both train and test data.

train$AgeYoung_d<-ifelse(train$Age=="Young",1,0)

train$Hist.Mid_d<-ifelse(train$history1=="Medium",1,0)

train$Children2_d<-ifelse(train$Children=="2",1,0)

train$Children3_d<-ifelse(train$Children=="3",1,0)

test$AgeYoung_d<-ifelse(test$Age=="Young",1,0)

test$Hist.Mid_d<-ifelse(test$history1=="Medium",1,0)

test$Children2_d<-ifelse(test$Children=="2",1,0)

test$Children3_d<-ifelse(test$Children=="3",1,0)


#again do model include above variable

sales_mod2<-glm(formula = Target ~ AgeYoung_d + Location + Salary + Catalogs + 
                  Hist.Mid_d + Children2_d+ Children3_d, family = "binomial", data = train[, -9])

summary(sales_mod2)

#now do performance of the data, 
# predict the data

sales_pred<-predict(sales_mod2, type = "response", newdata = test)

head(sales_pred)

#now see probability of good customer(1), probability of bad customer(0) by table
table(sales$Target)/nrow(sales)

#now choose only the good customer having cutoff value 0.399

sales_pred<-ifelse(sales_pred>=0.399,1,0)
View(sales_pred)

#calibrating with kappa from library irr measure

kappa2(data.frame(test$Target, sales_pred)) # kappa=0.76 which is >0.6. so good

# now do another check confusion matrix from library caret.
args(confusionMatrix)

confusionMatrix(sales_pred, test$Target, positive = "1")

#next is gain chart which is very importent to varify. if my model is good then it will
#varify the top rows at (P=1) having higher probability and also shows higher accurecy 
# thus it will ben in descending order.

gains(test$Target, predict(sales_mod2, type = "response",newdata = test), groups = 10)

#we see that that first 30% having accuricy of 70% whis is good model

# let us incorporate it in test data
test$probab<-predict(sales_mod2, type = "response", newdata = test) 

# now quantile

test$quantile<-quantile(test$probab, prob = c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

# link with athe good customer of having 70% good score to the customer Id

Targetd<-test[test$probab>0.732602471 & test$probab<=0.999747759, "cust_Id"]

