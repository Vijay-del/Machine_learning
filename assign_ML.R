library(caret)
library(dplyr)
library(readr)
UniversalBank <- read_csv("C:/Users/Vijay/Downloads/UniversalBank.csv")  # Importing data set
View(UniversalBank)
bank<-UniversalBank[,c(-1,-5)] # excluding columns 1 and 5
View(bank)
set.seed(15)
train_index<-createDataPartition(bank$Age,p=0.6,list=FALSE) # creating partition of data into 60% & 40%
train_data<-bank[train_index,] #60% training data
val_data<-bank[-train_index,]  #40% validating data
test_index<-createDataPartition(bank$Age,p=0.2,list=FALSE) # creating parttion of data into 20% 
test_data<-bank[test_index,] #20% testing data
traval_data<-bank[-test_index,] 


train.norm.df<-train_data[,-8] #excluding perdictive indicator and assigning to the varialbe for normalizing the data
val.norm.df<-val_data[,-8]
test.norm.df<-test_data[,-8]
traval.norm.df<-traval_data[,-8]

#Normalizing the data
norm.values<-preProcess(train.norm.df,method = c("center","scale"))
train.norm.df<-predict(norm.values,train.norm.df)
val.norm.df<-predict(norm.values,val.norm.df)
test.norm.df<-predict(norm.values,test.norm.df)
traval.norm.df<-predict(norm.values,traval.norm.df)

# Assigning labels to the levels 
train_data$`Personal Loan`<-factor(train_data$`Personal Loan`,levels = c(0,1),labels = c("Deny","Accept"))
val_data$`Personal Loan`<-factor(val_data$`Personal Loan`,levels = c(0,1),labels = c("Deny","Accept"))
test_data$`Personal Loan`<-factor(test_data$`Personal Loan`,levels = c(0,1),labels = c("Deny","Accept"))
traval_data$`Personal Loan`<-factor(traval_data$`Personal Loan`,levels = c(0,1),labels = c("Deny","Accept"))

#Modelling KNN
library(FNN)
nn<- knn(train.norm.df,test=test.norm.df,cl=train_data$`Personal Loan`,k=1,prob = TRUE)
nn1<- knn(train.norm.df,test=test.norm.df,cl=train_data$`Personal Loan`,k=4,prob = TRUE)
confusionMatrix(nn,test_data$`Personal Loan`)$overall[1] 
confusionMatrix(nn1,test_data$`Personal Loan`)$overall[1] 

###Hypertuning using validation####
accuracy.df <- data.frame(k = seq(1, 55, 1), accuracy = rep(0, 55))
for(i in 1:55) {
  knn.pred <- knn(train.norm.df, val.norm.df, 
                  cl = train_data$`Personal Loan`, k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, val_data$`Personal Loan`)$overall[1] 
}
accuracy.df

accuracy.df[which.max(accuracy.df$accuracy),]

#choice of k that balances between overfitting is k=4;

library(gmodels)
CrossTable(x=test_data$`Personal Loan`,y=nn,prop.chisq = FALSE)
CrossTable(x=test_data$`Personal Loan`,y=nn1,prop.chisq = FALSE)

###################################################################################
# predict on Traval data set

norm.values<-preProcess(traval.norm.df,method=c("center","scale"))
traval.norm.df<-predict(norm.values,traval.norm.df)
test.norm.df<-predict(norm.values,test.norm.df)
nn2<-knn(traval.norm.df,test.norm.df,cl=traval_data$`Personal Loan`,k=4)
CrossTable(x=test_data$`Personal Loan`,y=nn2,prop.chisq = FALSE)

###################################################################################
# For the customer with below criteria, the model prediction would be "deny"

l1 <- data.frame("Age" = 40, "Experience" = 10, "Income" = 84, "Family" = 2, "CCAvg" = 2, "Education"= 1, "Mortgage" = 0, "Personal Loan"= "Deny","Securities Account" = 0, "CD Account" = 0, "Online" = 1,"Credit Card" = 1)
l2 <- data.frame("Age" = 40, "Experience" = 10, "Income" = 84, "Family" = 2, "CCAvg" = 2, "Education"= 2, "Mortgage" = 0, "Personal Loan"= "Accept","Securities Account" = 0, "CD Account" = 0, "Online" = 1,"Credit Card" = 1)
l3 <- data.frame("Age" = 40, "Experience" = 10, "Income" = 84, "Family" = 2, "CCAvg" = 2, "Education"= 3, "Mortgage" = 0, "Personal Loan"= "Deny","Securities Account" = 0, "CD Account" = 0, "Online" = 1,"Credit Card" = 1)
test_pre<-as.data.frame(rbind(l1,l2,l3))

test_pre.norm<-test_pre[,-8]
norm.values<- preProcess(test_pre.norm,method = c("center","scale"))
test_pre.norm<-predict(norm.values,test_pre.norm)
nn3<- knn(train.norm.df,test=test_pre.norm,cl=train_data$`Personal Loan`,k=4,prob = TRUE)
CrossTable(x=test_pre$`Personal.Loan`,y=nn3,prop.chisq = FALSE)

###################################################################################
#Data is paritioned into training, validation, and test sets (50% : 30% : 20%).

bank1<-UniversalBank[,c(-1,-5)] # excluding columns 1 and 5
View(bank1)
set.seed(15)
train_index1<-createDataPartition(bank1$Age,p=0.5,list=FALSE) # creating partition of data into 50%,30%,20%
train_data1<-bank1[train_index1,] #50% training data
val_data1<-bank1[-train_index1,]  #50% validating data
test_index1<-createDataPartition(val_data1$Age,p=0.2,list=FALSE) # creating parttion of data into 20% 
test_data1<-val_data1[test_index1,] #20% testing data
val_data1<-val_data1[-test_index,] 


train.norm.df1<-train_data1[,-8] #excluding perdictive indicator and assigning to the varialbe for normalizing the data
val.norm.df1<-val_data1[,-8]
test.norm.df1<-test_data1[,-8]


#Normalizing the data
norm.values1<-preProcess(train.norm.df1,method = c("center","scale"))
train.norm.df1<-predict(norm.values1,train.norm.df1)
val.norm.df1<-predict(norm.values1,val.norm.df1)
test.norm.df1<-predict(norm.values1,test.norm.df1)


# Assigning labels to the levels 
train_data1$`Personal Loan`<-factor(train_data1$`Personal Loan`,levels = c(0,1),labels = c("Deny","Accept"))
val_data1$`Personal Loan`<-factor(val_data1$`Personal Loan`,levels = c(0,1),labels = c("Deny","Accept"))
test_data1$`Personal Loan`<-factor(test_data1$`Personal Loan`,levels = c(0,1),labels = c("Deny","Accept"))


#Modelling KNN
library(FNN)
n_n<- knn(train.norm.df1,test=test.norm.df1,cl=train_data1$`Personal Loan`,k=4,prob = TRUE)
confusionMatrix(n_n,test_data1$`Personal Loan`)$overall[1]

###Hypertuning using validation####
accuracy.df1 <- data.frame(k = seq(1, 55, 1), accuracy = rep(0, 55))
for(i in 1:55) {
  knn.pred1 <- knn(train.norm.df1, val.norm.df1, 
                  cl = train_data1$`Personal Loan`, k = i)
  accuracy.df1[i, 2] <- confusionMatrix(knn.pred1, val_data1$`Personal Loan`)$overall[1] 
}
accuracy.df1

accuracy.df1[which.max(accuracy.df$accuracy),]

#choice of k that balances between overfitting is k=4;

library(gmodels)
CrossTable(x=test_data1$`Personal Loan`,y=n_n,prop.chisq = FALSE) 

#The accuracy with test and validattio data sets respectively are 96.01% and 95.22% the reason being would be due to sampling bias. 

###################################################################################


























