# Data set has been imported from UCI Machine learning repository.
# Data Set choosen is "adult.data"
library(readr)
adult<-read_csv("adult.data",col_names = FALSE)
View(adult)
names(adult)<-c("age", "workclass", "fnlwgt", "education", "education-num","marital-status", "occupation", "relationship", "race", "sex","capital-gain", "capital-loss", "hours-per-week", "native-country","50K<=50K")
############Descriptive Statistics#######################
mean(adult$age) #Average age of the workers
tapply(adult$`education-num`,adult$education,mean) #Average number of students per class
median(adult$age) #Median of the ages
sort(table(adult$age), decreasing = TRUE)[1] #Mode of the ages
print(table(adult$sex))  # To print the number of males and females in the class
a<-adult[2,] # To print the second row of the table
print(a)
print(adult[c(1:4),c(1:2)]) # To print first four rows and 2 columns of the data set
print(adult[c(1:6),3]) # To fetch the data of 3rd coloumn with 6 rows.
class(adult$`education-num`)# Transformation of numeric vector to characters 
w<-as.character(adult$`education-num`)
class(w)
x<-adult$age #Variance
var(x) 
z<-sum((x-mean(x))^2)/(length(x)-1) #Variance without function var()
sd(x) #Standard Deviation
sqrt(z) #SD without function sd()
summary(adult) #Summary of data set
s<-x # na.rm option
s[sample(s,5)]<-NA
mean(s,na.rm = TRUE)
quantile(s,na.rm = TRUE)
edun<-adult$`education-num`#Correlation b/w age and education-num in the data set using cor() function
cor(x,edun)
mat1<-as.matrix(cbind(x,edun)) #converting vectors to matrix form
cor(mat1)
h<-hist(adult$age,main = "Histogram of ages",xlab = "Ages", col = "lightgrey") # Histogram 
text(h$mids,h$counts,labels = h$counts, adj=c(0.5,-0.5)) # To label the bars
x1<-x[1:75]
x2<-range(x1)
plot(x1,c(5:79), main = "Correlation b/w no.of students at different stages",xlab ="Ages", ylab = "Frequency Count",pch=19) #Scatter plots
abline(lm(x1~c(5:79)),col="blue") # regression line shows the trend line between most no. of studetns at specific age
l<-sample(1:10,5)
plot(l,pch=20,type = "b")






