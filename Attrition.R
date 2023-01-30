#project- to determine factors leading to employee turnover
library(readxl)
ATTRITION_DATA<-read_excel("ATTRITION DATA.xlsx")
data<-read_excel("ATTRITION DATA.xlsx")
#data validation
#counting rows and clms
dim(data)
#top ten rows and last10, data type
head(data,10)
tail(data, 10)
str(data)
#count of dependent variable
table(data$status)
#attrition percentage 
(170/686)*100
summary(data)
#converting numerical categorical variables as factors
cols<-c("status","job_level","no_of_promotions","risk_of_attrition","potential_rating")
data[cols]<-lapply(data[cols],factor)
str(data)

#univariate analysis
library(summarytools)
dfSummary(data)
#check missing values
colSums(is.na(data))
#no missing values

#feature engineering
#creating new variables- variance= rating 2018-2017
data$variance_rating<-as.factor(data$performance_rating_2018-data$performance_rating_2017)
data$variance_rating
data$percentage_salary_change<-(data$salary_2018-data$salary_2017)*100/data$salary_2018
data$percentage_salary_change
data$age<-2018-data$year_of_birth
summary(data[ ,c("variance_rating","percentage_salary_change","age")])
#dimension reduction
data[,c("year_of_birth","salary_2017","performance_rating_2018","performance_rating_2017","hire_data","e_code")]<-list(NULL)

#bivariate analysis
library(vcd)
#mosaic plot
mosaic(~gender+status,data=data,shape=T,colorize=T,gp=gpar(fill=matrix(c("red","blue","green","black"),2,2)))
#metric approach creating prop tables
tab<-table(data$status, data$gender)
prop.table(tab)
#row wise-most employees who left were men
round(prop.table(tab,1)*100,digits=2)
#column wise-employees did not leave based on gender both left equally
round(prop.table(tab,2)*100,digits=2)
#prop tables of service agreement
tab2<-table(data$status, data$service_agreement)
tab2
#shows employees who didnt have service agreement chose to leave more
#row wise
round(prop.table(tab2,1)*100,digits=2)
#clm wise- shows 90% people out of all who left didnt have service agreement left
round(prop.table(tab2,2)*100,digit=2)

#hypothesis testing- category vs category=chisqr test
#h0: The two variables are independent
#h1: the two variables relate to eachother
tab<-table(data$gender, data$status)
tab
chisq.test(tab)
#As p value>0.05 null will be accepted- both are independent variables
tab2<-table(data$service_agreement, data$status)
chisq.test(tab2)
#p<0.05- null is rejected- there is dependence in the variables

#bivariate analysis- numerical and categorical variables
#ggplot for violine plot
library(ggplot2)
df<-ggplot(data, aes(x=status, y=age), addMean=TRUE, meanPointShape=23, meanPointSize=3, meanPointColor="black",meanPointFill="blue")+geom_violin()
df+geom_violin(trim=FALSE)+geom_violin(draw_quantiles=c(0.25,0.5,0.75))
#mean per level of status
mean_age<-by(data$age, data$status, mean)
mean_age
#employees didnt leave based on age
med_dist<-by(data$distance_from_home, data$status, median)
med_dist

#hypothesis testing(ttest) categorical with continuous variables like age, distance
#h0= age and status are independent
#h1=age and staus are dependent
t.test(age~status,data=data)
#p>0.05- accept null 
t.test(distance_from_home~ status, data=data)
#p<0.5- reject null=there is connection

#removing insignificant variables
data[,c("age","job_level","bonus", "gender")]<-list(NULL) 

#creating dummie variables
library(fastDummies)
dummie<-fastDummies::dummy_cols(data, remove_most_frequent_dummy = T,)
View(dummie)
names(dummie)
#dropping original variables of the dummies 
dummie[,c('service_agreement','gender')]<-list(NULL)
names(dummie)
colnames(dummie)[23]<-"var_rating_minus1"
colnames(dummie)[24]<-"var_rating_minus3"

#splitting data into train and test
library(caret)
set.seed(100)
#random number- 100
#70%training data and 30% test data
dft<-createDataPartition(dummie$status, p=0.7, list=FALSE)
str(dft)
#training data
x_train<-dummie[dft,]
#test data
x_test<-dummie[-dft,]
#change data type to factor not int
x_train$status<-as.factor(x_train$status)
x_test$status<-as.factor(x_test$status)

#logistic regression
model_1<-glm(status~.,data=x_train, family=binomial)
summary(model_1)
#removing the ones not defined due to singularity
model_2<-glm(status~.-no_of_promotions -potential_rating -var_rating_minus1 -var_rating_minus3, data=x_train, family=binomial)
summary(model_2)

#confusion matrix
predictions<-predict(model_1, x_test, type="response")
predictions<-ifelse(predictions>0.5,1,0)
confusionMatrix(table(x_test$status, predictions))
#model evaluation
library(pROC)
result.roc<-roc(x_test$status, as.numeric(as.character(predictions)))
plot(result.roc)
#area under curve
result.roc