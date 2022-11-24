mydata = read.csv('College_admission.csv')
str(mydata)

sum(is.na(mydata))  #There is 0 missing values in the dataset.

library(ggplot2)
boxplot(mydata)
boxplot(mydata)$out   #The boxplot shows the normal distribution of data and the presence of outliers so we perform outliers treatment by imputation with mean.

library(Hmisc)
impute(c(mydata$gpa, mydata$gre, mean))

#We turn admit, ses, rank, gender, race as factor variables.
mydata$admit = as.factor(mydata$admit)
mydata$ses = as.factor(mydata$ses)
mydata$Gender_Male = as.factor(mydata$Gender_Male)
mydata$rank = as.factor(mydata$rank)
mydata$Race = as.factor(mydata$Race)
str(mydata)

#To build our model, we split our dataset into a training set and a test set with a ratio of 70/30%.
set.seed(111)
spl = sample(2, nrow(mydata),replace = T,prob = c(0.7,0.3))
train_set = mydata[spl==1,]
test_set = mydata[spl==2,]

#We consider admit as our DV and the remaining variables as IDV.
model1 = glm(admit~ gre+gpa+ses+Gender_Male+Race+rank, data = train_set, family = binomial)
summary(model1)

#We chose to first drop the IDV gender, race and ses as they appear to be the less significant before running the model again.
model2 = glm(admit~ gpa+gre+rank, data = train_set, family = binomial)
summary(model2)

#Finally we drop the IDV gpa. 
model3 = glm(admit~ gre+rank, data = train_set, family = binomial)
summary(model3)   #It shows gre p-value has decreased, it is significant alongside rank3 and 4.

#Additionally, to evaluate the model we perform predictions.
p1 = predict(model3,train_set,type = 'response')
head(p1)
head(train_set)

#Then we calculate the misclassification rate.
predict1 = ifelse(p1>0.5,1,0)
tab1 = table(predicted= predict1,Actual = train_set$admit)
tab1        
1-sum(diag(tab1))/sum(tab1)
#The misclassification rate is 30%.

#We perform the same process with test_set.
p2 = predict(model3,test_set,type = 'response')
head(p2)
head(test_set)

predict2 = ifelse(p2>0.5,1,0)
tab2 = table(predicted= predict2,Actual = test_set$admit)
tab2        
1-sum(diag(tab2))/sum(tab2)
#The misclassification rate is 27%.