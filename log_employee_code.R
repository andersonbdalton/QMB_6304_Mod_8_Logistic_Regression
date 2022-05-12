#Dalton Anderson

rm(list=ls())

library(readxl)
library(dplyr)
library(ggplot2)
library(readxl)

#preprocessing

#1.) Load in dataset
#Import data
work_master <- read_excel("6304 Module 8 Assignment Data Set.xlsx")
  

names(work_master)
colnames(work_master)=tolower(make.names(colnames(work_master)))
names(work_master)
str(work)
as.factor(work_master$department.name)
as.numeric(work_master$nummos)
#2.) Using the numerical portion of your U number as a random number seed, take a random sample of 3000 cases
#sample set created
work_master= work_master %>% 
  select(department.name, nummos, over5000)
tempdf = work_master
set.seed(59657076)
sample <- tempdf
work = sample_n(sample, 3000)

#explore data
unique(work$department.name)

explore = aggregate(x= work$over5000,
          by= list(work$department.name),
          FUN=sum)
#plot results
ggplot(data =explore)
glem.bar
ggplot(data = explore)+
  xlab("group") + 
  ylab("value") 

ggplot(data=explore, aes(x=Group.1, y=x)) +
  xlab("Department") + 
  ylab("Overtime Count")+
  geom_bar(stat = "identity")

#map department names
str(work)
#work$nummos<-as.factor(work$nummos)
work$over5000<-as.factor(work$over5000)
work$department.name<-as.factor(work$department.name)
str(work)
#Analysis (Using the Reduced Data Set)

#1.)	Parameterize a logistic regression model with over 5000 as the dependent and department.name and nummos independent variables.

#null model 
output0=glm(over5000~1,data=work,family=binomial)
summary(output0)
#working model
work.out=glm(over5000~department.name+nummos,
                   data=work,family=binomial)

#2.)	Using the summary() command report the results of the model from Step 1.  
summary(work.out)

#3.)	State whether you believe the Residual Deviance of your model is markedly different from the Null Deviance.

#there is a large difference between the null model and work.out model.
#leading for me to believe work.out reduces model error.


#4.)	Given your model from Part 1 and ignoring p values, which variable or variable/level will have the greatest influence 
#in increasing the modeled probability that an employee earned  $5000 or more in 2016?

#interpretation
#igoring the p-values the variable with the greatest influence would be department.namesFire

#5.)	Given your model from Part 1 and ignoring p values, which variable will have the greatest influence 
#in decreasing the modeled probability that an employee earned  $5000 or more in 2016?

#interpretation
#igoring the p-values the variable with the greatest influence would be department.nameStreets and Sanitation

#6.)	Using the expand.grid() command develop a prediction file with all independent variables in the Step 1 model. 
#For independent variables in this case use the unique() qualifier.  R will by default calculate predicted probabilities 
#to many decimal places.  For convenience in reporting round your stored predictions to only 4 decimal places.  
#Show the predicted probabilities for ONLY the first ten cases appearing in your prediction file.
# work Predictiion on Reduced Model

#predictiion on work model
pred.data=expand.grid(nummos=quantile(work$nummos,
                                       c(0,.25,.5,.75,1)),
                      
                      over5000=unique(work$over5000),
                      department.name=unique(work$department.name))
pred.probs=round(predict(work.out,
                         newdata=pred.data,type="response"),4)
work.predictions=cbind(pred.data,pred.probs)
#only 10
work.predictions=work.predictions %>% slice(1:10)
#next step
pred.probs=round(predict(work.out,
                         newdata=pred.data,type="response"),4)
work.predictions=cbind(pred.data,pred.probs)

#plots
plot(work.predictions$pred.probs,pch=19,
     main="work Probabilities")
work.predictions=
  work.predictions[order(
    work.predictions$pred.probs),]

plot(work.predictions$pred.probs,pch=19,
     main="work Probabilities")
work.predictions=
  work.predictions[order(
    -work.predictions$pred.probs),]

plot(work.predictions$pred.probs,pch=19,
     main="work Probabilities")
work.predictions=
  work.predictions[order(
    work.predictions$pred.probs),]





#7.)	Based on your predictions generated in Step 6, find the maximum and minimum predicted probabilities generated.  
#State the values of the independent variables for these max and min cases. 
#plot fit
#maxs
plot(work$over5000,work.out$fitted.values,
     pch=19,
     main=
       "Actual Binaries & Fitted Probabilities - work")
max(work.out$fitted.values)
work[which.max(work.out$fitted.values),]

plot(work.predictions$pred.probs,pch=19,
     main="work Probabilities")
work.predictions[which.max
                 (work.predictions$pred.probs),]
work.predictions[which.min
                 (work.predictions$pred.probs),]

  max(work.predictions$pred.probs)
  min(work.predictions$pred.probs)
