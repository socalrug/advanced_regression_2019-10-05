cholesterol.data<- read.csv(file="C:/Users/Olga/Desktop/Example8.1Data.csv", 
header=TRUE, sep=",")

#creating long-form data set
install.packages("reshape2")
library(reshape2)

longform.data<- melt(cholesterol.data, id.vars=c("id", "gender", "age"), 
variable.name = "LDLmonth", value.name="LDL")

#creating numeric variable for time
month<- ifelse(longform.data$LDLmonth=="LDL0", 0, ifelse(longform.data$LDLmonth
=="LDL6", 6, ifelse(longform.data$LDLmonth=="LDL9",9,24)))

#plotting histogram with fitted normal density
install.packages("rcompanion")
library(rcompanion)

plotNormalHistogram(longform.data$LDL)

#testing for normality of distribution 
shapiro.test(longform.data$LDL)

#specifying reference category
gender.rel<- relevel(longform.data$gender, ref="M")

#fitting random slope and intercept model
install.packages("nlme")
library(nlme)

summary(fitted.model<- lme(LDL ~ gender.rel+age+month, 
random =~ 1+month|id, data=longform.data))
intervals(fitted.model)

#computing AICC
n<- 108
p<- 8
print(AICC<- -2*logLik(fitted.model)+2*p*n/(n-p-1))

#checking model fit
null.model <- glm(LDL~gender.rel+age+month,
data=longform.data)
print(deviance<- -2*(logLik(null.model)-logLik(fitted.model)))
print(p.value<- pchisq(deviance, df=3, lower.tail=FALSE))

#using fitted model for prediction
print(predict(fitted.model, data.frame(gender.rel="F", 
age=48, month.num=3), level=0))


