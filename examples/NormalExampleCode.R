job.satisfaction.data<- read.csv(file="C:/Users/Olga/Desktop/Example1.1Data.csv",
header=TRUE, sep=",")

#plotting histogram with fitted normal density
install.packages("rcompanion")
library(rcompanion)
plotNormalHistogram(job.satisfaction.data$score)

#testing normality of distribution 
shapiro.test(job.satisfaction.data$score)

#specifying reference levels
educ.rel<- relevel(job.satisfaction.data$educ, ref="masters") 
gender.rel<- relevel(job.satisfaction.data$gender, ref="F")

#fitting general linear model
summary(fitted.model<- glm(score ~ gender.rel + age + educ.rel, 
data=job.satisfaction.data, family=gaussian(link=identity)))

#outputting estimated sigma
sigma(fitted.model)

#checking model fit 
intercept.only.model<- glm(score ~ 1, data=job.satisfaction.data, 
family=gaussian(link=identity))
print(deviance<- -2*(logLik(intercept.only.model)-logLik(fitted.model)))
print(p.value<- pchisq(deviance, df=4, lower.tail=FALSE))

#using fitted model for prediction
print(predict(fitted.model, data.frame(gender.rel="F", 
age=40, educ.rel="bachelor")))


