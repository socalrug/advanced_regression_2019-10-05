smoking.data<- read.csv(file="C:/Users/Olga/Desktop/Example5.3Data.csv", header=TRUE, sep=",")
install.packages("pscl")
library(pscl)

#specifying reference category
health.rel<- relevel(smoking.data$health, ref="good")

#fitting zero-inflated Poisson model
summary(fitted.model<- zeroinfl(cigarettes ~ gender + age|health.rel, 
data=smoking.data))

#checking model fit
intercept.only.model<- zeroinfl(cigarettes ~ 1, data=smoking.data)
print(deviance<- -2*(logLik(intercept.only.model)-logLik(fitted.model)))
print(p.value<- pchisq(deviance, df=3, lower.tail=FALSE))

#using fitted model for prediction
print(predict(fitted.model, data.frame(gender="M", health.rel="good", 
age=50)))
