hospitalstay.data<- read.csv(file="C:/Users/Olga/Desktop/Example5.1Data.csv", header=TRUE, sep=",")

#eliminating zeros from the original data set
hospitaldays.data<- hospitalstay.data[ which(hospitalstay.data$days!=0), ]

install.packages("VGAM")
library(VGAM)

#fitting zero-truncated Poisson model
summary(fitted.model<- vglm(days ~ gender + age + illness,
data=hospitaldays.data, family=pospoisson()))

#checking model fit
intercept.only.model<- vglm(days ~ 1, 
data=hospitaldays.data, family=pospoisson())
print(deviance<- -2*(logLik(intercept.only.model)-logLik(fitted.model)))
print(p.value<- pchisq(deviance, df=3, lower.tail=FALSE))

#using fitted model for prediction
print(predict(fitted.model, data.frame(gender="M", age=55, illness="no"), 
type="response"))
