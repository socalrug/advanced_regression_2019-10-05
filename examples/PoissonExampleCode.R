hospitalstay.data<- read.csv(file="C:/Users/Olga/Desktop/Example5.1Data.csv", header=TRUE, sep=",")

#fitting Poisson model
summary(fitted.model<- glm(days ~ gender + age + illness, 
data=hospitalstay.data, family=poisson(link=log)))

#checking model fit
intercept.only.model<- glm(days ~ 1, 
data=hospitalstay.data, family=poisson(link=log))
print(deviance<- -2*(logLik(intercept.only.model)-logLik(fitted.model)))
print(p.value<- pchisq(deviance, df=3, lower.tail=FALSE))

#using fitted model for prediction
print(predict(fitted.model, data.frame(gender="M", age=55, illness="no"), 
type="response"))