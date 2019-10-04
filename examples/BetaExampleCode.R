libraries.data<- read.csv(file="C:/Users/Olga/Desktop/Example7.1Data.csv", 
header=TRUE, sep=",")
install.packages("betareg")
library(betareg)

#specifying reference category
location.rel<- relevel(libraries.data$location, ref="rural")

#fitting beta regression model
summary(fitted.model<- betareg(propontime ~ nbooks + ncardholders
+ location.rel, data=libraries.data, link="logit")) 

#checking model fit
intercept.only.model<- betareg(propontime ~ 1,  
data=libraries.data, link="logit") 
print(deviance<- -2*(logLik(intercept.only.model)-logLik(fitted.model)))
print(p.value<- pchisq(deviance, df=3, lower.tail=FALSE))

#using fitted model for prediction
print(predict(fitted.model, 
data.frame(nbooks=15, ncardholders=2.5, location.rel="rural")))
       


