real.estate.data<- read.csv(file="C:/Users/Olga/Desktop/Example2.1Data.csv", 
header=TRUE, sep=",")

#rescaling variables and specifying reference categories
price10K<- real.estate.data$price/10000
sqftK<-real.estate.data$sqft/1000
heating.rel<- relevel(real.estate.data$heating, ref="none")
AC.rel<-relevel(real.estate.data$AC, ref="no")
lotK<-real.estate.data$lot/1000

#fitting gamma regression 
summary(fitted.model<- glm(price10K ~ beds + baths 
+ sqftK + heating.rel + AC.rel + lotK, data=real.estate.data,
family=Gamma(link=log)))

#checking model fit
intercept.only.model<- glm(price10K ~ 1, family=Gamma(link=log))
print(deviance<- -2*(logLik(intercept.only.model)-logLik(fitted.model)))
print(p.value<- pchisq(deviance, df=7, lower.tail=FALSE))

#using fitted model for prediction
print(10000*predict(fitted.model, type="response", data.frame(beds=4, baths=2, 
sqftK=1.68, heating.rel="central", AC.rel="no", lotK=5)))

