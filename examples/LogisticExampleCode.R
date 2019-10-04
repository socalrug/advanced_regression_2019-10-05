companies.data<- read.csv(file="C:/Users/Olga/Desktop/Example3.1Data.csv", 
header=TRUE, sep=",")

#specifying reference categories
ownership.rel<- relevel(companies.data$ownership, ref="partner")
approach.rel<- relevel(companies.data$approach, ref="comp")

#fitting logistic model
summary(fitted.model<- glm(approach.rel ~ ownership.rel + nemployees, 
data=companies.data, family=binomial(link=logit)))

#extracting AICC and BIC for fitted model
p<- 4
n<- 50
print(AICC<- -2*logLik(fitted.model)+2*p*n/(n-p-1))
BIC(fitted.model)

#checking model fit
intercept.only.model<- glm(approach.rel ~ 1, family=binomial(link=logit))
print(deviance<- -2*(logLik(intercept.only.model)-logLik(fitted.model)))
print(p.value<- pchisq(deviance, df=3, lower.tail=FALSE))

#using fitted model for prediction
print(predict(fitted.model, type="response", 
data.frame(ownership.rel="sole", nemployees=40)))
