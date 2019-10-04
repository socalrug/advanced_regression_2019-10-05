cholesterol.data<- read.csv(file="C:/Users/Olga/Desktop/Example8.1data.csv", 
header=TRUE, sep=",")

#creating long-form data set
install.packages("reshape2")
library(reshape2)

longform.data<- melt(cholesterol.data, id.vars=c("id", "gender", "age"), 
variable.name = "LDLmonth", value.name="LDL")

#creating numeric variable for time
month<- ifelse(longform.data$LDLmonth=="LDL0", 0, ifelse(longform.data$LDLmonth
=="LDL6", 6, ifelse(longform.data$LDLmonth=="LDL9",9,24)))

#specifying reference category
gender.rel<- relevel(longform.data$gender, ref="M")

install.packages("nlme")
library(nlme)

#fitting random slope and intercept model with
#unstructured covariance matrix of error terms
summary(un.fitted.model<- lme(LDL ~ gender.rel + age + month,
random=~ 1 + month|id, data=longform.data,
correlation=corSymm(), weights=varIdent(form=~ id|month)))
getVarCov(un.fitted.model, type="conditional")

#computing AICC
n<- 108
p<- 17
print(AICC<- -2*logLik(un.fitted.model)+2*p*n/(n-p-1))

#checking model fit
summary(null.model<- glm(LDL ~ gender.rel + age + month,
data=longform.data, family=gaussian(link=identity)))
print(deviance<- -2*(logLik(null.model)-logLik(un.fitted.model)))
print(p.value<- pchisq(deviance, df=12, lower.tail=FALSE))


#fitting random slope and intercept model with
#Toeplitz covariance matrix of error terms 
summary(toep.fitted.model<- lme(LDL ~ gender.rel + age + month, 
random=~ 1 + month|id, data=longform.data, 
correlation=corARMA(form=~ 1|id, p=1, q=1)))
getVarCov(toep.fitted.model, type="conditional")

#computing AICC
p<- 11
print(AICC<- -2*logLik(toep.fitted.model)+2*p*n/(n-p-1))

#checking model fit
print(deviance<- -2*(logLik(null.model)-logLik(toep.fitted.model)))
print(p.value<- pchisq(deviance, df=6, lower.tail=FALSE))

#fitting random slope and intercept model with
#spatial power covariance matrix of error terms
summary(sppow.fitted.model<- lme(LDL ~ gender.rel + age + month,
random=~ 1 + month|id, data=longform.data,
correlation=corCAR1(form=~ month|id)))
getVarCov(sppow.fitted.model, type="conditional")

#computing AICC
p<- 9
print(AICC<- -2*logLik(sppow.fitted.model)+2*p*n/(n-p-1))

#checking model fit
print(deviance<- -2*(logLik(null.model)-logLik(sppow.fitted.model)))
print(p.value<- pchisq(deviance, df=4, lower.tail=FALSE))

#fitting random slope and intercept model with
#autoregressive covariance matrix of error terms
summary(ar1.fitted.model<- lme(LDL ~ gender.rel + age + month,
random=~ 1 + month|id, data=longform.data,
correlation=corAR1(form=~ 1|id)))
getVarCov(ar1.fitted.model, type="conditional")

#computing AICC
p<- 9
print(AICC<- -2*logLik(ar1.fitted.model)+2*p*n/(n-p-1))

#checking model fit
print(deviance<- -2*(logLik(null.model)-logLik(ar1.fitted.model)))
print(p.value<- pchisq(deviance, df=4, lower.tail=FALSE))

#fitting random slope and intercept model with
#compound symmetric covariance matrix of error terms
summary(cs.fitted.model<- lme(LDL ~ gender.rel + age + month, 
random=~ 1 + month|id, data=longform.data, 
correlation=corCompSymm(form=~ 1|id)))
getVarCov(cs.fitted.model, type="conditional")

#computing AICC
p<- 9
print(AICC<- -2*logLik(cs.fitted.model)+2*p*n/(n-p-1))

#checking model fit
print(deviance<- -2*(logLik(null.model)-logLik(cs.fitted.model)))
print(p.value<- pchisq(deviance, df=4, lower.tail=FALSE))

#using AR fitted model for prediction
print(predict(ar1.fitted.model, data.frame(gender.rel="F", 
age=48, month=3), level=0))