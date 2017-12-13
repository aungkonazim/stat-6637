setwd("C:\\Users\\aungkon\\Desktop\\stat\\assignment\\generalized linear model")
rm(list=ls())
install.packages("glm2")

library(glm2)
data(crabs)
attach(crabs)
head(crabs)
is.factor(GoodSpine)
logreg<-glm(Satellites~Width+Dark+GoodSpine,family=poisson)
summary(logreg)
1-pchisq(560.96,169)
logreg1<-glm(Satellites~Width*Dark*GoodSpine,family=poisson)
summary(logreg1)
1-pchisq(549.49,165)
logreg2<-glm(Satellites~Width*Dark,family=poisson)
summary(logreg2)
1-pchisq(555.42,169)


logreg3<-glm(Satellites~Width*Dark*GoodSpine,family=quasipoisson)
summary(logreg3)
plot(logreg3$residuals)

rm(list=ls())
data(heart)
head(heart)
attach(heart)
is.factor(AgeGroup)
AgeGroup
is.factor(Severity)
Severity
logreg<-glm(cbind(Deaths,Patients-Deaths)~as.factor(AgeGroup)+as.factor(Delay)+as.factor(Severity)+as.factor(Region),family=binomial)
summary(logreg)
1-pchisq(113.11,65)
logreg1<-glm(cbind(Deaths,Patients-Deaths)~as.factor(AgeGroup)*as.factor(Delay)*as.factor(Severity)*as.factor(Region),family=binomial)
summary(logreg1)
1-pchisq(2.7229e-10,0)
logreg2<-glm(cbind(Deaths,Patients-Deaths)~as.factor(AgeGroup)*as.factor(Delay)+as.factor(Severity)+as.factor(Region),family=quasibinomial)
summary(logreg2)
1-pchisq(97.567,61)

logreg3<-glm(cbind(Deaths,Patients-Deaths)~as.factor(AgeGroup)+as.factor(Severity)+as.factor(Region),family=binomial)
summary(logreg3)
logreg4<-glm(cbind(Deaths,Patients-Deaths)~as.factor(AgeGroup)*as.factor(Severity)*as.factor(Region)+as.factor(Delay),family=binomial)
summary(logreg4)
1-pchisq(62.885,47)
plot(logreg4$residuals)



logreg<-glm(cbind(Deaths,Patients-Deaths)~as.factor(AgeGroup)+as.factor(Delay)+as.factor(Severity)+as.factor(Region),family=binomial(link=probit))
summary(logreg)
1-pchisq(98.546,65)

logreg1<-glm(cbind(Deaths,Patients-Deaths)~as.factor(AgeGroup)*as.factor(Region)*as.factor(Severity)+as.factor(Delay),family=binomial(link=probit))
summary(logreg1)
1-pchisq(63.29,47)
plot(logreg1$residuals)

