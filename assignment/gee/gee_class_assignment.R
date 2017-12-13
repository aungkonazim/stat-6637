rm(list=ls())
setwd("C:\\Users\\aungkon\\Desktop\\stat\\assignment\\gee")
data1 <- read.csv("EG_final.csv",header=T)
head(data1)
attach(data1)
summary(data1)
library(geepack)

fit.exch<-geeglm(Dead~as.factor(Dose)+as.factor(Sex)+as.factor(left), family=binomial(link="logit"),data=data1,id=data1$Dam.ID,corstr="independent")
summary(fit.exch)
library(doBy)
est<-esticon(fit.exch,diag(8))
OR.CI<-exp(cbind(est$Estimate,est$Lower,est$Upper))
rownames(OR.CI)<-names(coef(fit.exch))
colnames(OR.CI)<-c("OR","Lower","Upper")
OR.CI
predict<-predict(fit.exch,type="response")
table(data1$Dead,predict>0.5)
cbind(data1$Dead,predict>0.5)
est1<-esticon(fit.exch,c(0,0,0,-1,0,0,0,0)) 
exp(est1$Estimate)

fit.exch<-geeglm(Dead~Sex+as.factor(left)+as.factor(Dose), family=gaussian(link="identity"),data=data1,id=data1$Dam.ID,corstr="exchangeable")
summary(fit.exch)
library(doBy)
est<-esticon(fit.exch,diag(8))
OR.CI<-exp(cbind(est$Estimate,est$Lower,est$Upper))
rownames(OR.CI)<-names(coef(fit.exch))
colnames(OR.CI)<-c("OR","Lower","Upper")
OR.CI
predict<-predict(fit.exch,type="response")
table(data1$Dead,predict>0.5)
cbind(data1$Dead,predict>0.5)
est1<-esticon(fit.exch,c(0,0,0,0,0,0,0,-1)) 
exp(est1$Estimate)


fit.exch<-geeglm(Mal~as.factor(Dose)+as.factor(Sex), family=binomial(link="logit"),data=data1,id=data1$Dam.ID,corstr="exchangeable")
summary(fit.exch)
library(doBy)
est<-esticon(fit.exch,diag(7))
est
OR.CI<-exp(cbind(est$Estimate,est$Lower,est$Upper))
rownames(OR.CI)<-names(coef(fit.exch))
colnames(OR.CI)<-c("OR","Lower","Upper")
OR.CI
predict<-predict(fit.exch,type="response")
table(data1$Mal,predict>0.5)
cbind(data1$Dead,predict>0.5)
est1<-esticon(fit.exch,c(0,0,0,-1,0)) 
exp(est1$Estimate)
is.factor(Dose)
is.factor(left)

male <- numeric(length(Sex))
female <- numeric(length(Sex))
neutral <- numeric(length(Sex))
male[which(data1$Sex=="M")] <- 1
female[which(data1$Sex=="F")] <- 1
neutral[which(data1$Sex=="N")] <- 1
data1$male <- male
data1$female <- female
data1$neutral <- neutral

fit.exch<-geeglm(Wt~male+female+Mal+as.factor(Dose)+as.factor(left), family=poisson,data=data1,id=data1$Dam.ID,corstr="unstructured")
summary(fit.exch)
library(doBy)
est<-esticon(fit.exch,diag(8))
OR.CI<-exp(cbind(est$Estimate,est$Lower,est$Upper))
rownames(OR.CI)<-names(coef(fit.exch))
colnames(OR.CI)<-c("OR","Lower","Upper")
predict<-predict(fit.exch,type="response")
table(data1$Mal,predict>0.5)
cbind(data1$Dead,predict>0.5)
est1<-esticon(fit.exch,c(0,0,0,0,0,0,0,1)) 
exp(est1$Estimate)

