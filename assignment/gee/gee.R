##in R there are two packages that do gee's gee and geepack
##install and load package geepack
install.packages("geepack")
library(geepack)
data(ohio)
head(ohio)
##a longitudinal study of health effects of air pollution
##resp=an indicator of wheeze status 1=yes 0=no
##id = a numeric vector for subject id
##age = a numeric vector of age 0=9years
##smoke = an indicator of whether the mother smoked when the study started
summary(ohio)

### resp is my response so we use what link? logistic
##id is numeric but it will work for id as numeric or factor
##age is numeric
##smoke can also be a number 
fit.exch<-geeglm(resp~age+smoke, family=binomial(link="logit"),data=ohio,
id=id,corstr="exchangeable")
##look at an unstructured covariance matrix
fit.unstr<-geeglm(resp~age+smoke,family=binomial(link="logit"),data=ohio,
id=id,corstr="unstructured")
##in this case all the estimated correlations (alpha) were close to the 
##estimate from the exchangeable fit so the standard errors of estimates
##do not differ much - also an exchangeable model is appropriate
### what happens if we treat age as a factor?
fit<-geeglm(resp~factor(age)+smoke,family=binomial(link="logit"),data=ohio,
id=id,corstr="exchangeable")
##test the effect of smoke using ANOVA
fit2<-geeglm(resp~factor(age),family=binomial(link="logit"),data=ohio,id=id,
corstr="exchangeable")
anova(fit,fit2)
##Null hypothesis is that the parameter on the terms in fit that are not in
##fit2 are zero.  If p-value is small we need the terms in the model
##otherwise we don't
###model diagnostics
install.packages("doBy")
library(doBy)
##individual wald tests and C.I. for each parameter
est<-esticon(fit,diag(5))
##diag(5) makes a 5 by 5 identity matrix. this is the dimension of the 
##Cov(beta-hat)
##odds ratio and confidence intervals
##odds of an event = prob(event happens)/prob(does not happen)
##odds ratios are odds(event A)/odds(event B)
##odds ratios and confidence intervals
OR.CI<-exp(cbind(est$Estimate,est$Lower,est$Upper))
rownames(OR.CI)<-names(coef(fit))
colnames(OR.CI)<-c("OR","Lower","Upper")
##OR.CI for each coefficient, it the confidence interval contains 1 that term 
##is not needed in the model.
##odds ratio of wheezing for a 9 year old with a mother who smoked compared
##to a 8 year old with mother who did not smoke
##[smoke+factor(age)0] compared to [factor(age)-1]
esticon(fit,c())####more to come
predict<-predict(fit,type="response")
table(ohio$resp,predict>0.5)
##makes a confusion matrix - which assesses the fit of the model



































