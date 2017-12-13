## for the family you pick the canonical link is default
## you can specify the link by
## glm(formula,family=binomimal(link=probit))

## logistic regression - response is yes or no 0 or 1


rm(list=ls())

library(data.table)
data <- fread("http://data.princeton.edu/wws509/datasets/cuse.dat")
attach(data)
logreg <- glm(cbind(using,notUsing)~age+education+wantsMore,family=binomial)
summary(logreg)
