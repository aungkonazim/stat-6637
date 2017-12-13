install.packages("geepack")
library(geepack)
data(ohio)
head(ohio)
## a longitudinal study of health effects of air pollution
## response indicator of wheeze status 1=yes 0=no
## id = a numeric vector for subject id
## age = a numeric vector of age 0=9 years
## smoke = an indicator whetrher the mother smoked when the study started
summary(ohio)
## resp is my response so we use what kind of link?logistic
## id is shown as numeric but it will work for id as numeric or factor
## age as numeric
## smoke can also be a number

fit.exch <- geeglm(resp~age+smoke,family=binomial(link="logit"),data=ohio,id=id,corstr="exchangeable")
summary(fit.exch)
