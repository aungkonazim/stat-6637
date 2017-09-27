###multinomial
library(MASS)
head(survey)
smoke.freq<-table(survey$Smoke)
##suppose it is known that the proportion of these categories
##on college campuses nationwide is
##Heavy 4.5%
##Never 79.5%
##Occas 8.5%
##Regul 7.5%
##Test whether the proportions in the survey are the same as the
##college campuses nationwide
smoke.prob<-c(.045,.795,.085,.075)
## to test our null hypothesis
chisq.test(smoke.freq,p=smoke.prob)
##can't reject the null hypothesis - conclude the frequencies in the
##survey are the same as those on college campuses nationwide

###Test for Independence
##In the survey data, let's test whether smoking and exercise are ##independent
## make contingency table
tbl<-table(survey$Smoke, survey$Exer)
tbl
##4 by 3 contingency table
##test null hypothesis that smoking and exercise are independent
chisq.test(tbl)
##warning - some cell counts are too low
##combine cells
ctbl<-cbind(tbl[,"Freq"],tbl[,"None"]+tbl[,"Some"])
chisq.test(ctbl)
##Can't reject the null hypothesis, conclude Smoking and Exericise are
##independent in this survey
### If you don't have table data, you can make a matrix
color<-matrix(c(68,119,26,7,20,84,17,94,15,54,14,10,5,29,14,16),nrow=4,byrow=T,dimnames=list(eye.color=c("Brown","Blue","Hazel","Green"),hair.color=c("Black","Brown","Red","Blond")))
## to test whether hair color and eye color are independent
chisq.test(color)
## reject the null hypothesis and conclude hair color and eye color are
##not independent

##goodness of fit test for Poisson distribution
O<-c(229,211,93,35,7,0,0,1)
n<-c(0,1,2,3,4,5,6,7)
#mean
mn<-sum(O*n)/sum(O)
##expected
E<-sum(O)*mn^n*exp(-mn)/factorial(n)
##combine categories since the last 3 are <5
O<-c(O[1:4],sum(O[5:8]))
E<-c(E[1:4],sum(E[5:8]))
### Test statistic
T<-sum(((O-E)^2)/E)
##p-value compare to a chi-square distribution with 3 degrees of freedom
## k-1-1 (for estimating the mean)
pv<-1-pchisq(T,3)
##can't reject that the observations are from a Poisson distribution
##if all cells are small and we can't combine cells to get 
## a large enough cell count = Use Fisher's exact test that
##looks at all possible tables with fixed N
Job<-matrix(c(1,2,1,0,3,3,6,1,10,10,14,9,6,7,12,11),4,4,dimnames=list(income=c("<15k","15-25k","25-40k",">40k"),satisfaction=c("VeryD","LittleD","ModerateS","VeryS")))
## is income level independent of job satisfaction
## Are satisfaction levels the same for each income level (Homogeneity)
fisher.test(Job)
##p-value 0.7827 don't reject the null - income level is independent of
##job satisfaction


















