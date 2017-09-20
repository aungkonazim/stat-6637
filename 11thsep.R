rm(list=ls())

## input single sample data as a vector
y <- c(23.8,26,26.9,27.4,28,30.3,30.7,35.9,36.1,36.4,36.6,37.2,37.3,37.9,31.2,31.3,32.8,33.2,33.9,34.3,34.9,35,38.2,39.6,40.6,41.1,42.3,42.8,44,45.8)

## exploratory data analysi9s
hist(y)

## don't like the number of bins

hist(y,8)


### check normality
## this will compare to any distribution
a <- rnorm(length(y))
qqplot(y,a) ## if the distribution of y and a is same it should be a 45 degree line

## specifically for normal distribution 
qqnorm(y)
## no deparatures from normality seen
## shapiro wilks test for normality(not very powerful)
## null hypothesis is data is normally distributed

shapiro.test(y)
## accept the null hypothesis that the population is normally distributed

## compute the mean and variance
mean(y)
var(y)

## 95 % confidence interval for the data
## assuming normality and large n
mean(y) + qnorm(.025)*sqrt(var(y)/length(y))
mean(y) - qnorm(.025)*sqrt(var(y)/length(y))

## 95% confidence interval for the mean and a small unknown variance
mean(y) + qt(.025,length(y)-1)*sqrt(var(y)/length(y))
mean(y) - qt(.025,length(y)-1)*sqrt(var(y)/length(y))

## bootstrap 95% confidence interval
## does not assume normality or sigma squared known, n can be large or small
b <- numeric(10000)
for(i in 1:10000) b[i] <- mean(sample(y,replace = T))
b <- sort(b)
b[250]
b[9750]

## test statistic and p value assuming large n
## H_0: mu <= 30
## H_1: mu > 30

## test statistic
z<-(mean(y)-30)/sqrt(var(y)/length(y))
z

## p value
p<- 1-pnorm(z)
p

## using one sided
t.test(y,alternative ="greater",mu=30 )

## two tailed test
t.test(y,alternative = "two.sided",mu = 30)

##wilcoxin test for mean=30
wilcox.test(y,mu=30)

## variance
## 95% confidence interval for variance assuming normality

n<- length(y)
(n-1)*var(y)/qchisq(.025,n-1)
(n-1)*var(y)/qchisq(.975,n-1)


## bootstrap 95% confidence interval
b1 <- numeric(10000)
for(i in 1:10000) b1[i] <- var(sample(y,replace = T))
b1 <- sort(b1)
b1[250]
b1[9750]


## hypothesis test for population variance
## H_0: sigma^2 <=30
## H_1: sigma^2 >30

pvalu <- 1-pchisq((n-1)*var(y)/30,n-1)
pvalu


### summary results
## population does not significantly differ from n0rmal distribution
## the sample mean and variance were .. and ... A 95% confidence interval for the mean is (..,..)
##A 95% confidence interval for the variance is (..,..)
## A hypothesis test for the mean equal to 30 vs not equal to 30 is rejected with a pvalue less than .0001
## A hypothesis test for the variance of less than or equal to 30 vs greater to 30 resulted in a p value of .424 indicating the null hpothesis can not be rejected

## In courseware under data there is a file const.xls
## 1. Compute the 95% ci for the mean of bonus and entitlement
##use only those available
## test the mean bonus is 30 or not
## test whether mean entitlement is <=1000 or >1000
## Get a 95% CI for variance of bonus
## write up results



