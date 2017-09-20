## The effects of two types viruses on tobaco leaves was studied 
##by rubbing a preparation of each virus onto 8 tobacco leaves. The number of 
## lesions counted on each leaf for each virus is given here

v1 <- c(31,20,18,17,9,8,10,7)
v2 <- c(18,17,14,11,10,7,5,6)

## interest is whether there is a difference in the average number of lesions for the two viruses


### Paired design since each virus ius on each leaf

d <- v1-v2
## eda
qqnorm(d) ## qq plot
shapiro.test(d) ## test for normality
## cant reject the assumption of normality

mean(d)
var(d)
t.test(v1,v2,paired = T,mu=0,alternative = "two.sided")

## paired t test rejects the null hypothesis that the means are the same 
## what is the value of the paired test

cor(v1,v2)
## highly correlated

t.test(v1,v2,mu=0,alternative = "two.sided") ## default is not paired
## independent test does not reject the null hypothesis that the means are the same


### A blood bank kept a record of heartbeats per minute for blood donors.
## Test whether avg heartbeat is same for the men and women
## Test whether mean HB for men = mean HB for women

## not paired - independent samples
men <- c(58,76,82,74,79,65,74,86)
women <- c(66,74,69,76,72,73,75,67,68)
## EDA
##side by side boxplots
par(mfrow=c(1,2))
boxplot(men)
boxplot(women)

## check the qqplot for difference in distribution
par(mfrow=c(1,1))
qqplot(men,women)

## distributions look different (not 45 degree line)
## check normality
qqnorm(men) ## does not look normal
qqnorm(women) ## normal probably ok assumption

shapiro.test(men) ## cant reject normality p value = .62
shapiro.test(women) ## cant reject normality p value = .4592

mean(men)
mean(women)

var(men)
var(women)

## test for equal variances
var.test(men,women,ratio = 1,alternative = "two.sided")
## reject null hypothesis that variances are the same - conclude variances are unequal
## need to do welch's test

## non parametric test for equal variances
mood.test(men,women,alternative = "two.sided") ## agree with variance test

## welches t test for equal means
t.test(men,women,alternative = "two.sided",var.equal = F)
## cant reject the null hypothesis that the means are the same because p value is same

## t test assuming variances are equal
t.test(men,women,alternative = "two.sided",var.equal = T)


## man whitney non parametric test for non normal data
wilcox.test(men,women,alternative = "two.sided",exact = F)

### randomization test
x<-c(0,1,1,0,-2)
y<-c(6,7,7,4,-3,9,14)

n <- 5
m <- 7

## so there are 12 choose 5 diferent samples of size 5 that can be drawn from the combined sample

xy <- c(x,y)
T<-numeric(10000)
for (i in 1:10000)T[i] <- sum(sample(xy,size = length(x),replace = F))
Tobs <- sum(x) 
table(T)

## Tobs for the sample x was 0 , count the number of T's <= 0 = 125
## p-value = 125/10000
## so reject the null hypothesis

hist(T)


## assignments: read and study inferences on proportions

## 1. pollution levels were measured downstream and upstream from a plant. 
## On a given day  one measure was taken upstream and one was taken downstream. 16 days."streams.csv". Test
## whether there is a difference in the pollution level up and down the stream

## Yield in pounds of tomatoes per week for two gardens with different types of fertilizer are given in the file "t.test.data.csv".
## determine whether the average yields for the two gardens differ














































