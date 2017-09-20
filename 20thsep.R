setwd("C:\\Users\\aungkon\\Desktop\\stat")
## multinomial 
library(MASS)
head(survey)
levels(survey$Smoke)
smoke.freq <- table(survey$Smoke)
## suppose it is known that the proportion of these categories on college campuses nation
## wide is 
## Heavy 4.5%
## Never 79.5%
## Occass 8.5%
## Regul 7.5%

## Test whether the proportions of the survey are the same as college campuses nationwide


smoke.prob <- c(.045,.795,.085,.075)

## to test our null hypo

chisq.test(smoke.freq,p=smoke.prob)
## cant reject the null hypothesis. conclude that the frequencies in survey are same as those on college 
## campuses nationwide

### test for independence
## in the survey data lets test whether smoking and exercise re independent

## make contingency table

tbl <- table(survey$Smoke,survey$Exer)
## 4 by 3 contingency table

## test whether smoking and exercise are independent

chisq.test(tbl)
## chi square is imappropriate  beacuase some cell counts r too low

## combine cells

ctbl <- cbind(tbl[,"Freq"],tbl[,"None"] +tbl[,"Some"] )

## 1 cell is still less than 5 but ignorable

chisq.test(ctbl)
### cant reject the null hypothesis so conclude smoking and exercise are independent in this survey

##If you dont have table data you can make a matrix

color <- matrix(c(68,119,26,7,20,84,17,94,15,54,14,10,5,29,14,16),nrow=4,byrow=T,
                dimnames=list(eye.color=c("Brown","Blue","Hazel","Green"),hair.color=c("Black","Brown","Red","Blonde")))

## test whther hair color and eye color are independent
chisq.test(color)


## Reject the null hypo and conclude that the hair color and eye color are not independent



##
O <- c(229,211,93,35,7,0,0,1)
n<- c(0,1,2,3,4,5,6,7)

## mean
mn <- sum(O*n)/sum(O)
## expected
E <- sum(O)*mn^n*exp(-mn)/factorial(n)
## 

O <- c(O[1:4],sum(O[5:8]))
E <- c(E[1:4],sum(E[5:8]))

## test statistic

T <- sum(((O-E)^2)/E)

pv <- 1- pchisq(T,3)
pv



## If all cells are small and we cant combine cells to get a large enough cell count = Use fishers exact test that looks at all possible table with fixed N

Job <- matrix(c(1,2,1,0,3,3,6,1,10,10,14,9,6,7,12,11),4,4,dimnames = list(income = c("<15k","15-25k","25-40k",">40k"),satisfaction = c("veryD","LittleD","ModerateD","VeryS")))

## is income level independent of job satisfaction

fisher.test(Job)
##p-value = 7827 so we cant rejet the hypothesis that income level independent of job satisfaction

### Are satisfaction levels the same for each income level?(Homogeniety)






















