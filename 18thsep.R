setwd("C:\\Users\\aungkon\\Desktop\\stat")
## out of 29 males samples 9 said that they snored.out of 31 females 4 said they snored.
# are the proportions of males and females that snore different?

prop.test(x=c(9,4),n=c(29,31),correct=F)
# p-value = .08846 conclude that there is no significant difference
# if the confidence interval contains 0 then we can not reject the 
# null hypothesis that the population proportions are the same
## prop.test uses normal apporoximation

#Data set: quine-survey info from an australian town classified by ....


library(MASS)
head(quine)

## interested in difference between the female population of aboriginal students (ETH=A) and 
# female proportions of non aboriginal students

table(quine$Eth,quine$Sex)

prop.test(table(quine$Eth,quine$Sex),correct=F)
## p=.9491 cant reject the null hypothesis

##Example while imprisoned during WW2 English mathematician John Kerrick tossed a coin 10000 times and got 5067 heads
## is this experiment consistent with the hypothesis that the coin is fair?

## Here we are testing one sample whether p=.5

binom.test(x=5067,n=10000,p=.5)

## p-value =.1835
## can not reject the null hypothesis that p =.5

prop.test(x=5067,n=10000,p=.5,correct=F)

## Incidence of olds among 279 French skiers that were randomized to take either viatamin c or a placebo was found to be

## Vitamin C : 17 colds out of 139
## Pacebo : 31 out of 140

#Does Vitamin C tend to reduce the number of cold?


prop.test(x=c(17,31),n=c(139,140),correct=F)


## p value = .02827 Reejct Null Hypothesis that the proportions are same



## Fischers exact test: Look at all possible 2*2 matrices ...

fisher.test(matrix(c(17,139-17,31,141-31),ncol=2))


#Assignment: Read and study Count Data.
## Data from an article by Robert Ruteledge in annals of surgery in 1993


#no seat belt-survived = 1781, Died = 135
#seat belt - survived = 1443, Died =47
## whether wearing seat belt has any effect?


## compare the two proportions and actual tests for difference in 2 proportions

#placebo - Fatal 18- Non Fatal 171 - No Attack 10845
#aspirin - Fatal 5 - Non fatal 99 - No Attack 10993
##    a. look at only the groups that had an attack
##    b. Look at attacks of any type versus no attack

























