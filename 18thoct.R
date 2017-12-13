rm(list=ls())
setwd("C:\\Users\\aungkon\\Desktop\\stat")
## dataset dealing with plasma retinal
## http://lib.stat.cmu.edu/datasets/Plasma_Retinol

library(data.table)
data <- fread("http://lib.stat.cmu.edu/datasets/Plasma_Retinol",skip = 15)

## we want to see if cholesterol levels V10 is the same for each smoking group V3

## smoking group V3 1 = Never, 2 = Former, 3 = Current Smoker

is.factor(data$V3)

## V3 is not a factor variable so we need to adjust either use as.factor(data$V3)

## data$V3 <- as.factor(data$V3)

### EDA

## two assumptions to check 1= normal, 2= equal variances 3. plot to see if means look different 4. look for outliers
boxplot(data$V10~data$V3)

## normality of cholesterol levels of never smoking group
qqnorm(data$V10[which(data$V3==1)])

## normality of cholesterol levels of former smoking group
qqnorm(data$V10[which(data$V3==2)])

## normality of cholesterol levels of current smoking group
qqnorm(data$V10[which(data$V3==3)])

## each of them are questionable

## variance assumption
var(data$V10[which(data$V3==1)])
var(data$V10[which(data$V3==2)])
var(data$V10[which(data$V3==3)])

## if one variance is 10 times than other one we need to give up on anova. we are good here with equal variance assumption

## var.test is too strict for here(highly sensitive to normality,t test is robust but f test is not)

fit <- aov(data$V10~as.factor(data$V3))
summary(fit)
## p value = .109 so null hypothesis of mean1 = mean2 = mean3 can not be rejected

## Since normality is questionable and we saw outliers we use a non parametric test ## kruskal-Wallis test based on ranks
kruskal.test(data$V10~as.factor(data$V3))

## testing the same null hypothesis in p-value = .059 so reject ?

## since the F test did not reject the null hypothesis we dont need to do multiple comparisons




## Example 2

data <- fread("http://www.itl.nist.gov/div898/education/anova/newcar.dat")
## V1 = interest rates V2  = cities
head(data)
is.factor(data$V2)

boxplot(data$V1~data$V2)
## means look different
int <- split(data$V1,as.factor(data$V2))
##normal plots
qqnorm(int[[1]])
qqnorm(int[[2]])
qqnorm(int[[3]])
qqnorm(int[[4]])
qqnorm(int[[5]])
qqnorm(int[[6]])
### normality assumption is okay but we did see outliers
##variance assumption

aggregate(data$V1~as.factor(data$V2),FUN = var)
##  no one variance is 10 times the other variances ok

fit1 <- aov(data$V1~as.factor(data$V2))
summary(fit1)
## we reject the null hypothesis that means are equal(so at least one mean is different)

kruskal.test(data$V1~as.factor(data$V2))
## indicates rejecttion so no assumptions were violated

## since means are different we want to do multiple comparisons to see which means differ

TukeyHSD(fit1)
## if p value is less than .05 then different

plot(TukeyHSD(fit1))


## other pairwise comparisons

pairwise.t.test(data$V1,as.factor(data$V2)) ## holm is default

## bonferoni
pairwise.t.test(data$V1,as.factor(data$V2),p.adjust.method = "bonf")
















