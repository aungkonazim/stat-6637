install.packages("data.table")
library(data.table)
data <- fread("http://ww2.amstat.org/publications/jse/v21n2/ehrhardt/Drunkwalks.txt")
attach(data)
table(FIRST_STUMBLE,MATCH)
prop.test(table(FIRST_STUMBLE),correct = F)
prop.test(table(MATCH),correct = F)

table(SEX,MATCH)
prop.test(x=c(59,48),n=c(59+24,48+20),correct=F)
prop.test(x = 107,n=107+44,p=.5,correct=F)

library(tidyverse)
ggplot(data=data)+geom_bar(mapping=aes(x = FIRST_STUMBLE))
sum1 = 0
sum2 = 0
for (i in 1:length(DRINKS)){
  if (FIRST_STUMBLE[i] == 0){
    sum1 = sum1 + DRINKS[i]
  }
  if (FIRST_STUMBLE[i] == 1){
    sum2 = sum2 + DRINKS[i]
  }
  
}
  
e = c(sum1,sum2)
p = c(0,1)
ggplot() + geom_point(mapping=aes(x=p,y=e,color = p))

fall <- rep(1, times = length(unique(V1)))

uni_drink = unique(V1)






barplot(c(sum1,sum2),main="BAR plot showing total number of Drinks for first stumble in left vs right",names.arg=c("left","right"))

data <- fread("http://ww2.amstat.org/publications/jse/datasets/fev.dat.txt")
attach(data)
Sm<-data$V2[which(data$V5==1)]
Nsm <- data$V2[which(data$V5==0)]

t.test(Sm,Nsm,mu=0,alternative = "two.sided") ## default is not paired

var.test(Sm,Nsm,ratio = 1,alternative = "two.sided")
t.test(Sm,Nsm,alternative = "two.sided",var.equal = F)



m<-data$V2[which(data$V4==1)]
f<-data$V2[which(data$V4==0)]
var.test(m,f,ratio = 1,alternative = "two.sided")

t.test(m,f,alternative = "two.sided",var.equal = F)

x <- matrix(V2,V1)
barplot(table(V1,V2),col=c("lightblue", "mistyrose"),
        main = "Stacked Barplot showing No. of stumbles on either side vs No. of Drinks",
        legend = c("Non-Dominant","Dominant"),
        xlab = "No. Of Drinks")

data <- fread("http://ww2.amstat.org/publications/jse/datasets/fev.dat.txt")
attach(data)

data <- data.table(data)
boxplot(table(V2,V1))
age <- data[, mean(V2), by=V1]


age <- data.table(age)

age_e <- c()
meanfev <- c()

for (i in 1:length(unique(V1))){
  age_e =c(age_e,as.numeric(age[i,1]))
  meanfev =c(meanfev, as.numeric(age[i,2]))
}

ggplot() + geom_point(mapping=aes(x=age_e,y=meanfev))+ xlab("Age") +  ylab("Mean Forced Expiratory Volume")+
ggtitle("Plot showing mean forced expiratory volume for each age")

ggplot() + geom_boxplot(mapping = aes(x=V1(V1),y=V2))



