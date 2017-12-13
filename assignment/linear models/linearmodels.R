rm(list=ls())
setwd("C:\\Users\\aungkon\\Desktop\\stat\\assignment\\linear models")
data <- read.table("http://data.princeton.edu/wws509/datasets/housing.dat")
head(data)
names(data) <- c("proximity","contact","norm","favorable","unfavorable")
attach(data)
summary(data)
res.man<-glm(cbind(favorable,unfavorable)~as.factor(proximity)+as.factor(contact)+as.factor(norm),data=data,family=binomial)
summary(res.man)
1-pchisq(2.2378,4)
plot(res.man$residuals)
summary.aov(res.man)
pairs(data)
mlml<-lm(cbind(favorable,unfavorable)~as.factor(proximity)+as.factor(contact)+as.factor(norm),data=data)
summary(mlml)


data1 <- read.csv("https://www.sheffield.ac.uk/polopoly_fs/1.570199!/file/stcp-Rdataset-Diet.csv",header = T)
head(data1)
data1$weightlost <- data1$pre.weight - data1$weight6weeks
attach(data1)
data1$gender[is.na(data1$gender)] <- 2
library(Rmisc)
sum<-summarySE(data1,measurevar="weightlost",groupvars=c("Diet","gender"))
sum
aggregate(weightlost~Diet*gender,FUN=var)
ggplot(sum,aes(x=Diet,y=weightlost,color=gender))+
  geom_errorbar(aes(ymin=weightlost-se,ymax=weightlost+se),width=0.2,size=0.7,
                position=pd)+
  geom_point(shape=15,size=4,position=pd)+
  theme_bw()+
  theme(axis.title.y= element_text(vjust=1.8),
        axis.title.x=element_text(vjust=-0.5),
        axis.title = element_text(face="bold"))

boxplot(weightlost~as.factor(gender))
boxplot(weightlost~as.factor(Diet))
fit<-aov(weightlost~as.factor(Diet)*as.factor(gender))
##fit<-aov(len~supp+dose+supp:dose)
summary(fit) #or anova(fit)
interaction.plot(as.factor(Diet),as.factor(gender),weightlost)
fit1<-aov(weightlost~as.factor(gender))
##fit<-aov(len~supp+dose+supp:dose)
summary(fit1) #or anova(fit)

fit2<-aov(weightlost~as.factor(Diet))
##fit<-aov(len~supp+dose+supp:dose)
summary(fit2) #or anova(fit)
plot(TukeyHSD(fit2))
pairwise.t.test(weightlost,as.factor(Diet),p.adjust.method = "bonf")


data2 <- read.table("http://www.statsci.org/data/general/punting.txt",header = T)
head(data2)
attach(data2)
summary(data2)
pairs(data2)
qqnorm(Distance)
library("geepack")
fit.exch<-lm(Distance~Hang+R_Strength+L_Strength+R_Flexibility+L_Flexibility+O_Strength, data = data2)
summary(fit.exch)

fit.exch<-lm(Distance~Hang, data = data2)
summary(fit.exch)

fit.exch<-lm(Distance~R_Strength, data = data2)
summary(fit.exch)

fit.exch<-lm(Distance~L_Strength, data = data2)
summary(fit.exch)
fit.exch<-lm(Distance~L_Flexibility, data = data2)
summary(fit.exch)
fit.exch<-lm(Distance~R_Flexibility, data = data2)
summary(fit.exch)

fit.exch<-lm(Distance~O_Strength, data = data2)
summary(fit.exch)

fit.exch<-lm(Distance~Hang+R_Strength+L_Strength+R_Flexibility+O_Strength, data = data2)
summary(fit.exch)

plot(Distance,type="l",col="red")
lines(predict(fit.exch),col="green")+title("Green = Predicted from our Model, Red = Original Count Value")
