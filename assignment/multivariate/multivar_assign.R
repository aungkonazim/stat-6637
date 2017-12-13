rm(list=ls())
setwd("C:\\Users\\aungkon\\Desktop\\stat\\assignment\\multivariate")
data <- read.table("CHEM.dat")
names(data) <- c("IND","START","DESIRED","UNWANTED","TEMP","CONC","TIME")
attach(data)
summary(data)
data1<-data[c("START","DESIRED","UNWANTED","TEMP","CONC","TIME")] 
pairs(data1)
qqnorm(START)
qqnorm(DESIRED)
qqnorm(UNWANTED)
mlml<-lm(cbind(START,DESIRED,UNWANTED)~TEMP+CONC+TIME,data=data)
summary(mlml)
vcov(mlml)
anova(mlml)


mlml2<-lm(cbind(START,DESIRED,UNWANTED)~TEMP+CONC,data=data)
summary(mlml2)
vcov(mlml2)
anova(mlml2)

anova(mlml,mlml2)
plot(mlml$residuals)
qqnorm(mlml$residuals)
attach()
plot(mlml$residuals)
plot(CONC,mlml$residuals)

mlml3<-lm(cbind(START+DESIRED+UNWANTED)~TEMP+CONC+I(CONC^2)+I(TEMP^2),data=data)
summary(mlml3)

mlml4<-lm(cbind(START+DESIRED+UNWANTED)~TEMP+CONC+I(CONC^2),data=data)
summary(mlml4)
plot(mlml4$fitted,mlml4$residuals)

rm(list=ls())
setwd("C:\\Users\\aungkon\\Desktop\\stat\\assignment\\multivariate")
data <- read.table("FISH.dat")
names(data) <- c("METHOD","AROMA","FLAVOUR","TEXTURE","MOISTURE")
summary(data)
attach(data)
res.man<-manova(cbind(AROMA,FLAVOUR,TEXTURE,MOISTURE)~as.factor(METHOD),data=data)
summary(res.man)
summary.aov(res.man)

fit <-aov(FLAVOUR~as.factor(METHOD),data=data)
plot(TukeyHSD(fit))
pairwise.t.test(FLAVOUR,as.factor(METHOD),p.adjust.method = "bonf")
boxplot(FLAVOUR~as.factor(METHOD))


fit <-aov(TEXTURE~as.factor(METHOD),data=data)
plot(TukeyHSD(fit))
pairwise.t.test(TEXTURE,as.factor(METHOD),p.adjust.method = "bonf")
boxplot(FLAVOUR~as.factor(METHOD))

