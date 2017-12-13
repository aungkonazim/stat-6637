rm(list=ls())
setwd("C:\\Users\\aungkon\\Desktop\\stat\\assignment\\multipleregression\\data")
data<-read.csv("test.pollute.csv",head=T)
attach(data)
summary(data)
pairs(data)
full.lm<-lm(pollution~.,data=data)
summary(full.lm)

our.part<-lm(pollution~temp+industry+population+wind,data=data)
summary(our.part)

resid<- residuals(our.part)
qqnorm(resid)

plot(resid~temp)+title("Temp vs Residuals")
plot(resid~industry)+title("Industry vs Residuals")
plot(resid~population)+title("Population vs Residuals")
plot(resid~wind)+title("Wind vs Residuals")
plot(resid~pollution)+title("Polution vs Residuals")

our.part1<-lm(pollution~temp+industry+population+wind+I(wind^2)+I(temp^2),data=data)
summary(our.part1)
fitted<- predict(our.part)
plot(fitted)
plot(pollution,type="l",col="red")
lines(fitted,col="green")+title("Green = Predicted from our Model, Red = Original Pollution Value")



data<-read.csv("daphnia.csv",head=T)
attach(data)
summary(data)
pairs(data)
full.lm<-lm(Growth.rate~.,data=data)
summary(full.lm)

x1 <- ifelse(Daphnia=="Clone1",1,0)
x2 <- ifelse(Daphnia=="Clone2",1,0)
x3 <- ifelse(Daphnia=="Clone3",1,0)

our.part<-lm(Growth.rate~x3+x2)
summary(our.part)



resid<- residuals(our.part)
plot(resid~x2)+title("Daphia==Clone2 vs Residuals")
plot(resid~x3)+title("Daphia==Clone3 vs Residuals")

plot(resid~Growth.rate)+title("Growth rate vs Residuals")
qqnorm(resid)

fitted<- predict(our.part)

plot(Growth.rate,type="l",col="red")
lines(fitted,col="green")+title("Green = Predicted from our Model, Red = Original Growth rate Value")

rm(list=ls())

data<-read.csv("germination.csv",head=T)

attach(data)
summary(data)
pairs(data)
full.lm<-lm(count~.,data=data)
summary(full.lm)

x1 <- ifelse(Orobanche=="a73:10",1,0)

our.part<-lm(count~x1+sample)
summary(our.part)

resid<- residuals(full.lm)
plot(resid~x1)+title("Orobanche == a73:10 vs Residuals")
plot(resid~sample)+title("sample vs Residuals")

plot(resid~count)+title("count vs Residuals")
qqnorm(resid)
fitted<- predict(full.lm)

plot(count,type="l",col="red")
lines(fitted,col="green")+title("Green = Predicted from our Model, Red = Original Count Value")
