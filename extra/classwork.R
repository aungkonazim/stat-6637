rm(list=ls())
setwd("C:\\Users\\aungkon\\Desktop\\stat\\extra")
data <- read.csv("corn.csv",header = T)
head(data)
qqnorm(data$yield)
is.factor(data$seed)
is.factor(data$fert)
model <- aov(yield~as.factor(seed)*fert,data=data)
summary(model)
pairwise.t.test(data$yield,data$fert)
pairwise.t.test(data$yield,as.factor(data$seed))

data(women)
head(women)
attach(women)
qqnorm(height)
qqnorm(weight)
cor.test(height,weight,alternative = "two.sided")

model <-lm(weight~height,data=women) 
summary(model)
qqnorm(model$residuals)
plot(model$residuals,height)

data<- read.csv("kuiper.csv",header = T,fileEncoding="UTF-8-BOM")
head(data)

qqnorm(data$Price)
shapiro.test(data$Price)
attach(data)


model <- lm(Price~Mileage+Make+Model+Doors+Leather,data=data)
summary(model)

qqnorm(model$residuals)
plot(model$residuals,data$Price)

plot(model$residuals,data$Mileage)
plot(model$residuals,data$Make)
plot(model$residuals,data$Model)
plot(model$residuals,data$Price)
plot(model$residuals,data$Price)
