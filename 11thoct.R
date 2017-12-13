rm(list=ls())
setwd("C:\\Users\\aungkon\\Desktop\\stat")
data<-read.table("car.test.frame.txt",head=T)
attach(data)
summary(data)
pairs(data)
########
#Price is our response variable
#country is nominal
#reliability is ordinal
#mileage is numerical
#type is nominal
# weight, disp, hp are numerical
#Reliability has some NA in it and these records will be omitted 
#change NA to zeros
############

data$Reliability[is.na(data$Reliability)] <- 0
attach(data)


full.lm<-lm(Price~.,data=data)
summary(full.lm)

data$Country <- as.numeric(data$Country=="USA")

Little<-as.numeric(Type=="Small" | Type=="Compact")
Medium<-as.numeric(Type=="Medium")
Sporty<-as.numeric(Type=="Sporty")

data$Type<-Little
data<-cbind(data,Medium,Sporty)

pairs(data)
our.full<-lm(Price~.,data=data)
summary(our.full)

our.part<-lm(Price~Country+Weight+Medium+HP,data=data)
summary(our.part)


one<-c(rep(1,length(Price)))
X2<-cbind(one,data$Country,data$Weight,data$Medium,data$HP)
pairs(X2)
one<-c(rep(1,length(Price)))
X1<-cbind(one,data[,-Price])
pairs(X1)
data1 <- cbind(Price,X2[,-one])
pairs(data1)

cor(data)

anova(our.part,our.full)
## p value denotes if extra terms are needed

AIC(our.full)










