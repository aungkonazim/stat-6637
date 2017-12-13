rm(list=ls())
setwd("C:\\Users\\aungkon\\Desktop\\stat\\assignment\\two way anova")
data <- read.table("data.txt",sep = "\t",header = T)
attach(data)
is.factor(location)
is.factor(factor)
library(Rmisc)
sum<-summarySE(data,measurevar="count",groupvars=c("location","factor"))
sum
aggregate(count~location*factor,FUN=var)
library(ggplot2)
pd<-position_dodge(.2)
ggplot(sum,aes(x=location,y=count,color=factor))+
  geom_errorbar(aes(ymin=count-se,ymax=count+se),width=0.2,size=0.7,
                position=pd)+
  geom_point(shape=15,size=4,position=pd)+
  theme_bw()+
  theme(axis.title.y= element_text(vjust=1.8),
        axis.title.x=element_text(vjust=-0.5),
        axis.title = element_text(face="bold"))+
  scale_color_manual(values=c("black","blue","red"))
boxplot(count~location,data=data)
#potential difference between supp
boxplot(count~factor,data=data)
fit<-aov(count~factor*location)
summary(fit)
interaction.plot(factor,location,count)
plot(TukeyHSD(fit))

fit1<-aov(count~location)
plot(TukeyHSD(fit1))
pairwise.t.test(count,location,p.adjust.method = "bonf")

fit2<-aov(count~factor)
plot(TukeyHSD(fit2))
pairwise.t.test(count,factor,p.adjust.method = "bonf")

rm(list=ls())
library(ggplot2)
head(diamonds)
attach(diamonds)
depth[is.na(depth)] <- 0
is.factor(cut)
is.factor(clarity)
is.factor(color)
library(Rmisc)
sum<-summarySE(diamonds,measurevar="depth",groupvars=c("cut","clarity","color"))
sum
aggregate(depth~cut*clarity*color,FUN=var)

boxplot(depth~cut,data=diamonds)
boxplot(depth~clarity,data=diamonds)
boxplot(depth~color,data=diamonds)
fit<-aov(depth~cut*color*clarity,diamonds)
summary(fit)
interaction.plot(cut,clarity,depth)
interaction.plot(color,clarity,depth)
interaction.plot(cut,color,depth)

sum<-summarySE(diamonds,measurevar="depth",groupvars=c("color","cut"))
library(ggplot2)
pd<-position_dodge(.2)
ggplot(sum,aes(x=color,y=depth,color=cut))+
  geom_errorbar(aes(ymin=depth-se,ymax=depth+se),width=0.2,size=0.7,
                position=pd)+
  geom_point(shape=15,size=4,position=pd)+
  theme_bw()+
  theme(axis.title.y= element_text(vjust=1.8),
        axis.title.x=element_text(vjust=-0.5),
        axis.title = element_text(face="bold"))+
  scale_color_manual(values=c("black","blue","red","yellow","green","violet","pink","lightblue"))
