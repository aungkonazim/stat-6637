attach(ToothGrowth)
head(ToothGrowth)
summary(ToothGrowth)
##we need both supp and dose to be factors in order to determine
##whether mean tooth len is related to supp and dose using ANOVA
is.factor(dose)
dose<-as.factor(dose)
is.factor(dose)
is.factor(supp)
###EDA
install.packages("Rmisc")
library(Rmisc)
sum<-summarySE(ToothGrowth,measurevar="len",groupvars=c("supp","dose"))
sum
##returns number of items in each cell, cell mean, and cell sd
aggregate(len~supp*dose,FUN=var)
#we can assume equal variance (not an order of magnitude apart)
##plot both factors together
library(ggplot2)
install.packages("ggplot2")
library(ggplot2)
pd<-position_dodge(.2)
ggplot(sum,aes(x=dose,y=len,color=supp))+
	geom_errorbar(aes(ymin=len-se,ymax=len+se),width=0.2,size=0.7,
		position=pd)+
	geom_point(shape=15,size=4,position=pd)+
	theme_bw()+
	theme(axis.title.y= element_text(vjust=1.8),
			axis.title.x=element_text(vjust=-0.5),
			axis.title = element_text(face="bold"))+
	scale_color_manual(values=c("black","blue"))
##looks like there is a difference in means at dose 2.0
#simple boxplots
boxplot(len~supp,data=ToothGrowth)
#potential difference between supp
boxplot(len~dose,data=ToothGrowth)
##mean looks different for dose levels
fit<-aov(len~supp*dose)
##fit<-aov(len~supp+dose+supp:dose)
summary(fit) #or anova(fit)
##interaction effect looks significant
interaction.plot(supp,dose,len)
#this plot shows that being in dose 2.0 negatives the effect of
#the supplement in that dose group - this is the significant interaction
#effect
dataOJ<-ToothGrowth[which(supp=="OJ"),]
dataVC<-ToothGrowth[which(supp=="VC"),]
##separates ToothGrowth by supp type
anova(aov(len~dose,dataOJ))
anova(aov(len~dose,dataVC))
##check normality
qqnorm(dataOJ$len)
qqnorm(dataVC$len)
data05<-ToothGrowth[which(dose=="0.5"),]
data10<-ToothGrowth[which(dose=="1"),]
data20<-ToothGrowth[which(dose=="2"),]
anova(aov(len~supp,data05))
anova(aov(len~supp,data10))
anova(aov(len~supp,data20))
##check normality
qqnorm(data05)
qqnorm(data05$len)
qqnorm(data10$len)
qqnorm(data20$len)
#Sothe supplement affect on tooth growth depends on the dose.
#At dose 0.5 and 1.0 there is a significant effect of supplement on length
#but at dose 2.0 there is no significant supplement effect.

##no significant interaction
head(mpg)
sapply(mpg,is.factor)
anova(aov(hwy~as.factor(manufacturer)*as.factor(trans),mpg))
##there is a difference in mean hwy mpg in manufacturer but not by trans
##and there was no significant interaction effect
##since there is a different in manufacturers, use multiple comparison
##to see which pairs differ
# pairwise.t.test(mpg$hwy,mpg$manufacturer)
##can also use Tukey and plot
###can have higher way ANOVAs
fit<-aov(hwy~as.factor(cyl)*as.factor(drv)*as.factor(fl),mpg)
interaction.plot(mpg$drv,mpg$fl,mpg$hwy)
interaction.plot(mpg$cyl,mpg$fl,mpg$hwy)

































