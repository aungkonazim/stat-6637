setwd("C:\\Users\\aungkon\\Desktop\\stat\\assignment\\one way anova")
head(airquality)
attach(airquality)
is.factor(Month)
is.factor(Ozone)
boxplot(Ozone~Month)
Ozone[is.na(Ozone)] <- 0
qqnorm(Ozone[which(Month==6)])
qqnorm(Ozone[which(Month==7)])
qqnorm(Ozone[which(Month==8)])
qqnorm(Ozone[which(Month==9)])

var(Ozone[which(Month==5)])
var(Ozone[which(Month==6)])
var(Ozone[which(Month==7)])
var(Ozone[which(Month==8)])
var(Ozone[which(Month==9)])

fit <- aov(Ozone~as.factor(Month))
summary(fit)
kruskal.test(Ozone~as.factor(Month))

TukeyHSD(fit)
plot(TukeyHSD(fit))
pairwise.t.test(Ozone,as.factor(Month),p.adjust.method = "bonf")

is.factor(Solar.R)
is.factor(Month)
boxplot(Solar.R~Month)
Solar.R[is.na(Solar.R)] <- 0
qqnorm(Solar.R[which(Month==5)])
qqnorm(Solar.R[which(Month==6)])
qqnorm(Solar.R[which(Month==7)])
qqnorm(Solar.R[which(Month==8)])
qqnorm(Solar.R[which(Month==9)])

var(Solar.R[which(Month==5)])
var(Solar.R[which(Month==6)])
var(Solar.R[which(Month==7)])
var(Solar.R[which(Month==8)])

fit <- aov(Solar.R~as.factor(Month))
summary(fit)
kruskal.test(Solar.R~as.factor(Month))

TukeyHSD(fit)
plot(TukeyHSD(fit))
pairwise.t.test(Solar.R,as.factor(Month),p.adjust.method = "bonf")

is.factor(Wind)
is.factor(Month)
boxplot(Wind~Month)
Wind[is.na(Wind)] <- 0
qqnorm(Wind[which(Month==5)])
qqnorm(Wind[which(Month==6)])
qqnorm(Wind[which(Month==7)])
qqnorm(Wind[which(Month==8)])
qqnorm(Wind[which(Month==9)])

var(Wind[which(Month==5)])
var(Wind[which(Month==6)])
var(Wind[which(Month==7)])
var(Wind[which(Month==8)])
var(Wind[which(Month==9)])


fit <- aov(Wind~as.factor(Month))
summary(fit)
kruskal.test(Solar.R~as.factor(Month))

TukeyHSD(fit)
plot(TukeyHSD(fit))
pairwise.t.test(Wind,as.factor(Month),p.adjust.method = "bonf")


