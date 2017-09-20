setwd("C:\\Users\\aungkon\\Desktop\\stat\\assignment\\stat-6637-single-sample")
df <- read.csv("bonus.csv",header = T)
attach(df)
mean(Bonus)
hist(Bonus,8)
summary(Bonus)
boxplot(Bonus)+title("Boxplot of Bonus")

df <- read.csv("entitlement.csv",header = T)
attach(df)
mean(Entitlement)
hist(Entitlement,10)
summary(Entitlement)
boxplot(Entitlement)+title("Boxplot of Entitlement")

qqnorm(Bonus)

shapiro.test(Bonus)

b <- numeric(10000)
for(i in 1:10000) b[i] <- mean(sample(Entitlement,replace = T))
b <- sort(b)
b[250]
b[9750]

qqnorm(Entitlement)

shapiro.test(Entitlement)

t.test(Entitlement,alternative ="greater",mu=1000)
wilcox.test(Entitlement,alternative ="greater",mu=1000)
