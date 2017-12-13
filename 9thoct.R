## this data is from a study of stripped crickets
## x - chirps/sec for the striped ground cricket
## y - temperature in degrees farenhit

## can temperature be used to predict the number of hirps 
setwd("C:\\Users\\aungkon\\Desktop\\stat")
data <- read.csv("slr02.csv",sep = "\t")
plot(data$Y,data$X)

qqnorm(data$X)
shapiro.test(data$X)

fit <- lm(data$X~data$Y)
summary(fit)
anova(fit)


plot(data$X,data$Y)
abline(fit$coeff)

fitted <- predict(fit)
resid <- residuals(fit)

qqnorm(resid)
plot(data$Y,resid)

##plot predicted vs residuals - non constant variance
plot(fitted,resid)
## no evidence of non constant variance

## we can explore the relationship between two variables by looking at the correlation between them

cor.test(data$Y,data$X)
## the null hypothesis of no correlation is rejected with estimated correlation of .8351438


attach(Puromycin)
head(Puromycin)
## Can concentration be used to predict rate?
##EDA
plot(conc,rate)
## there is a relationship but linear maybe not
qqnorm(rate)
## no problems
cor.test(conc,rate)
## reject null hypothesis of no correlation so we proceed with a linear model
fit <- lm(rate~conc)
summary(fit)
## we reject the null hypothesis that the slope is zero(wald test)
## r^2 = .6489

## plot residuals
fitted <- predict(fit)
resid <- residuals(fit) 

plot(conc,resid) ## this is the plot that says if higher order terms are needed
## need a quadratic term

fit2 <- lm(rate~conc + I(conc^2))
summary(fit2)

## both coeffs are conc and conc^2 are significantly different from 0 - both are useful in the model
## r^2 = .801 so keep both of these terms in the model

fitted2 <- predict(fit2)
resid2 <- residuals(fit2) 

qqnorm(resid2)
plot(conc,resid2) ## this is the plot that says if higher order terms are needed
## maybe a cubic term

## 
plot(fitted2,resid2)
## yucky different type of analysis?










## non quantitative variables  - dummy variables





## let X=1 if state is treated or 0 otherwise

X <- ifelse(state=="treated",1,0)
## to see whether there is a difference in treated and untreated group

plot(X,rate)
## there appears to be some effect of treated on rate

fit4 <- lm(rate~X)
summary(fit4)







fit3 <- lm(rate~conc + I(conc^2) + I(conc^3))
summary(fit3)

fitted3 <- predict(fit3)
resid3 <- residuals(fit3) 

qqnorm(resid3)
plot(conc,resid3)


fit4 <- lm(rate~conc + I(conc^2) + I(conc^3) + I(conc^4))
summary(fit4)




attach(warpbreaks)

head(warpbreaks)

X <- ifelse(wool=="A",1,0)

plot(X,breaks)

qqnorm(breaks)

cor.test(X,breaks)
fit <- lm(breaks~X)
summary(fit)
anova(fit)
fitted<- predict(fit)
resid<- residuals(fit) 

qqnorm(resid)
plot(X,resid)
plot(fitted,resid)

attach(trees)
head(trees)
plot(Height,sqrt(Volume))

qqnorm(sqrt(Volume))
shapiro.test(sqrt(Volume))


fit <- lm(sqrt(Volume)~Height)
summary(fit)
anova(fit)
fitted<- predict(fit)
resid<- residuals(fit) 

qqnorm(resid)
plot(Height,resid)
plot(fitted,resid)

fit <- lm(sqrt(Volume)~Height + I(Height^2))

summary(fit)


plot(Height,sqrt(Volume))
abline(fit)




