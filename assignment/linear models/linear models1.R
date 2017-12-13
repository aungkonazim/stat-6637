#1. In base R the dataset Orange is a set of measurements of age and trunk
#circumference with an index variable or which tree was measured.
##Model circumference as a function of age
##EDA
coplot(circumference~age|Tree,data=Orange,show.given=FALSE)
##there appears to be a linear relationship between age and circumference for
##each tree
qqnorm(Orange$circumference)
##normally distributed
aggregate(Orange$circumference~Orange$Tree,FUN=mean)
###using the fantastic flowchart, we determine to do a gee analysis
library(geepack)
fit<-geeglm(circumference~age,data=Orange,id=Tree,corstr="exchangeable")
summary(fit)
##The effect of age on circumference is significant with p-value <.0001. The
##estimated parameter in the model was 0.107 indicating that as age increases
##circumference also increases. circumference = a + b*age

##2. The data is
data<-read.csv("https://openmv.net/media/datasets/bioreactor-yields.csv")
##is a result of a randomized experiment on chemical yield.  The independent
##variables are temperature, duration,speed, baffles and the response is yield.
##Model yield as a linear function of the independent variables
## Observing the data, duration never changes and so will not have an
##impact on differing yield (don't put in the model)
##EDA
qqnorm(data$yield)
fit<-lm(yield~temperature*speed*baffles,data=data)
summary(fit)
##no parameters are significantly different from zero, yet the overall
##test for significant regression is significant, this could mean
##the model is overfit
fit1<-lm(yield~temperature+speed+baffles,data=data)
summary(fit1)
##now all main effects are significant - good model
##model diagnostics
plot(fit1$fitted.values,fit1$residuals)
##no evidence of non-constant variance
plot(data$temperature,fit1$residuals)
##no higher order temperature terms are needed
plot(data$speed,fit1$residuals)
##no higher order speed terms are needed - some suggestion of curves
qqnorm(fit1$residuals)
##all model diagnostics look good
##The fit model was Yield = 52 -.47 temperature +.008 speed -9.09 baffles
##All terms were significantly different from zero.  Yield decreases with
##increasing temperature, increases with increasing speed and is lower for 
##baffles= yes. No interaction terms were estimated because of the small
##number of data points.
plot(resid(fit1))###checks for time effect
3. 
data<-read.csv("spider.csv",fileEncoding="UTF-8-BOM")
##sand grain size on 28 beaches in Japan and the presence or absence of the 
##burrowing wolf spider.  Goal of this study is to determine whether there 
##is a relationship between sand grain size and presence or absence of spider
head(data)
##univariate, response is grain size and factor is presence or absence
##of spider (not necessarily biologically sound approach, unless they are
##in love with ANOVA!!)
is.factor(data$Spider)
##only way ANOVA so we don't have to check for balanced design
##EDA
boxplot(data$Grain.size~data$Spider) ##check for outliers and mean differences
y1<-data$Grain.size[which(data$Spider=="absent")]
y2<-data$Grain.size[which(data$Spider=="present")]
qqnorm(y1)
qqnorm(y2)
##check equal variances
var(y1)
var(y2)
##sample variances are similar enough to assume equal variances
fit<-aov(data$Grain.size~data$Spider)
summary(fit)
##p-value of 0.059 indicates possible difference in the means of the two
##groups
t.test(y1,y2)
##Welch's test assuming unequal variances rejects the null hypothesis
##that the means are equal so 1)the variances are too different 
##to do ANOVA and 2) the means are significantly different.  There is a
##relationship between grain size and presence of spiders.
## The burrowing spider is endangered so we would like to find an equation
##that predicts how likely we are to find a spider for a given grain size.
##response = presence/absence of spider, explanatory variable is grain size.
###logistic regression is a good method
fit<-glm(Spider~Grain.size,family=binomial,data=data)
summary(fit)
##to check if the model is adequate
1-pchisq(30.632,26)
##high p-value means model is adequate
##check predictive value - knowing grain size can I predict whether spiders
##are there
predict<-predict(fit,type="response")
table(data$Spider,predict>0.5)
##model has some predictive value
4. 



































































