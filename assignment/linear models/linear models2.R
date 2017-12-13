##the data is in file WEAR.DAT
## read in the data
data<-read.table("WEAR.DAT",header=FALSE)
## Four factors and 3 response variables
## 3 responses implies a multivariate analysis
## V1 = age group 1=young, 2=middle, 3=old
##V2 = gender 1=male, 2=female
## V3 = exercise level 1=yes, 2=no
## v4 = sleep 1 = 8hours or more , 2 = less than 8 hours
## V5 = level of good cholesterol
## V6 = level of bad cholesterol
## V7 = level of triglycerides

## model the three response vectors as functions of age, gender, exercise and sleep
## assign names to the variables
names(data)<-c("age","gender","exercise","sleep","good", "bad", "tri")
##multivariate with all covariates factors = MANOVA
##Are there differences in response means due to the factor gender?
##One-way MANOVA
##Are there differences in response meanse due to factors age and gender?
##Two way manova
is.factor(gender)
data$gender<-as.factor(data$gender)
fit<-manova(cbind(good,bad,tri)~gender,data=data)
summary(fit)
##there is a significant difference in at least one of the mean responses
fit1<-aov(good~gender,data=data)
summary(fit1)
#there is a significant difference in mean good cholesterol across gender
fit2<-aov(bad~gender,data=data)
summary(fit2)
## there is no significant difference in mean bad cholesterol across gender
fit3<-aov(tri~gender,data=data)
summary(fit3)
##no difference in mean triglycerides across gender
##no pairwise comparison for good since there is only two levels
##check if age is a factor
is.factor(data$age)
data$age<-as.factor(data$age)
fit<-manova(cbind(good,bad,tri)~age*gender,data=data)
summary(fit)
##age, gender interaction is not significant
fit<-manova(cbind(good,bad,tri)~age+gender,data=data)
summary(fit)
##so age does not have a significant effect on mean responses
## so we can leave that term out and we get the first model where
## mean good cholesterol is affected by gender but the other 
## mean responses are not





























