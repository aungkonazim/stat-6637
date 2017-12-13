#Start with multivariate multiple regression
#data involves patients who overdosed on the drug amitriptyline
#two responses
#TOT = total TCAD plasma level
#AMI = amount of amitriptyline in TCAD level
##covariates
#GEN=gender (male=0, female=1)
#AMT = amount of drug taken
#PR = PR wave measurement
#DIAP = diastolic blood pressure
#QRS = QRS wave measurement
#read in data and add names
ami_data<-read.table("http://static.lib.virginia.edu/statlab/materials/data/
ami_data.DAT")
names(ami_data) = c("TOT","AMI","GEN","AMT","PR","DIAP","QRS")
summary(ami_data)
pairs(ami_data)
##some relationships are observed - strong relationship between responses
qqnorm(ami_data$TOT)##looks like one outlier is messing up normality
qqnorm(ami_data$AMI)
##we will proceed with the analysis but in practice the outlier should
##be investigated
mlml<-lm(cbind(TOT,AMI)~GEN+AMT+PR+DIAP+QRS,data=ami_data)
summary(mlml)
ml<-lm(TOT~GEN+AMT+PR+DIAP+QRS,data=ami_data)
summary(ml)
##variance/covariance matrix of Bhat is joint for both responses
vcov(mlml)
##to test whether variables should be included in the model
anova(mlml)
##so in the joint model only AMT seems to be significant - in contrast
##to the individual models
###check diagnostics
head(resid(mlml))
head(fitted(mlml))
plot(fitted(mlml)[,1],resid(mlml)[,1])
###since amt was the only significant variable, let's try a model with
##just amt
mlml2<-update(mlml,.~.-GEN-PR-DIAP-QRS)
##Does the model with only AMT work better? i.e. are any other terms needed
anova(mlml,mlml2)
##if the p-value of this test is small, the 2nd model does not fit as well
## and needs more terms p=0.056 is on the borderline
##try another model by adding in the next most significan term
mlml3<-update(mlml,.~.-PR-DIAP-QRS)
anova(mlml3,mlml2)

###MANOVA
##we will use the iris data set in base R
##to look at a representative sample from iris
install.packages("dplyr")
library(dplyr)
set.seed(1234)
dplyr::sample_n(iris,10)
## we want to know if there is any significant difference in means
## in sepal and petal length between different species
res.man<-manova(cbind(Sepal.Length,Petal.Length)~Species,data=iris)
summary(res.man)
#reject H0 that all species mean sepal.Length and Petal.length are equal
#which response variable have different means
summary.aov(res.man)
##both responses differ
##to see which species have different means do multiple comparison
pairwise.t.test(iris$Sepal.Length,iris$Species)
##all Sepal.Length means are different from each other



























