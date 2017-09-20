data() #gives a list of data sets available in base R
head(esoph) #looks at the top 6 rows in the data
attach(esoph) #attaches the data set
dim(esoph) #gives the rows and columns
is.data.frame(esoph) #check for data frame
#what lom of variables
is.numeric(agegp)
is.numeric(ncases)
summary(esoph)
#agegp
tableage<-table(agegp)
barplot(tableage,main="Table of agegroups")
names(tableage)
#ncases
mean(ncases)
median(ncases)
var(ncases)
range(ncases)
sqrt(var(ncases))
sn<-sort(ncases)
length(ncases)
med<-(sn[44]+sn[45])/2
quantile(ncases,p=c(0.25,0.5,0.75))
#five number summary is 0,0,1,4,17
boxplot(ncases,ylab="number of cases")
#histogram
hist(ncases)
hist(ncases,freq=FALSE)
lines(density(ncases),lwd=3,col="blue")
Assignment 1: perform EDA on alcgp and ncontrols
detach(esoph)
###time series plot
plot(sunspot.month)
###multivariate
summary(swiss)
hist(swiss$Catholic)
hist(swiss$Education)
hist(swiss$Fertility)
boxplot(swiss)
pairs(swiss)
Assignment 2: do EDA on trees
##response and explanatory
attach(rock)
head(rock)
plot(area,peri)
plot(area,perm)
cor(area,perm)
cor(area,peri)
cor(rock)
Assignment 3: do EDA on stackloss
## to save a graph
















b




















