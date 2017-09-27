install.packages("data.table")
library(data.table)
data <- fread("http://ww2.amstat.org/publications/jse/v21n2/ehrhardt/Drunkwalks.txt")
attach(data)
table(FIRST_STUMBLE,MATCH)
prop.test(table(FIRST_STUMBLE),correct = F)
prop.test(table(MATCH),correct = F)

table(SEX,MATCH)
prop.test(x=c(59,48),n=c(59+24,48+20),correct=F)
prop.test(x = 107,n=107+44,p=.5,correct=F)

library(tidyverse)
ggplot(data=data)+geom_point(mapping=aes(x = DRINKS,y = FIRST_STUMBLE))
