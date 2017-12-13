data <- read.csv("lunsford.csv")
job <- matrix(c(13,7,6,7,12,8,19,13,12,8,2,2),3,4)

chisq.test(job) 
fisher.test(table(data$Class,data$First))
install.packages("data.table")
library(data.table)
data <- fread("http://ww2.amstat.org/publications/jse/datasets/normtemp.dat.txt")

data$V1
male <- data$V1[which(data$V2==1)]
female <- data$V1[which(data$V2==2)]
var.test(male,female)
t.test(male,female,alternative = "two.sided",var.equal = F)
