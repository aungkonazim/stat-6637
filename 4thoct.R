install.packages("data.table")
library(data.table)
data <- fread("http://www.imperial.ac.uk/bio/research/crawley/statistics")
summary(data)
