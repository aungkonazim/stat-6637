pdf("1.pdf")
ggplot(data=mpg)+geom_point(mapping=aes(x=hwy,y=cyl,color=year))
dev.off()

install.packages("tidyverse")
library(tidyverse)
pdf("1.pdf")

ggplot(data=mpg)+ geom_point(mapping=aes(x=hwy,y=cyl,color=year)) + xlab("Highway Mileage") +  ylab("Number of Cylinders") + 
  ggtitle("Scatter plot of Highway Mileage vs Number of Cylinders(colored by years)")

ggplot(data=mpg) + geom_bar(mapping = aes(x=manufacturer),position="dodge") + xlab("Manufacturer") +  ylab("Number of Cars")+ 
  ggtitle("Bar plot of Number of Cars by Manufacturer")+coord_flip()

ggplot(data=mpg) + geom_bar(mapping = aes(x=manufacturer,fill=class)) + xlab("Manufacturer") +  ylab("Number of Cars") +
  ggtitle("Stacked Barplpot of Number of Cars of different class by Manufacturer")+ coord_flip()

ggplot(data = mpg)+geom_boxplot(aes(x=factor(manufacturer), y=hwy))+xlab("Manufacturer") +  ylab("Highway Mileage") +
  ggtitle("Boxplot of Highway Mileage for each Manufacturer")

dev.off()
dev.off()