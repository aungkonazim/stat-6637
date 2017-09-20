#when you want to print quality graphics use ggplot2

install.packages("tidyverse")
library(tidyverse)
?mpg

#to produce a scatter plot of displacement vs mpg

ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy))

#geom point tells ggplot what type of graph to produce
#mapping is always paired with aes()
# a template for creating the graph in ggplot is ggplot(data = <DATASET>)+<GEOM FUNCTION>(mapping=aes(<MAPPING>))

## add athird classification variable to a plot using aesthetics

## Color aesthetic
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy,color=class))


## size aesthetic
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy,size=class))


## transparency aesthetic
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy,alpha=class))

## color and transparency for manufacturer and class
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy,alpha=class,color=manufacturer))

## if you set aesthetic outside aes it affects the whole graph
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy),color="blue")

## geoms that we can use
## geom_bar gives a bar plot
## geom_line gives a line graph
## geom_boxplot gives a boxplot
## geom_point scatterplot


##best fit
ggplot(data=mpg)+geom_smooth(mapping = aes(x=displ,y=hwy))

#we can actually change the line typoes for diffn factor variables
ggplot(data=mpg)+geom_smooth(mapping = aes(x=displ,y=hwy,linetype=drv))

## we can add the original points by taking this original thing and adding the geom_point
ggplot(data=mpg)+geom_smooth(mapping = aes(x=displ,y=hwy,linetype=drv))+geom_point(mapping = aes(x=displ,y=hwy,color=drv))

## suppo0se we want a histogram 
ggplot(data=mpg) + geom_histogram(mapping=aes(x=hwy))

## barcharts
ggplot(data=diamonds) + geom_bar(mapping = aes(x=cut))
## add color
ggplot(data=diamonds) + geom_bar(mapping = aes(x=cut,color=cut))
# to fill color
ggplot(data=diamonds) + geom_bar(mapping = aes(x=cut,fill=cut))


##to fill with a different variable(stacked)
ggplot(data=diamonds) + geom_bar(mapping = aes(x=cut,fill=clarity))

##to put them side by side
ggplot(data=diamonds) + geom_bar(mapping = aes(x=cut,fill=clarity),position="dodge")


##to flip the barchart
ggplot(data=diamonds) + geom_bar(mapping = aes(x=cut,fill=clarity),position="dodge") + coord_flip()

# use mpg to
##1 make a scatterplot of hwy vs cyl color by year
##2 draw a horizontal barchart of manufacturer
##3. color barchart ihn 2 byb class
##4 explore and plot something cool

## set aspect ratio for plotting spatial data
install.packages("maps")

nz<-map_data("nz")
ggplot(nz,aes(long,lat,group=group))+ geom_polygon(fill="white",color="black")+coord_quickmap()

