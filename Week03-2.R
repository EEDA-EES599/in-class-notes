#Week 3, part 2
#ggplot

#GGplot is an external package, so we need to install it first
install.packages("ggplot2")

#to use a package
library(ggplot2)


## A bit about ggplot's philosophy

#Grammar of graphics

#1. define the dataset that you want to visualize. In ggplot, this almost
#always works best with a data.frame

#2. Describe the relationship that you want to visualize.

#3. Define how we want to visualize all of this

#iris is a built in dataset
?iris
head(iris)


#lets make sure it's a dataframe
class(iris)
is.data.frame(iris)

names(iris)#get the headers
colnames(iris)

#First, lets make a simple scatterplot

ggplot(data=iris, aes(x=Sepal.Length,y=Petal.Length)) +
  geom_point()

#lets visualize (step 3) differently

ggplot(data=iris, aes(x=Sepal.Length,y=Petal.Length)) +
  geom_point() + 
  geom_line()

#let's colour the dots, based on species
ggplot(data=iris, aes(x=Sepal.Length,y=Petal.Length,colour = Species)) +
  geom_point()

#what is aes?
?aes

#what if we want all the data to be pink
ggplot(data=iris, aes(x=Sepal.Length,y=Petal.Length)) +
  geom_point(colour="chartreuse")

#what if we want the shape to be data-dependent
ggplot(data=iris, aes(x=Sepal.Length,y=Petal.Length,shape = Species)) +
  geom_point(colour="chartreuse",size = 8)

#make the same variable change multiple features
ggplot(data=iris, aes(x=Sepal.Length,y=Petal.Length,shape = Species,colour=Species,size=Species)) +
  geom_point()

#you can store ggplots as variables

MyPlot =  ggplot(data=iris, aes(x=Sepal.Length,y=Petal.Length,shape = Species,colour=Species,size=Species)) +
  geom_point()

print(MyPlot)
#MyPlot = MyPlot + geom_line()
print(MyPlot)

#so you want to make an image of your ggplot, use ggsave()
ggsave(filename = "myplot.pdf",MyPlot)


#lets explore some more features
?geom_smooth
MyPlot+geom_smooth(method = "lm")

#how can we do one regression for all the data
MyPlot =  ggplot(data=iris, aes(x=Sepal.Length,y=Petal.Length)) 

MyPlot+geom_point()+geom_smooth(method = "lm")


#more plotting options - my favorite kind of graph
ggplot(data=iris,aes(x=Sepal.Width))+geom_histogram()

#what if you'd rather show probability density than count
ggplot(data=iris,aes(x=Sepal.Width,y=..density..))+geom_histogram()


#there are a lot of options, use help
?geom_histogram
ggplot(data=iris,aes(x=Sepal.Width,y=..density..))+geom_histogram(binwidth=.1)

#one more example on histogram
ggplot(data=iris,aes(x=Sepal.Width,y=..density..))+
  geom_histogram(binwidth=.1)+
  labs(x="Sepal Width (cm)",title = "Fancy plot")+
  theme_bw()



#faceting
#makes multiple plots intelligently
ggplot(data=iris, aes(x=Sepal.Length,y=Petal.Length))+geom_point() 
#lets make multiple plots, by species
ggplot(data=iris, aes(x=Sepal.Length,y=Petal.Length))+
  geom_point()+
  facet_grid(Species ~ .)


#lets check out another dataset
library(reshape2)
?tips
head(tips)


plot = ggplot(data = tips, aes(x = total_bill, y = tip/total_bill))+
  geom_point()
print(plot)

plot+facet_grid(sex ~ day)

#look at just one, but present as a grid
plot+facet_wrap( ~ day)

#can we reorder days
#ggplot thinks in factors
factor(tips$day,levels = c(4,1,2,3))
?order

#how do you put each axis on it's own scale
plot+
  facet_grid(sex ~ day,scales = "free_y")

#reorder data frame by days
factor(tips$day, levels=c("Thur","Fri","Sat","Sun"))

#so to get ggplot to plot in a different order, you have to modify your dataset
newtips = tips
newtips$day = factor(tips$day, levels=c("Thur","Fri","Sat","Sun"))

ggplot(data = newtips, aes(x = total_bill, y = tip/total_bill))+
  geom_point()+facet_wrap( ~ day)



#let's make a simple bar plot.

bardata=data.frame(barHeight=c(3,7,5),labels=c("one","two","three"),blo=c(2,6.5,3),bhi=c(4,7.41,5.5))
#make a barplot

ggplot(bardata,aes(x=barHeight))+geom_bar()
ggplot(bardata,aes(x=labels,y=barHeight))+geom_bar(stat='identity',fill="magenta")


#repeat with numeric labels
bardata=data.frame(barHeight=c(3,7,5),labels=c(1,2,3),blo=c(2,6.5,3),bhi=c(4,7.41,5.5))
ggplot(bardata,aes(x=labels,y=barHeight))+geom_bar(stat='identity',fill="magenta")

#add error bars
ggplot(bardata,aes(x=labels,y=barHeight))+
  geom_bar(stat='identity',fill="magenta")+
  geom_errorbar(aes(ymin=blo,ymax=bhi))

#add static error bars
ggplot(bardata,aes(x=labels,y=barHeight))+
  geom_bar(stat='identity',fill="magenta")+
  geom_errorbar(ymin=1,ymax=7.5)







