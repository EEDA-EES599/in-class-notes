#Earth and Environmental Data Analysis
#In class notes, week 1

#Section 1 - R is a calculator
4+7

#and we can store variables with clever names
almostAnythingYouWant = 4+7+11+9

#Once we have a variable, we can treat it like a number
newVariable=almostAnythingYouWant*3
#use print() to see what you have in memory
print(newVariable)

#print() is a function!

#In R, most functions have help.
?print


#lets load in some data
getwd()
#lets change my working directory to downloads folder
setwd("C:/Users/npm4/AppData/Local/Downloads")
#lets make sure it worked
getwd()

#lets load in some chick weight data!
chick.weights = read.csv("chickWeights.csv")

#you can use the RStudio Gui to see the data, or you can use head()
head(chick.weights)

#can I get a subset of my data out for analysis?
chick.weights[3,4]

#what if you want more than one datapoint?
chick.weights[3:4,4:5]

#so how did that colon work?
5:10

#what if you want more than one datapoint?
chick.weights[3:6,4:6]

#what if I want different spacing?
?seq

seq(1,100,by=7)
seq(from=1,to=100,by=7)
#maybe I just want 86 evenly spaced values between 214 and 697
seq(from=214,to=697,length.out = 86)

#use c() to create a vector
c(6,1,3)

#subset my data irregularly
chick.weights[c(6,1,3),2]

#What if I want all the data from a column? or row?
chick.weights[,5]


#lets explore our data.frame a bit more
#what kind of variable is chick.weights?
class(chick.weights)
#use colnames()
colnames(chick.weights)
#there can also be row names
rownames(chick.weights)

#you can also grab a column data by its name
chick.weights$meatmeal

#How much did the runt of the meatmeal chicks weigh?
min(chick.weights[,5])

#What was the fattest chick of them all?
max(chick.weights)


#what is the median for each different treatment (or column)
#let's turn our dataset into a matrix
chickMatrix = as.matrix(chick.weights)
colnames(chickMatrix)
median(chickMatrix)

#so you want to apply a function across some dimension of a dataset?
?apply

medians = apply(chick.weights,2,median)
print(medians)

#which feeding treatment resulted in the heaviest median chick weights?
#use the which
?which

index.out = which(medians == max(medians))
#if I run just the boolean
medians == max(medians)

print(index.out)

colnames(chickMatrix)[index.out]


#plotting
#lets plot the distribution of the weights of the chicks that ate sunflower
hist(chick.weights$sunflower)
?hist

#we need better labels
hist(chick.weights$sunflower,xlab = "Weight (g)",ylab = "Number of chicks",main = "Weight distribution among chicks in the sunflower treatment")


#lets learn a couple other quick types of plots
plot(chick.weights$horsebean,type="l")


#and one more
boxplot(chick.weights)

#there are two ways of assigning data in R

one = 1
two <- 2




































