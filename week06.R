#Class notes - Week 06

#lets use this trees dataset
head(trees)

#So what's the variance of Girth?
girth = trees$Girth
girthHat = mean(girth)

#so the variance =
girth.var=t(girth-girthHat)%*%(girth-girthHat) / (length(girth)-1)
var(girth)#or use the built in function in R

#lets convert that to standard deviation
girth.sd = sqrt(girth.var)
print(girth.sd)
sd(girth)

#let's examine a relationship between two variables
library(ggplot2)
ggplot(trees)+geom_point(aes(x=Height,y=Volume))

#calculate the covariance of height and volume
height.anom = trees$Height - mean(trees$Height)
vol.anom = trees$Volume - mean(trees$Volume)

#Now use your matrix skills
covHV = t(height.anom)%*%vol.anom / (length(vol.anom)-1)
print(covHV)


#now lets compare to girth vs volume
girth.anom = trees$Girth-mean(trees$Girth)

covGV = t(girth.anom)%*%vol.anom / (length(vol.anom)-1)
print(covGV)
#check that I did it right
cov(trees$Girth,trees$Volume)


#let's calculate the correlation here, instead of covariance
corHV = covHV/sqrt(var(trees$Height)*var(trees$Volume))
print(corHV)
#lets check ourself in R
cor(trees$Height,trees$Volume)

corGV = covGV/sqrt(var(trees$Girth)*var(trees$Volume))
print(corGV)
#what fraction of total variance does the girth volume relationship explain?
print(corGV^2)
ggplot(trees)+geom_point(aes(x=Girth,y=Volume))


#let's generate a bunch of random data, correlate them, and see what we get
r=c()
for(i in 1:1000){
  rd1=rnorm(100)
  rd2=rnorm(100)
  r[i] = cor(rd1,rd2)
}

#lets look at the data
ggplot()+geom_histogram(aes(x=r,y=..density..))


#Beaver body temperature
ggplot(beaver1)+geom_histogram(aes(x=temp,y=..density..))+geom_vline(xintercept = 37,color="red")

#let's convert our data to a t-stat value
tmean = (beaver1$temp-mean(beaver1$temp))/(sd(beaver1$temp)/sqrt(length(beaver1$temp)))
t37 = (37-mean(beaver1$temp))/(sd(beaver1$temp)/sqrt(length(beaver1$temp)))
#Beaver body temperature
myplot = ggplot()+geom_histogram(aes(x=tmean,y=..density..))+geom_vline(xintercept = t37,color="red")

td = dt(seq(-3,3,by=.1),df=length(beaver1$temp)-2)
ndf=data.frame(x=seq(-3,3,by=.1),y=td)

myplot+geom_area(data=ndf,aes(x=x,y=y),fill="red",alpha=0.5)
#so to actuall do the p-value calculation, you use your T-statistic which is t37 here.
print(t37)
#we'll use the cumulative probability distribution for t to test this
tcum = pt(seq(-7,7,length.out=100),df=length(beaver1$temp)-2)
plot(seq(-7,7,length.out=100),tcum)
#we can calculate the cumulative probability that the value is less than our tstat like this:
pt(t37,df=length(beaver1$temp)-2)

#What about a different critical value
t36.84= (36.84-mean(beaver1$temp))/(sd(beaver1$temp)/sqrt(length(beaver1$temp)))
pt(t36.84,df=length(beaver1$temp)-2)

ggplot()+geom_histogram(aes(x=tmean,y=..density..))+geom_vline(xintercept = t36.84,color="blue")+geom_area(data=ndf,aes(x=x,y=y),fill="red",alpha=0.5)

#OK. What if we want to know the probability that our mean is more than 0.03 degrees different than our calculated mean.

observedMean= mean(beaver1$temp)
tstatHi = 0.03/(sd(beaver1$temp)/sqrt(length(beaver1$temp)))
tstatlo = -0.03/(sd(beaver1$temp)/sqrt(length(beaver1$temp)))

x1 = seq(-4,4,length.out = 50)
d1 = dt(x1,df=length(beaver1)-2)
x2 = seq(tstatHi,4,length.out = 50)
d2 = dt(x2,df=length(beaver1)-2)
x3 = seq(-4, tstatlo,length.out = 50)
d3= dt(x3,df=length(beaver1)-2)

plotdf = data.frame(x1,x2,x3,d1,d2,d3)
ggplot(plotdf)+geom_area(aes(x1,d1),colour = "black") +
  geom_area(aes(x2,d2),fill = "red",alpha = 0.5) + 
  geom_area(aes(x3,d3),fill = "red",alpha = 0.5)

#lets use the cumulative probability function to get the area for the left tail
pt(tstatlo,df = length(beaver1$temp)-2)
#what if we did this for tstatHi?
pt(tstatHi,df = length(beaver1$temp)-2)
x=tstatHi
#whats the total value
pValue = pt(-abs(x),df =length(beaver1$temp)-2)*2
print(pValue)

#Monte Carlo methods
#Let's simulate "random" beaver temperatures 1000 times, and see how much our mean varies.

bmean = mean(beaver1$temp)
bsd = sd(beaver1$temp)
nits = 1000
simBeaverMean = matrix(NA,nits,1)
for(i in 1:nits){
  simBeaver = rnorm(n = length(beaver1$temp),mean=bmean,sd=bsd)
  simBeaverMean[i] = mean(simBeaver)
}

#So now we have 1000 possible means from our dataset
#let's take a look
hiTemp = bmean+0.03
loTemp = bmean-0.03
ggplot()+geom_histogram(aes(x=simBeaverMean))+geom_vline(aes(xintercept=loTemp))+
  geom_vline(aes(xintercept = hiTemp))

#lets use our simulated results to calculate a 95% confidence interval on our mean
quantile(simBeaverMean,c(0.025, 0.975))

#what about figuring out percentiles, given a certain critical value?
beaverCDF = ecdf(simBeaverMean)
ourProbs = beaverCDF(c(loTemp,hiTemp))
print(ourProbs)
totalExtremeProb = ourProbs[1]+(1-ourProbs[2])
print(totalExtremeProb)


#One more monte carlo method.
#This is called bootstrapping.


nits = 1000
simBeaverMean = matrix(NA,nits,1)
for(i in 1:nits){
  simBeaver = sample(beaver1$temp,size = length(beaver1$temp),replace = TRUE)
  simBeaverMean[i] = mean(simBeaver)
}

#So now we have 1000 possible means from our dataset
#let's take a look
hiTemp = bmean+0.03
loTemp = bmean-0.03
ggplot()+geom_histogram(aes(x=simBeaverMean))+geom_vline(aes(xintercept=loTemp))+
  geom_vline(aes(xintercept = hiTemp))

#lets use our simulated results to calculate a 95% confidence interval on our mean
quantile(simBeaverMean,c(0.025, 0.975))

#what about figuring out percentiles, given a certain critical value?
beaverCDF = ecdf(simBeaverMean)
ourProbs = beaverCDF(c(loTemp,hiTemp))
print(ourProbs)
totalExtremeProb = ourProbs[1]+(1-ourProbs[2])
print(totalExtremeProb)

#One last example: Jackknifing
simBeaverMean = matrix(NA,length(beaver1$temp),1)
for(i in 1:length(beaver1$temp)){
  simBeaver = beaver1$temp[-i]
  simBeaverMean[i] = mean(simBeaver)
}



#So now we have 1000 possible means from our dataset
#let's take a look
hiTemp = bmean+0.03
loTemp = bmean-0.03
ggplot()+geom_histogram(aes(x=simBeaverMean))+geom_vline(aes(xintercept=loTemp))+
  geom_vline(aes(xintercept = hiTemp))

#lets use our simulated results to calculate a 95% confidence interval on our mean
quantile(simBeaverMean,c(0.025, 0.975))

#what about figuring out percentiles, given a certain critical value?
beaverCDF = ecdf(simBeaverMean)
ourProbs = beaverCDF(c(loTemp,hiTemp))
print(ourProbs)
totalExtremeProb = ourProbs[1]+(1-ourProbs[2])
print(totalExtremeProb)










