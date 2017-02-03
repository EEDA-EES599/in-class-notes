#In class script, Week 03 - class 1


#Booleans
5==6

answer=5==6

class(answer)

#other booleans
4>5
3<=3 

#what does NA mean?

x=NA
y=7
is.na(x)
is.na(y)

is.data.frame(x)
is.logical(answer)

5==5 & 5>6 & 5>4 #AND statements (all have to be TRUE to return TRUE)

5==5 | 5>6  #OR statements, if one or more is true, it returns TRUE

!is.na(x) #NOT statements, returns the opposite boolean given the expression
!5==5

file=download.file("https://www.dropbox.com/s/1vxlend4u6c1661/MonthlyGlobalTemperature.csv?dl=0", 
              destfile = "/tmp/test.csv", method = "curl")
data=read.csv("/tmp/test.csv")
year = data[,1] #grab all the rows in the first column
temperature = data[,-1] #grab all the temperatures 


year==1956 #vectors of 
length(year)
year>1900


year>1900 & year <1920
year>1900 | year <1920 #Everything is true

ioi=which(year>1900 & year <1920)
print(ioi)
year[ioi]

mean(temperature$Feb[ioi])


temperature$Feb < -.5#which Februaries were less than -.5?
coldFebruaries=which(temperature$Feb < -0.5) #find the indices that meet that criterion
print(coldFebruaries)
coldFebruaries
length(coldFebruaries)#how many indices met the criterion?

temperature$Feb[coldFebruaries] #show me those temperatures

year[coldFebruaries] #show me which years were cold

#Flow control.
#if else statement

if(5==6){
  print("It's TRUE!!!!")
}else{
  print("It's FALSE!!!")
}



t=0
#while loop
while(t<10){
t=t+1
print(t)
}

t=0
#while loop
while(t<10){
print(t)
t=t+1
}


#break
t=0
while(TRUE){
  t=t+1
  if(t==15){
    break
  }
}
print(t)



#FOR LOOPs
for(i in 3:length(temperature$Feb)){
print(temperature$Feb[i]+100)
}

NCOL(temperature)
meanMonthlyTemperature=c() #assign variable into memory
#better example
for(i in 1:NCOL(temperature)){
meanMonthlyTemperature[i] = mean(temperature[,i])
}

print(meanMonthlyTemperature)


#lets do it once more, this time assigning NAs into an appropriately sized matrix
meanMonthlyTemperature=matrix(NA,nrow=12,ncol=1)
print(meanMonthlyTemperature)

#better example
for(i in 1:NCOL(temperature)){
meanMonthlyTemperature[i] = mean(temperature[,i])
  if(i==6){
    break
  }
}

print(i)
print(meanMonthlyTemperature)


#Functional programming
#we want to create a function that simulates rolling dice and adding them together
#see if R already such a function
?dice
??dice

#first - create a new file
#we've creted dice.R

#first, load into memory
dice = function(nDice=2){
  #A function that simulates rolling any number dice, and then adds up the results
  #
  #inputs
  #nDice  - how many dice should we roll?
  #how this works
  #answer = dice(2)
  rolls=c()#assign a spot in memory for rolls
  
  for(i in 1:nDice){
    rolls[i]=sample(c(1,2,3,4,5,6),size=1)#randomly choose one side of a die
  }
  
  diceSum=sum(rolls)
  return(diceSum)
}

#test out how sample works
#?sample

#now lets test it.
dice(1)

#test with default inputs
dice()


#write a  in our dice guessing function
guessMyDice = function(guess,nDice=2){
  
  print(paste("your guess is",as.character(guess)))
  source("dice.R")
  roll=dice(nDice)
  
  if(guess==roll){
    print("congratulations!!! You're super smart!!!")
  }else{
    print("bummer. You're super dumb.")
  }
  return(roll)
}
guessMyDice(7)
guessMyDice(1)

guess=12
roll=1
counter=1
#I want to to be smart, at least once
while(guess!=roll){
roll=guessMyDice(guess)
counter=counter+1

}
print(counter)


























