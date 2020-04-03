library(pracma)

#Chapter 4 problem 5
#this problem uses a more complex model to predict the growth and the resulting population of blue
#and fin whales that accounts for a minimum viable population

#Transform step
#x[1]= blue whale population, x[2]=fin whale population
#model: g(P)=rP(P-c)(P+c)(1-P/K)-ax[1]x[2], P=populations x[1] and x[2]
#r = growth rate: .05 for blue .08 for fin
#c=minimum viable populations under which growth is negative: 3000 for blue 15000 for fin
#K= carrying capacity (max population): 150000 for blue 400000 for fin
#a=competition coefficient accounting for interactions between blue and fin whales, a=10^-8
#Assumptions
#blue whale viable populations are between 3000 and 150000
#fin whale viable populations are between 15000 and 400000
#growth rate is dependent only on population of the species minus competition with fin whales

#Solve / Interpret each answer
f = function(x){c(0.05*x[1]*(1-x[1]/150000)*((x[1]-3000)/(x[1]+3000))-10^(-8)*x[1]*x[2],
                  0.08*x[2]*(1-x[2]/400000)*((x[2]-15000)/(x[2]+15000))-10^(-8)*x[1]*x[2])}
#part a, can the two species of whale coexist?
x0=c(100000,300000)
print(zeros(f,x0))
#yes, the two species can coexist at 137698.4 blue whales and 392568.0 fin whales

#Part b sketch a vector field for this model and classify equilibrium points
ans=vectorfield(f,xlim=c(0,160000),ylim=c(0,410000),n=20,
                scale=0.15,col="red",main="Whale Population",xlab="BlueWhales",ylab="FinWhales")
x0=c(150000,400000)
print(zeros(f,x0))
#point at (137698.4,392568.0) is stable in every direction
x0=c(150000,10)
print(zeros(f,x0))
#point at (1.500000e+05,-7.531314e-20)=(150000,0) is unstable vertically
x0=c(10,400000)
print(zeros(f,x0))
#point at (-3.746668e-15,4.000000e+05)=(0,400000) is unstable horizontally
x0=c(1,1)
print(zeros(f,x0))
#point at (-6.353781e-23,-8.077936e-28)=(0,0) is unstable in every direction

#part c, population starts at (5000,70000)
x0=c(5000,70000)
print(zeros(f,x0))
#This model predicts that the populations will stabilize and remain at (3018.441,15011.765)

#part d new blue whale minimum viability (c) of 10000
f = function(x){c((0.05*x[1]*(1-(x[1]/150000))*((x[1]-10000)/(x[1]+10000)))-10^(-8)*x[1]*x[2],
                  0.08*x[2]*(1-(x[2]/400000))*((x[2]-15000)/(x[2]+15000))-10^(-8)*x[1]*x[2])}
ans=vectorfield(f,xlim=c(0,150000),ylim=c(0,400000),n=20,
                scale=0.15,col="red",main="Whale Population",xlab="BlueWhales",ylab="FinWhales")
x0=c(4000,70000)
print(zeros(f,x0))
#Now the populations adjust to (0,150000) which makes sense because the initial population is 
#below c, the minimum viable population mark for blue whales so their growth rate is negative
#and their population goes to zero
