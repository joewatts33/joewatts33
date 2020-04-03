#Chapter 4 problem 10 SIR model
#x[1]=susceptible, x[2]=infected, x[3]=recovered/immune
#population initially is 100,000
#disease is spread through contact 
#infectious period lasts approximately 3 weeks
#initially 18 cases, goes to 40 in one week.
#its estimated that 30% of the population is immune 
#assumptions: we can model the virus with SIR and x[1,2,3] above
#contact can be modeled by alpha*x[1]*x[2] and fit to the
#18 cases in week one and 40 cases in week two
#the infection period can be modeled with 20% recovery per week
#making te average recovery period 3 weeks, so beta=.2

#transform
alpha=.00002023
beta=.2
f=function(x){c(-alpha*x[1]*x[2],alpha*x[1]*x[2]*.7-beta*x[2],alpha*x[1]*x[2]*.3+beta*x[2])}
x0=c(100000-18,18,0)
norm2 = function(v){sqrt(v[1]^2+v[2]^2+v[3]^2)}
path = function(f,x0,deltat=0.01,N=1000,tol=1E-4){
  points=matrix(0,ncol=3)
  points[1,] = x0
  n = 0
  p = c(1,1,1)
  while(norm2(p)>tol & n<N){
    n=n+1
    p = f(x0)*deltat
    x0=x0+p
    points = rbind(points,x0)
  }
  
  rownames(points)=0:n
  return(points)
}
path(f,x0,deltat=1,N=50,tol=1)
#part a
#eventually 70,000 (susceptible nonimmune) individuals 
#will be infected because the model predicts that there 
#are 0 people left in the x[1] group

#part b
33470-19031
#the maximum new cases in a week is between week 9 and 10
#from 19031 to 33470 cases, so 14,439 cases in a week

#part c
#the largest assumption was the recovery rate of 20% per
#week, the other assumptions like cases start at 18, then 
#go to 40, and 30% of the population not being able to get
#sick due to immunity all stand.
#sensitivity of recovery rate (beta=% recovered per week)
beta=.33
f=function(x){c(-alpha*x[1]*x[2],alpha*x[1]*x[2]*.7-beta*x[2],alpha*x[1]*x[2]*.3+beta*x[2])}
x0=c(100000-18,18,0)
path(f,x0,deltat=1,N=50,tol=1)
#if they recover within 3 weeks, 247 susceptiple people dont get sick
beta=.5
f=function(x){c(-alpha*x[1]*x[2],alpha*x[1]*x[2]*.7-beta*x[2],alpha*x[1]*x[2]*.3+beta*x[2])}
x0=c(100000-18,18,0)
path(f,x0,deltat=1,N=50,tol=1)
#if they recover within 2 weeks, 3636  people do not get sick
beta=.1
f=function(x){c(-alpha*x[1]*x[2],alpha*x[1]*x[2]*.7-beta*x[2],alpha*x[1]*x[2]*.3+beta*x[2])}
x0=c(100000-18,18,0)
path(f,x0,deltat=1,N=90,tol=1)
#if they recover within 10 weeks (average 5 weeks) then it 
#takes much longer for the entire population to recover 
#and everyone gets sick

#part d
#if the 18 people originally sick was underreported
alpha=.00002023
beta=.2
f=function(x){c(-alpha*x[1]*x[2],alpha*x[1]*x[2]*.7-beta*x[2],alpha*x[1]*x[2]*.3+beta*x[2])}
x0=c(100000-30,30,0)
path(f,x0,deltat=1,N=60,tol=1)
#at 30 cases, the disease peaks sooner (week 11) and dies off at roughly the same time

alpha=.00002023
beta=.2
f=function(x){c(-alpha*x[1]*x[2],alpha*x[1]*x[2]*.7-beta*x[2],alpha*x[1]*x[2]*.3+beta*x[2])}
x0=c(100000-50,50,0)
path(f,x0,deltat=1,N=60,tol=1)
#at 50 cases, the disease peaks sooner (week 10) and dies off at roughly the same time

alpha=.00002023
beta=.2
f=function(x){c(-alpha*x[1]*x[2],alpha*x[1]*x[2]*.7-beta*x[2],alpha*x[1]*x[2]*.3+beta*x[2])}
x0=c(100000-100,100,0)
path(f,x0,deltat=1,N=60,tol=1)
#at 50 cases, the disease peaks sooner (week 9) and dies off at roughly the same time
#the virus is not very sensitve to initial case number