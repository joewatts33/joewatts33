path = function(f,x0,deltat=0.01,N=1000,tol=1E-4){
  len = length(x0)
  points=matrix(0,ncol=len)
  points[1,] = x0
  n = 0
  p = c(1,1)
  while(Norm(p)>tol & n<N){
    n=n+1
    p = f(x0)*deltat
    x0=x0+p
    points = rbind(points,x0)
  }
  
  rownames(points)=0:n
  return(points)
}
#Chapter 6 Problem 1
#effect of weather on Red and Blue armies
#x[1]=red forces, x[2]=blue forces
#loss of red forces from direct fire=a*x[2]
#loss of blue forces from direct fire=b*x[1]
#loss of red from IDF=c*x[1]*x[2]
#loss of blue from IDF=d*x[1]*x[2]
#x0=c(5,2), red forces 2.5 times larger than blue. 
#blue army more effective than red: a>b, c>d.
#blue effectiveness is modeled by lambda*b=a, lambda*d=c
#part a states blue army is 3x more effective (lambda=3)
#weather degrades effectiveness of direct fires, 0<w<1

#part a 
#given lambda=3 and w=.25 (75% decrease in effectiveness of direct fire)
w=.25
lambda=3
b=.05
d=.005
wmodel=function(x){c(-w*lambda*b*x[2]-lambda*d*x[1]*x[2],-w*b*x[1]-d*x[1]*x[2])}
x0=c(5,2)
path(wmodel,x0,N=50,deltat=1)
#this model predicts that red army will win after 38 days of fighting with 2.23 divisions left

#part b 
w=.1
wmodel=function(x){c(-w*lambda*b*x[2]-lambda*d*x[1]*x[2],-w*b*x[1]-d*x[1]*x[2])}
path(wmodel,x0,N=105,deltat=1)
#red wins after 105 days, have 1.34 divisions left
w=.2
wmodel=function(x){c(-w*lambda*b*x[2]-lambda*d*x[1]*x[2],-w*b*x[1]-d*x[1]*x[2])}
path(wmodel,x0,N=50,deltat=1)
#red wins after 49 days, have 2.034 divisions left
w=.5
wmodel=function(x){c(-w*lambda*b*x[2]-lambda*d*x[1]*x[2],-w*b*x[1]-d*x[1]*x[2])}
path(wmodel,x0,N=20,deltat=1)
#red wins after 20 days, have 2.71 divisions left
w=.75
wmodel=function(x){c(-w*lambda*b*x[2]-lambda*d*x[1]*x[2],-w*b*x[1]-d*x[1]*x[2])}
path(wmodel,x0,N=15,deltat=1)
#red wins after 13 days, have 2.9 divisions left

#part c
#the blue army benefits massively from fighting in adverse weather 
#so if I was the blue commander I would expect reds larger force to 
#attack on a sunny good weather day (w>.6)

#part d
lambda=1.5

w=.1
wmodel=function(x){c(-w*lambda*b*x[2]-lambda*d*x[1]*x[2],-w*b*x[1]-d*x[1]*x[2])}
path(wmodel,x0,N=60,deltat=1)
#red wins after 58 days, have 3.3 divisions left
w=.2
wmodel=function(x){c(-w*lambda*b*x[2]-lambda*d*x[1]*x[2],-w*b*x[1]-d*x[1]*x[2])}
path(wmodel,x0,N=35,deltat=1)
#red wins after 34 days, have 3.62 divisions left
w=.5
wmodel=function(x){c(-w*lambda*b*x[2]-lambda*d*x[1]*x[2],-w*b*x[1]-d*x[1]*x[2])}
path(wmodel,x0,N=17,deltat=1)
#red wins after 16 days, have 3.93 divisions left
w=.75
wmodel=function(x){c(-w*lambda*b*x[2]-lambda*d*x[1]*x[2],-w*b*x[1]-d*x[1]*x[2])}
path(wmodel,x0,N=12,deltat=1)
#red wins after 11 days, have 4.025 divisions left

lambda=2

w=.1
wmodel=function(x){c(-w*lambda*b*x[2]-lambda*d*x[1]*x[2],-w*b*x[1]-d*x[1]*x[2])}
path(wmodel,x0,N=70,deltat=1)
#red wins after 66 days, have 2.69 divisions left
w=.2
wmodel=function(x){c(-w*lambda*b*x[2]-lambda*d*x[1]*x[2],-w*b*x[1]-d*x[1]*x[2])}
path(wmodel,x0,N=40,deltat=1)
#red wins after 38 days, have 3.12 divisions left
w=.5
wmodel=function(x){c(-w*lambda*b*x[2]-lambda*d*x[1]*x[2],-w*b*x[1]-d*x[1]*x[2])}
path(wmodel,x0,N=20,deltat=1)
#red wins after 17 days, have 3.55 divisions left
w=.75
wmodel=function(x){c(-w*lambda*b*x[2]-lambda*d*x[1]*x[2],-w*b*x[1]-d*x[1]*x[2])}
path(wmodel,x0,N=15,deltat=1)
#red wins after 12 days, have 3.66 divisions left

lambda=4

w=.1
wmodel=function(x){c(-w*lambda*b*x[2]-lambda*d*x[1]*x[2],-w*b*x[1]-d*x[1]*x[2])}
path(wmodel,x0,N=110,deltat=1)
#blue wins after 107 days, have .51 divisions left
w=.2
wmodel=function(x){c(-w*lambda*b*x[2]-lambda*d*x[1]*x[2],-w*b*x[1]-d*x[1]*x[2])}
path(wmodel,x0,N=120,deltat=1)
#red wins after 113 days, have .366 divisions left
w=.5
wmodel=function(x){c(-w*lambda*b*x[2]-lambda*d*x[1]*x[2],-w*b*x[1]-d*x[1]*x[2])}
path(wmodel,x0,N=30,deltat=1)
#red wins after 26 days, have 1.7 divisions left
w=.75
wmodel=function(x){c(-w*lambda*b*x[2]-lambda*d*x[1]*x[2],-w*b*x[1]-d*x[1]*x[2])}
path(wmodel,x0,N=20,deltat=1)
#red wins after 16 days, have 2.0 divisions left

lambda=5

w=.1
wmodel=function(x){c(-w*lambda*b*x[2]-lambda*d*x[1]*x[2],-w*b*x[1]-d*x[1]*x[2])}
path(wmodel,x0,N=65,deltat=1)
#blue wins after 61 days, have .89 divisions left
w=.2
wmodel=function(x){c(-w*lambda*b*x[2]-lambda*d*x[1]*x[2],-w*b*x[1]-d*x[1]*x[2])}
path(wmodel,x0,N=50,deltat=1)
#blue wins after 46 days, have .74 divisions left
w=.5
wmodel=function(x){c(-w*lambda*b*x[2]-lambda*d*x[1]*x[2],-w*b*x[1]-d*x[1]*x[2])}
path(wmodel,x0,N=40,deltat=1)
#blue wins after 36 days, have .335 divisions left
w=.75
wmodel=function(x){c(-w*lambda*b*x[2]-lambda*d*x[1]*x[2],-w*b*x[1]-d*x[1]*x[2])}
path(wmodel,x0,N=30,deltat=1)
#red wins after 28 days, have .58 divisions left

#conclusions from sensitivity analysis
#conclusions from part c stand.
#blue army performs much better during bad weather. Their higher efficiency
#fares much better during periods of bad weather than the red army.
#However, during sunny good weather, the blue army will still lose
#even when 5 times more efficient as shown in the last model of part d.




