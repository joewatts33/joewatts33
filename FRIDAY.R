#Chapter 7: 5,6,11
#5 part a
2050/12
#I estimate 171 calls per month if they are evenly distributed

#part b
#assuming 'normal range of variation' is 95% confidence interval
#also realizing that exponential distribution does not take into
#account the time interval before that point to determine the time 
#until another event. This means each month 'resets' the ability to
#have housefires so the yearly fire cannot be generalized with a yearly rate
#Using the confidence interval from the book and the fact that 
#this is an exponential distribution so sigma=1/lambda
#(n/171)+-2*sqrt(n)/171 interval must contain 1 
(147/171)+2*sqrt(147)/171
(199/171)-2*sqrt(199)/171
#in one month, calls can range {147,199}, so a yearly range would be
147*12;199*12
#{1764,2388}

#part c 
#range of lambda that 2050 is in range of normal distribution
#use n/lambda +- 2*sqrt(n)/lambda , set equal to one and solve
#this gives a yearly lambda, divide by 12 for monthly
(2050+2*sqrt(2050))/12
(2050-2*sqrt(2050))/12
#{163,178} our 171 estimate is very accurate

#part d 
#x=#calls in a year, lambda=171
#assume x is poisson lambda*t, t=total # months
#use formula lambda+-2*sqrt(lambda/t), 
#set 2*sqrt(lambda/t) equal to 1/2
4^2*171
2736/12
#2736 months, or 228 years


#number 6
#part a show expected value and variance=lambda*t
#taken from the Class notebook, posted as a photo


#part b 
#probability that calls deviate from 171 by as much as 18
171-18;171+18
#P(153<X<189)
ppois(189,171)-ppois(153,171)
#probability is 83.1%

#part c
#for normal variation, we need p=95%
ppois(197,171)-ppois(145,171)
#normal variation 95% CI: {145,197}

#part d
#comparing models
#example 7.4 in the text is based off of the exponential model
#and problem 6 is based off of poisson which is a long run distribution
#based on exponential behavior. Because of this, Determining normal
#variation in calls in a day would be best modeled with exponential,
#but the year and longer estimates should be done with poisson




#problem 11
#16 bombers
#two paths w defense high(missiles) and low(guns)
#binomial distribution: planes are hit or not hit 

#LOW
#1 minute
#20 shells/min
#20 shells total
#ptotal=pdetect*paquire*phit for each shell
shellprob=.9*.8*.05
dbinom(3,20,shellprob)
#.0285% chance 3 planes are hit
1-pbinom(3,20,shellprob)
#gives .005% chance no more than 3 planes are hit

#HIGH
#5 minutes
#3 missiles/min
#15 missiles total
#ptotal=pdetect*paquire*phit for each missile
missileprob=.75*.95*.7
pbinom(9,15,missileprob)-pbinom(6,15,missileprob)
#gives almost 54% chance 6, 7, or 8 planes are hit
#The LOW route is much safer and maximizes bombers survival

#part b
#part a tells us how to predict the # of bombers that we will have
#looking for 95% or above to be statistically significant
1-pbinom(2,20,shellprob)
#not 95%, this is 92.7%
1-pbinom(3,20,shellprob)
#this is 99.5%, safe to assume only 3 bombers will be lost.
#13 bombers to approach target
#70% chance to destroy target per bomber
dbinom(0,13,.7)
#the probability that the target is not hit is .00000159%.
#this mission should succeed

#part c
#minimum # bombers for 95% mission success
1-dbinom(0,3,.7)
#The minimim # is 3 bombers for 97.3% mission success (2=91%)

#part d
#analyze sensitivity to p=.7 that bombers complete mission
#find new minimum number of bombers for each p value for 95% success
#p=.6
1-dbinom(0,4,.6)
#minimum 4 bombers

#p=.5
1-dbinom(0,5,.5)
#minimum 5 bombers

#p=.4
1-dbinom(0,6,.4)
#minimum 6 bombers

#p=.3
1-dbinom(0,9,.3)
#minimum 9 bombers

#part e
#say pdetect and p are reduced by 33%
#gun pdetect is now .6, bomber p is now .46666
#new shell probability to hit bomber
shellprob=.6*.8*.05
1-pbinom(3,20,shellprob)
#gives .001% chance no more than 3 planes are hit, small change
#p=.4666
1-dbinom(0,5,.466666)
#95% chance of success with 5 planes in bad conditions
#the outcome is still the same in bad weather, but the
#defenders have less drawback due to the bad weather
#because the attackers need more bombers but defense effectiveness
#is relatively unchanged. However, the bombers success is still very likely.






