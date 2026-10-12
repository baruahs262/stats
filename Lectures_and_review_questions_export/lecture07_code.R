#%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%----
#Functions that created the plots shown in lecture 7
#Creates a scatter plot of x and y
plainplot = function(x, y, xlim=c(-3,3), ylim=c(-3,3), main="")
{
  options(repr.plot.width=4, repr.plot.height=4)
  plot(x, y, xlab="x", ylab="y", xlim=xlim, ylim=ylim, main=main, pch=16)
}

#Graphs a scatter plot and histogram for the marginal distribution of x
marginalx = function(x, y, xlim=c(-3,3), ylim=c(-3,3), breaks=(-7:6 + .5)/2)
{
  options(repr.plot.width=8, repr.plot.height=4)
  par(mar=c(4,4,1,1), mfrow=c(1, 2))
  plot(x, y, xlab="x", ylab="y", xlim=xlim, ylim=ylim,pch=16)
  hist(x[(x>min(breaks)) & (x<max(breaks))], freq=FALSE, breaks=breaks, main="Histogram of x", xlab="x")
}

#Graphs a scatter plot and histogram for the marginal distribution of y
marginaly = function(x, y, xlim=c(-3,3), ylim=c(-3,3), breaks=(-7:6 + .5)/2)
{
  options(repr.plot.width=8, repr.plot.height=4)
  par(mar=c(4,4,1,1), mfrow=c(1, 2))
  plot(x, y, xlab="x", ylab="y", xlim=xlim, ylim=ylim, pch=16)
  hist(y[(y>min(breaks)) & (y<max(breaks))], freq=FALSE, breaks=breaks, main="Histogram of y", xlab="y")
}

#Shows the vertical strip on the scatter plot and graphs a histogram for the conditional distribution of y
#given that x is roughly x0
conditionaly = function(x, y, x0, width=0.5, xlim=c(-3,3), ylim=c(-3,3), breaks=(-7:6 + .5)/2)
{
  options(repr.plot.width=8, repr.plot.height=4)
  par(mar=c(4,4,1,1), mfrow=c(1, 2))
  plot(x, y, xlab="x", ylab="y", xlim=xlim, ylim=ylim, pch=16)
  lines(c(x0-width/2, x0-width/2), ylim, lty=2, col="green", lwd=4)
  lines(c(x0+width/2, x0+width/2), ylim, lty=2, col="green", lwd=4)    
  hist(y[(x < x0+width/2) & (x > x0-width/2) & (y>min(breaks)) & (y<max(breaks))], freq=FALSE, breaks=breaks, xlab = "y", main="Histogram of y in vertical strip")
}

#Shows the horizontal strip on the scatter plot and graphs a histogram for the conditional distribution of x
#given that y is roughly y0
conditionalx = function(x, y, y0, width=0.5, xlim=c(-3,3), ylim=c(-3,3), breaks=(-7:6 + .5)/2)
{
  options(repr.plot.width=8, repr.plot.height=4)
  par(mar=c(4,4,1,1), mfrow=c(1, 2))
  plot(x, y, xlab="x", ylab="y", xlim=xlim, ylim=ylim, pch=16)
  lines(xlim, c(y0-width/2, y0-width/2), lty=2, col="green", lwd=4)
  lines(xlim, c(y0+width/2, y0+width/2), lty=2, col="green", lwd=4)    
  hist(x[(y < y0+width/2) & (y > y0-width/2) & (x>min(breaks)) & (x<max(breaks))], freq=FALSE, breaks=breaks, xlab = "x", main="Histogram of x in horizontal strip")
}

#Shows the vertical strip on the scatter plot, graphs a histogram for the conditional distribution of y
#given that x is roughly x0, and plots the conditional average for y on the scatter plot and in the histogram
conditionalavgy = function(x, y, x0, width=0.5, xlim=c(-3,3), ylim=c(-3,3), breaks=(-7:6 + .5)/2)
{
  options(repr.plot.width=8, repr.plot.height=4)
  par(mar=c(4,4,1,1), mfrow=c(1, 2))
  plot(x, y, xlab="x", ylab="y", xlim=xlim, ylim=ylim, pch=16)
  lines(c(x0-width/2, x0-width/2), ylim, lty=2, col="green", lwd=4)
  lines(c(x0+width/2, x0+width/2), ylim, lty=2, col="green", lwd=4) 
  condmean = mean(y[(x < x0+width/2) & (x > x0-width/2)])
  points(x0, condmean, lwd=3, col="green", pch=3, cex=1.5)
  hist(y[(x < x0+width/2) & (x > x0-width/2) & (y>min(breaks)) & (y<max(breaks))], freq=FALSE, breaks=breaks, xlab = "y", main="Histogram of y in vertical strip")
  lines(c(condmean, condmean), ylim, lty=3, col="green", lwd = 4)  
}

#Shows the horizontal strip on the scatter plot, graphs a histogram for the conditional distribution of x
#given that y is roughly y0, and plots the conditional average for x on the scatter plot and in the histogram
conditionalavgx = function(x, y, y0, width=0.5, xlim=c(-3,3), ylim=c(-3,3), breaks=(-7:6 + .5)/2)
{
  options(repr.plot.width=8, repr.plot.height=4)
  par(mar=c(4,4,1,1), mfrow=c(1, 2))
  plot(x, y, xlab="x", ylab="y", xlim=xlim, ylim=ylim, pch=16)
  lines(xlim, c(y0-width/2, y0-width/2), lty=2, col="green", lwd=4)
  lines(xlim, c(y0+width/2, y0+width/2), lty=2, col="green", lwd=4) 
  condmean = mean(x[(y < y0+width/2) & (y > y0-width/2)])
  points(condmean, y0, lwd=3, col="green", pch=3, cex=1.5)
  hist(x[(y < y0+width/2) & (y > y0-width/2) & (x>min(breaks)) & (x<max(breaks))], freq=FALSE, breaks=breaks, xlab = "x", main="Histogram of x in horizontal strip")
  lines(c(condmean, condmean), ylim, lty=3, col="green", lwd = 4)  
}

#Displays the graph of averages for y on the scatter plot.
graphavgy = function(x, y, x0=(-3:3)/2, width=0.5, xlim=c(-3,3), ylim=c(-3,3))
{
  options(repr.plot.width=4, repr.plot.height=4)
  par(mar=c(4,4,1,1), mfrow=c(1, 1))
  plot(x, y, xlab="x", ylab="y", xlim=xlim, ylim=ylim, col="yellow",pch=16)
  for (x0i in x0)
  {
    condmean = mean(y[(x < x0i+width/2) & (x > x0i-width/2)])
    points(x0i, condmean, lwd=3, pch=3, cex=1.5)
  }
}

#Displays the graph of averages for x on the scatter plot.
graphavgx = function(x, y, y0=(-3:3)/2, width=0.5, xlim=c(-3,3), ylim=c(-3,3))
{
  options(repr.plot.width=4, repr.plot.height=4)
  par(mar=c(4,4,1,1), mfrow=c(1, 1))
  plot(x, y, xlab="x", ylab="y", xlim=xlim, ylim=ylim, col="yellow",pch=16)
  for (y0i in y0)
  {
    condmean = mean(x[(y < y0i+width/2) & (y > y0i-width/2)])
    points(condmean, y0i, lwd=3, pch=3, cex=1.5)
  }
}

#Displays the graph of averages for both x and y on the scatter plot.
graphavgxy = function(x, y, x0=(-3:3)/2, y0=(-3:3)/2, width=0.5, xlim=c(-3,3), ylim=c(-3,3))
{
  options(repr.plot.width=4, repr.plot.height=4)
  par(mar=c(4,4,1,1), mfrow=c(1, 1))
  plot(x, y, xlab="x", ylab="y", xlim=xlim, col="yellow",ylim=ylim,pch=16)
  for (y0i in y0)
  {
    condmean = mean(x[(y < y0i+width/2) & (y > y0i-width/2)])
    points(condmean, y0i, lwd=3, pch=3, cex=1.5)
  }
  for (x0i in x0)
  {
    condmean = mean(y[(x < x0i+width/2) & (x > x0i-width/2)])
    points(x0i, condmean, lwd=3, pch=3,cex=1.5)
  }
}

#Displays the graph of averages for y and the regression line for predicting y based on x on the scatter plot.
graphavgregy = function(x, y, x0=(-3:3)/2, width=0.5, xlim=c(-3,3), ylim=c(-3,3))
{
  options(repr.plot.width=4, repr.plot.height=4)
  par(mar=c(4,4,1,1), mfrow=c(1, 1))
  plot(x, y, xlab="x", ylab="y", xlim=xlim, ylim=ylim, col="yellow",pch=16)
  for (x0i in x0)
  {
    condmean = mean(y[(x < x0i+width/2) & (x > x0i-width/2)])
    points(x0i, condmean, lwd=3, pch=3, cex=1.5)
  }
  beta = coef(lm(y~x))
  lines(xlim, c(beta[1] + xlim[1]*beta[2], beta[1] + xlim[2]*beta[2]), lwd=2)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%% GENERATE DATA %%%%%%%%%%%%%%%%%%----
#This code sets up the data to be used in the 5 plots in the exercises.
library(MASS)
set.seed(1234567)

## Data A
fb0.8 = mvrnorm(500,c(0,0),matrix(c(1,0.8,0.8,1),2,2))
colnames(fb0.8) = c("x", "y")

## Data B
x=runif(600)*4-2
y = 2-x^2+0.35*rnorm(600)
arc = cbind(x,y)

## Data C
x=rexp(1000)*1.0-3
y=rexp(500)*1.0-3
rtail = cbind(x,y)

## Data D
hetero = mvrnorm(500,c(0,0),matrix(c(1,0.7,0.7,1),2,2))
colnames(hetero) = c("x", "y") 
hetero[,2] = hetero[,2] * exp(hetero[,1]/4)/2

## Data E
theta = runif(500)*2*pi
radius = rchisq(500, df=40)/20
x=radius*cos(theta)
y=radius*sin(theta)
sinusoid = cbind(x,y)
  



#%%%%%%%%%%%%%%%%%%%%%%%%% PLOTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%----
par(mfrow=c(2,3))
plainplot(fb0.8[,1], fb0.8[,2], main="Plot A") #The data for plot A
plainplot(arc[,1], arc[,2], main="Plot B") #The data for plot B
plainplot(rtail[,1], rtail[,2], main="Plot C") #The data for plot C
plainplot(hetero[,1], hetero[,2], main="Plot D") #The data for plot D
plainplot(sinusoid[,1], sinusoid[,2], main="Plot E") #The data for plot E
