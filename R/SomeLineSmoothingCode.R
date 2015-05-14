# You'll have to test some of these parameters to get the best smooth
# for your data :)

# Some data - "cars" is a built in dataset
x <- cars$speed
y <- cars$dist

# jagged line:
plot(x,y, type='l', lwd=2, col='red')

# smooth - "span" is the degree of smoothing - you'll need to test this out
# on your data. 
sm <- loess(y~x, span=0.35)
# Use this to interpolate a bunch more points if you don't get a smooth enough line
xl <- seq(min(x),max(x), (max(x) - min(x))/1000)
# OR
smoothSpline <- smooth.spline(x,y,spar=0.40)
#pr <- predict(sm, se=T)

# plot w/ points first
plot(x,y)
# loess smooth (generally works better with more data than I have here - try it out)
lines(predict(sm), col="red", lwd=2)
# smoother loess
lines(xl, predict(sm, xl), col="green", lwd=2)
# smoothSpline
lines(smoothSpline, col="blue", lwd=2)


#####

# Now with ggplot
library(ggplot2)

# The "se=F" in geom_smooth means to NOT plot the confidence interval of the smoothed fit.
# You can simply remove it if you want to see the confidence interval. 
p <- ggplot(cars, aes(x=speed, y=dist)) + geom_point() + geom_smooth(se=F)
p
