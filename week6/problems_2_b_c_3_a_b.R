#########################################################################
############################# Problem 2 (c) #############################
#########################################################################

### generate ten thousand observations from Y = 10*log(Gamma(10,1)) - Gamma(10,1)
Y.gamma <- rgamma(10000, shape=10, rate=1)
Y <- 10*log(Y.gamma) - Y.gamma

### compute the cdf (xaxis and emp.cdf used for plotting)
xaxis <- seq( min(Y), max(Y), 0.01)
emp.cdf<- xaxis
for(i in 1:length( xaxis ) )
{
    emp.cdf[i] <- mean( Y <= xaxis[i] )
}

### plot this cdf (not really necessary)
plot( xaxis, emp.cdf, main="CDF", xlab="y", ylab="cdf", cex.main=2, cex.lab=1.5,'l', lwd=3, col='blue' )

### where is this cdf equal 0.05? Inspecting the output gives the answer.
k = xaxis[579]   # ans: 11.08295  by inspection of output this is where emp.cdf equals 0.05
# emp.cdf[579]   # this just checks that the answer is indeed 0.05

### We can now recover c from the formula k = log( (10*c)^10 ) 
c <- .1*exp(xaxis[579]/10)   #ans: 0.3029189

#########################################################################
############################# Problem 3 (a) #############################
#########################################################################

### given data:
x.vals <- c(1.07, 0.88, 0.66, 0.55, 1.15, 0.65, 3.45, 3.55, 3.51, 0.48)

### compute the test statistic:
test.stat <- 10*log(sum(x.vals)) - sum(x.vals)  # ans: 11.74459

### obtain the p-value by computing the cdf at this point (at 11.74459).
xaxis[645]      #the closest point to 11.74459 in the xaxis array is here
p.value <- emp.cdf[645]    # the value of the cdf here is the p-value = 0.113



#########################################################################
############################# Problem 3 (b) #############################
#########################################################################

X.bar <- mean(x.vals)
X.bar
GLRT.stat <- -2*10*log(X.bar) - 2*10 +2*10*X.bar


# plot chi-squared-1 (not really necessary)
x <- seq(-.05, 12, length=100)
plot(x, dchisq(x, df=1), main="chi squared, 1 degree of freedom", type="l", lwd=2, col="blue")

#lines(x, dgamma(x,shape=1/2 ,scale=2), type="l", lwd=2, col="blue")  # same as chi-square(1)

1 - pchisq(2.562525, df=1)   #ans: 0.1094237

