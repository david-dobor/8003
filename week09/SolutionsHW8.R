#########################################################################
######################## Solutions to Homework 8 ######################## 
############################ (Andrew, David) ############################


#########################################################################
## problem 8.1
#chi squared with 20 degrees freedom at .975 and .025
chi.low <- qchisq( 0.025, df=20 )
chi.high <- qchisq( 0.975, df=20 )

s <- sum( c(1.07, 0.88, 0.66, 0.55, 1.15, 0.65, 3.45, 3.55, 3.51, 0.48) )

ci.lower <-  chi.low / (2*s)   #0.3006513
ci.higher <- chi.high / (2*s)  #1.071148


#########################################################################
## problem 8.3
alpha = 0.05
x <- c(4.5,7.5,22,9,7,10.5,14.5,15,9,19,9,3.5,8,11,2.5,5,9,8.5,7.5,18,20,14,20,8)
n <- length( x )

#part (a)  - Asymptotic C.I. using CLM
lower.norm <- mean( x ) - qnorm( 1-alpha/2 ) * sqrt( var(x) )/sqrt(n)
upper.norm <- mean( x ) + qnorm( 1-alpha/2 ) * sqrt( var(x) )/sqrt(n)

#part (b) - C.I. based on T-distributions
lower.t <- mean( x ) - qt( 1-alpha/2, n-1 )* sqrt( var(x) )/sqrt(n)
upper.t <- mean( x ) + qt( 1-alpha/2, n-1) * sqrt( var(x) )/sqrt(n)


#part (c) - bootstrap method
#using package bootstrap:
library(bootstrap)
mu.boot <- bootstrap(x, nboot=10000,theta=mean)
conf.int = quantile(mu.boot$thetastar,c(.025,.975))

#alternatively, using package boot:
library(boot)
samplemean <- function(x,d) {
    return(mean(x[d]))
}
x.mean.boot1 <- boot( data=x, statistic=samplemean, R=10000 )
boot.lb <- x.bar + qnorm( p=alpha/2 ) * sqrt( var( x.mean.boot1$t[,1] ) )
boot.ub <- x.bar - qnorm( p=alpha/2 ) * sqrt( var( x.mean.boot1$t[,1] ) )


#########################################################################
## problem 8.4
# given data:
n = c(0:13)
freq = c(14, 30, 36, 68, 43, 43, 30, 14, 10,  6,  4,  1,  1,  0)
alpha = 0.05

# esitmate of the poisson parameter:
X.bar = sum( n * freq ) / sum(freq)   #ans:3.893333

#part(a) - C.I. based on the pivot method
lb.pivot <- X.bar - sqrt( X.bar ) * qnorm( 1-alpha/2 ) / sqrt(sum( freq ))
ub.pivot <- X.bar + sqrt( X.bar ) * qnorm( 1-alpha/2 ) / sqrt(sum( freq ))
   ### ans: C.I.pivot = [3.670054, 4.116613]

#part(b) - C.I. based on variance stabilization
lb.vst <- (sqrt( X.bar ) - qnorm( 1-alpha/2 )/(sqrt( sum( freq ) )*2) )^2 
ub.vst <- (sqrt( X.bar ) + qnorm( 1-alpha/2 )/(sqrt( sum( freq ))*2 ) )^2  
   ### ans: C.I.vst = [3.673255, 4.119814]

# the lengths of the coverage intervals turn out to be the same:
ub.vst - lb.vst      #ans: 0.4465584
ub.pivot - lb.pivot  #ans: 0.4465584
