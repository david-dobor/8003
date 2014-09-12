
####################################################################################################################################
## load the data
gdp.all <- read.csv("http://astro.temple.edu/~zhaozhg/Stat8003/data/GDP_Per_Capita.csv")

gdp <- gdp.all[,62]
gdp[ is.na(gdp) ] <- NULL


####################################################################################################################################
## The method of moment based on the normal distribution
m1 <- mean( gdp )
m2 <- mean( gdp^2 )

mu.hat <- m1
sigma.2.hat <- m2 - m1^2

plot( density( gdp, from=0 ), col='green', xlab="gdp", ylab="density", main="Normal Model", cex.lab=1.5, cex=2)
x=c(1:max(gdp))
points( x, dnorm( x, mu.hat, sqrt(sigma.2.hat) ), 'l', col='red' )
legend(50000, 3e-05, c("density", "fitted density"), lty=c(1, 1), col=c('green', 'red'), cex=2 )

## The method of moment based on the log-normal distribution
sigma.2.hat.lnorm <- log( m2/m1^2 )
mu.hat.lnorm <- log(m1) - sigma.2.hat.lnorm/2
x=c(1:max(gdp))
plot( density( gdp, from=0 ), col='green', 'l', xlab="gdp", ylab="density", main="Log Normal Model", cex.lab=1.5, cex=2, ylim=c(0,10e-05))
points( x, dlnorm( x, mu.hat.lnorm, sqrt(sigma.2.hat.lnorm) ), 'l', col='red' )
legend(50000, 3e-05, c("density", "fitted density"), lty=c(1, 1), col=c('green', 'red'), cex=2 )


## Log-transformation of the data
l.gdp <- log(gdp)
m1.log <- mean( l.gdp )
m2.log <- mean( l.gdp^2)
mu.hat.log <- m1.log
sigma.2.hat.log <- m2.log - m1.log^2

plot( density( l.gdp, from=0 ), col='green', ylim=c(0,0.4), xlab="log(gdp)", ylab="density", main="Log Normal Model", cex.lab=1.5, cex=2, )
x <- seq( min(l.gdp), max(l.gdp), (max(l.gdp)-min(l.gdp))/1000 )
points( x, dnorm( x, mu.hat.log, sqrt(sigma.2.hat.log) ), 'l', col='red' )
legend(2, 0.3, c("density", "fitted density"), lty=c(1, 1), col=c('green', 'red'), cex=2 )

####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
## Run the simulation to compare two estimators for the log-normal model
mu <- 0
sigma <- 1

mom1.lognormal <- function(X)
  {
    m1 <- mean( X )
    m2 <- mean( X^2 )
    
    sigma.2.hat.lnorm <- log( m2/m1^2 )
    mu.hat.lnorm <- log(m1) - sigma.2.hat.lnorm/2

    ## Return the value
    c(mu.hat.lnorm, sigma.2.hat.lnorm)
  }
mom2.lognormal <- function(X)
  {
    m1.log <- mean( log(X) )
    m2.log <- mean( log(X)^2 )
    mu.hat.log <- m1.log
    sigma.2.hat.log <- m2.log - m1.log^2

    ## Return the value
    c(mu.hat.log, sigma.2.hat.log)
  }

nsim <- 2000
n <- 1000
mom1 <- matrix(0, nsim, 2)
mom2 <- matrix(0, nsim, 2)
for( i in 1:nsim)
  {
    X <- exp( rnorm(1000, mu, sigma) )
    mom1[i,] <- mom1.lognormal( X )
    mom2[i,] <- mom2.lognormal( X )
  }
## Calculate the risk function
truePar <- matrix( c( rep(mu, nsim), rep(sigma, nsim) ), nsim, 2)
sq.error.1 <- apply( (mom1-truePar)^2, 1, sum )
sq.error.2 <- apply( (mom2-truePar)^2, 1, sum )
mse.1 <- mean(sq.error.1)
mse.2 <- mean(sq.error.2)

####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
## Example: traffic light
traffic.light <- read.csv("http://astro.temple.edu/~zhaozhg/Stat8003/data/traffic_light.txt")
apply( traffic.light, 1, prod )
m1 <- sum( traffic.light$n * traffic.light$freq )/sum( traffic.light$freq )
lambda.hat1 <- m1
## Calculate MOM based on the variance
## ## create a new column x_i-bar{x}
traffic.light$diff <- (traffic.light$n)^2
m2 <- sum(  traffic.light$freq * traffic.light$diff )/ ( sum( traffic.light$freq ) )
lambda.hat2 <- ( sqrt( 1+4*m2) -1 )/2



####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################

## Plot the density function and the likelihood function of N(\theta, 1)
##              theta=1
##              X=1
x <- c(1:1000)/100
postscript( file="./figure/density.eps",horizontal=FALSE)
plot(x, dexp( x, 1), 'l', col='red', xlab="x", ylab="density", main="pdf", cex.lab=1.5, cex.main=2)
dev.off()

lambda.all <- c(1:1000)/100
postscript( file="./figure/likelihood.eps",horizontal=FALSE)
plot(lambda.all, dexp( 1, lambda.all), 'l', col='red', xlab="lambda", ylab="likelihood", main="Likelihood", cex.lab=1.5, cex.main=2)
dev.off()
