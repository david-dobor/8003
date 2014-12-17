#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
## Example 8.4.1
library(gtools)
alpha <- c(728, 584, 138)
seq.length <- 1000
theta <- rdirichlet(seq.length, alpha )

theta.diff <- theta[,1]-theta[,2]
quantile(theta.diff, c(0.025,0.5,0.975) )
hist(theta.diff)

#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
## Example 8.4.2
lightspeed <- read.table("http://astro.temple.edu/~zhaozhg/Stat8003/data/lightspeed.txt", sep=" ",col.names=FALSE)
colnames(lightspeed) <- c("time")
hist(lightspeed$time,br=20)
true.time <- 7441/299792458

mu0 <- 30
k0 <- 10
nu0 <- 2
sigma0 <- 2
seq.length <- 1000

n <- length( lightspeed$time )
ybar <- mean( lightspeed$time )
sSq <- var( lightspeed$time )
rate <- ( nu0 * sigma0^2 + (n-1)*sSq + k0*n*(ybar - mu0)^2/(k0+n) )/2
shape <- (nu0 + n)/2

sigma.sim <- 1/rgamma( seq.length, shape=shape, rate=rate )
M <- n/(n+k0)
mu.sim <- rnorm( seq.length, M*ybar + (1-M)*mu0, sqrt( M*sigma.sim ) )


## ADDED BY ME: ##
hist(mu.sim)
mean(mu.sim)
median(mu.sim)
quantile(mu.sim, c(0.025, 0.5, 0.975))
## END ADDED BY ME ## 

########################
## Model checking using the posterior predictive p-values
Y.rep <- rnorm(seq.length, mu.sim, sqrt( sigma.sim ) )
##(a)
par( mfrow=c(5,4))
hist(lightspeed$time)
for( i in 1:19)
  hist( sample(Y.rep,  n, replace=FALSE) )
       
##(b)
num.sample <- 10000
count <- 0
for( i in 1:10000)
  if( min( sample(Y.rep, n, replace=FALSE) ) < min(lightspeed$time) )
  count <- count+1
p.value.post <- count/num.sample


##(c)
test.stat <- function(Y, theta)
{
  Y.sort <- sort(Y)
  abs( Y.sort[61]-theta ) - abs( Y.sort[6]- theta)
}
t.stat <- rep( 0, seq.length )
t.stat.sim <- rep(0, seq.length)
for( i in 1:seq.length )
  {
    Y <- rnorm( n, mu.sim[i], sqrt( sigma.sim[i] ) )
    t.stat[i] <- test.stat( lightspeed$time, mu.sim[i]) 
    t.stat.sim[i] <- test.stat(Y, mu.sim[i] )
  }
p.value.post2 <- mean( t.stat.sim > t.stat )


######################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
## Gibbs sampler for the Baseball dataset

baseball <- read.table("http://astro.temple.edu/~zhaozhg/Stat8003/data/EfronMorrisBB.txt", header=TRUE)
y <- baseball[,5]
J <- length(y)

num.mcmc <- 50000
theta.mcmc <- array(0, c(num.mcmc, J ))
mu.mcmc <- array(0, c(num.mcmc, 1))
sigma.mcmc <- array(0, c(num.mcmc, 1) )
tau.mcmc <- array(0, c(num.mcmc, 1 ) )
set.seed(2)

theta.mcmc[1,] <- rnorm(J, 0.2, 0.1)
mu.mcmc[1] <- rnorm(1,0,1)
sigma.mcmc[1] <- 1/rgamma(1,2,2)
tau.mcmc[1] <- 1/rgamma(1,2,2)

for( i in 2:num.mcmc )
  {
    M <- tau.mcmc[i-1,1] /(tau.mcmc[i-1,1] + sigma.mcmc[i-1,1] )
    theta.mcmc[i,] <- rnorm(J,  M*y + (1-M)*mu.mcmc[i-1,1], sqrt( M *sigma.mcmc[i-1,1] ) )
    mu.mcmc[i,1] <- rnorm( 1, mean( theta.mcmc[i,]), sqrt( tau.mcmc[i-1,1]/J) )
    sigma.mcmc[i,1] <- 1/rgamma(1, shape=J/2+0.001, rate=sum( (y-theta.mcmc[i,])^2 )/2 +0.001 )
    tau.mcmc[i,1] <- 1/rgamma(1, shape=J/2+0.001, rate=sum( (theta.mcmc[i,]-mu.mcmc[i,1])^2)/2 + 0.001 )
  }

array.mcmc <- cbind(theta.mcmc, mu.mcmc, sigma.mcmc, tau.mcmc)
colnames(array.mcmc) <- c("theta1","theta2","theta3","theta4","theta5","theta6","theta7","theta8","theta9","theta10","theta11","theta12","theta13","theta14","theta15","theta16","theta17","theta18","mu","sigma","tau")

burnin <- 10000
thin <- 40
keep.ind <- 10000 + c(1:1000)*40
array.mcmc.thin <- array.mcmc[ keep.ind,]
par(mfrow=c(5,5))
for(i in 1:21)
  {
    hist( array.mcmc.thin[,i] ) 
  }


par(mfrow=c(5,5))
for(i in 1:21)
  acf( array.mcmc[,i])

par(mfrow=c(5,5))
for(i in 1:21)
  acf( array.mcmc.thin[,i])

apply( array.mcmc.thin, 2, quantile, c(0.025,0.5,0.975) )
