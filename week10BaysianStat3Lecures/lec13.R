
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

#initial values:
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



######################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
## empirical Bayes estimator for the Baseball dataset

baseball <- read.table("http://astro.temple.edu/~zhaozhg/Stat8003/data/EfronMorrisBB.txt", header=TRUE)
y <- baseball[,5]
y.vst <- 2*sqrt(45)*asin( sqrt(y) )
m1 <- mean(y.vst)
m2 <- mean(y.vst^2)

mu.hat <- m1
tau.hat.sq <- m2-1-mu.hat^2

M.hat <- tau.hat.sq/(tau.hat.sq+1)
theta.hat <- M.hat*y.vst + (1-M.hat)*mu.hat
p.hat <- sin( theta.hat/2/sqrt(45))^2


######################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
## Lindley-James stein estimator
M.LJS <- (1- 15/sum( (y.vst-m1)^2 ) )
theta.LJS <- M.LJS*y.vst+(1-M.LJS)*m1
p.LJS <- sin( theta.LJS/2/sqrt(45))^2

Season.Ave=baseball[,10]
sum( (p.hat - Season.Ave)^2 )
sum( (p.LJS -Season.Ave)^2 )
sum( (y -Season.Ave)^2 )




######################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#### Two group models
#### Example 6.4.4
X <- read.table("http://astro.temple.edu/~zhaozhg/Stat8003/data/prostate_X.csv",sep=",")
Y <- read.table("http://astro.temple.edu/~zhaozhg/Stat8003/data/prostate_Y.csv",sep=",")

unequalvar <- function(Xi, group)
  {
    n1 <- length( which(group==1) )
    n2 <- length( which(group==2) )
    mean1 <- mean(Xi[ group==1] )
    mean2 <- mean(Xi[ group==2] )
    s1 <- var( Xi[group==1] )
    s2 <- var( Xi[group==2] )

    s.unpool <- (s1/n1 +s2/n2 )
    t.stat <- (mean1-mean2)/sqrt(s.unpool)
    df.sat <- (s1/n1+s2/n2)^2/( (s1/n1)^2/(n1-1) + (s2/n2)^2/(n2-1) )

    z.value <- qnorm( pt( t.stat, df.sat ) )
  }

z.value <- apply( X, 1, unequalvar, Y)

num <- dnorm( z.value )

p <- length(z.value)
h <- 1.06 * sqrt( var(z.value) ) / (p^(1/5))
f.hat <- rep(0, p)

for( i in 1:p )
  {
    f.hat[i] <- mean( dnorm( z.value[i]-z.value, 0, h ))
  }

local.fdr <- num/f.hat
alpha <- 0.05

R <- max( ( cumsum(sort( local.fdr))/c(1:p) < alpha ) * c(1:p) )
