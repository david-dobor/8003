########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
#### Example 7.2.3
alpha <- 0.05
p.hat <- 0.41
N <- 1022
p.Low <- p.hat - qnorm( 1-alpha/2 )* sqrt( p.hat*(1-p.hat))/sqrt(N)
p.Upp <- p.hat + qnorm( 1-alpha/2 )* sqrt( p.hat*(1-p.hat))/sqrt(N)

## Simulate the coverage probability for small p
p <- 0.005
N <- 1022
numSim <- 1000
p.Lows <- rep(0, numSim)
p.Upps <- rep(0, numSim)

for(i in 1:numSim)
  {
    X <- rbinom(N, 1, p )
    p.hat <- mean(X)
    
    p.Lows[i] <- p.hat - qnorm( 1-alpha/2 )* sqrt( p.hat*(1-p.hat))/sqrt(N)
    p.Upps[i] <- p.hat + qnorm( 1-alpha/2 )* sqrt( p.hat*(1-p.hat))/sqrt(N)

  }
cov <- mean( (p>p.Lows)*(p<p.Upps) )


########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
#### Interval using VST and delta methods
alpha <- 0.05
p.hat <- 0.41
N <- 1022

vst.Low <- sin( asin( sqrt(p.hat) ) - qnorm( 1-alpha/2)/2/sqrt(N) )^2
vst.Upp <- sin( asin( sqrt(p.hat) ) + qnorm( 1-alpha/2)/2/sqrt(N) )^2

## Simulate the coverage probability for small p
p <- 0.005
N <- 1022
numSim <- 1000
vst.Lows <- rep(0, numSim)
vst.Upps <- rep(0, numSim)

for(i in 1:numSim)
  {
    X <- rbinom(N, 1, p )
    p.hat <- mean(X)
    
    vst.Lows[i] <- sin( asin( sqrt(p.hat) ) - qnorm( 1-alpha/2)/2/sqrt(N) )^2
    vst.Upps[i] <- sin( asin( sqrt(p.hat) ) + qnorm( 1-alpha/2)/2/sqrt(N) )^2

  }
vst.cov <- mean( (p>vst.Lows)*(p<vst.Upps) )

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
#### Using bootstrap


library(bootstrap)
args(bootstrap)

# Law School Data

lsat <- c(576,635,558,578,666,580,555,661,651,605,653,575,545,572,594)
gpa <- c(3.39,3.30,2.81,3.03,3.44,3.07,3.00,3.43,3.36,3.13,3.12,2.74,2.76,2.88,2.96)
summary(lsat)
summary(gpa)

law.school <- data.frame(lsat,gpa)

#pdf("law_scatter.pdf",width=8,height=6)
plot(lsat,gpa,main="Law School Data", pch = 18, cex = 1.5)
#dev.off()

#pdf("law_histbox.pdf",width=8,height=6)
par(mfrow=c(2,2))
hist(lsat,main='lsat',col='gray')
hist(gpa,main='gpa',col='gray')
boxplot(lsat,main='lsat',col='gray')
boxplot(gpa,main='gpa',col='gray')
#dev.off()

set.seed(49123)

bootstrap(lsat,nboot=10,theta=mean)
lsat.mean.boot <- bootstrap(lsat,nboot=1000,theta=mean)
quantile(lsat.mean.boot$thetastar,c(.025,.975))
lsat.median.boot <- bootstrap(lsat,nboot=1000,theta=median)
quantile(lsat.median.boot$thetastar,c(.025,.975))

# now calculate the correlation

rho = cor(lsat,gpa)
xi = 0.5*log((1+rho)/(1-rho))
rho
xi

rhof = function(x,xdata){
  cor(xdata[x,1],xdata[x,2])
}

xif = function(x,xdata){
  rho = cor(xdata[x,1],xdata[x,2])
  return(0.5*log((1+rho)/(1-rho)))
}    

rhof(1:15,law.school)
xif(1:15,law.school)

lsat.boot1 = bootstrap(1:15,nboot=1000,theta=rhof,xdata=law.school)
hist(lsat.boot1$thetastar)
ci1 = quantile(lsat.boot1$thetastar,c(.025,.975))
ci1

lsat.boot2 = bootstrap(1:15,nboot=1000,theta=xif,xdata=law.school)
xistar = lsat.boot2$thetastar
hist(xistar)
rhostar = (exp(2*xistar)-1)/(exp(2*xistar)+1)
hist(rhostar)
ci21 = quantile(lsat.boot2$thetastar,c(.025,.975))
ci2 = (exp(2*ci21)-1)/(exp(2*ci21)+1)
ci2


# final plot of boostrapped statistics
# pdf("law_bcor.eps",width=8,height=6)
par(mfrow=c(1,2))
hist(lsat.boot2$thetastar, col='gray',main='Bootstrap Correlations')
boxplot(lsat.boot2$thetastar, col='gray')
# dev.off()

par(mfrow=c(2,2))
hist(lsat.boot1$thetastar, probability=TRUE,xlab="rho",col='gray',
     main='Boostrap Correlations (Un-transformed)')
hist(xistar, probability=TRUE, xlab="xi",col='gray',
     main='Boostrap Correlations (Transformed)')
hist(rhostar, probability=TRUE, xlab = "rho",col='gray',
     main='Boostrap Correlations (Transformed)')
boxplot(rhostar,xlab ="rho",col='gray',
        main='Boostrap Correlations (Transformed)')

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
## Example 7.3.1
## Gene 1 (equal variance)

X <- read.table("http://astro.temple.edu/~zhaozhg/Stat8003/data/prostate_X.csv",sep=",")
Y <- read.table("http://astro.temple.edu/~zhaozhg/Stat8003/data/prostate_Y.csv",sep=",")

ci.equalvar <- function(Xi, group, alpha=0.05)
  {
    n1 <- length( which(group==1) )
    n2 <- length( which(group==2) )
    mean1 <- mean(Xi[ group==1] )
    mean2 <- mean(Xi[ group==2] )
    s1 <- var( Xi[group==1] )
    s2 <- var( Xi[group==2] )

    s.pool <- ( (n1-1)*s1 + (n2-1)*s2 )/( n1+n2-2 )

    low <- mean1-mean2 - qt(1-alpha/2, n1+n2-2) * sqrt(s.pool) * sqrt(1/n1+1/n2)
    upp <- mean1-mean2 + qt(1-alpha/2, n1+n2-2) * sqrt(s.pool) * sqrt(1/n1+1/n2)

    list(low=low, upp=upp)
  }

ci.unequalvar <- function(Xi, group,alpha=0.05)
  {
    n1 <- length( which(group==1) )
    n2 <- length( which(group==2) )
    mean1 <- mean(Xi[ group==1] )
    mean2 <- mean(Xi[ group==2] )
    s1 <- var( Xi[group==1] )
    s2 <- var( Xi[group==2] )

    s.unpool <- (s1/n1 +s2/n2 )
    df.sat <- (s1/n1+s2/n2)^2/( (s1/n1)^2/(n1-1) + (s2/n2)^2/(n2-1) )

    low <- mean1-mean2 - qt(1-alpha/2, df.sat) * sqrt( s1/n1 + s2/n2 )
    upp <- mean1-mean2 + qt(1-alpha/2, df.sat) * sqrt( s1/n1 + s2/n2 )

    list(low=low, upp=upp)
  }
## Consider gene 1
X1 <- X[1,]
ci1 <- ci.equalvar( X1, Y, 0.05)

## Gene 2 (unequal variance)
X2 <- X[2,]
ci2 <- ci.unequalvar( X2, Y, 0.05)


ci.variance <- function( Xi, group, alpha=0.05 )
  {
    n1 <- length( which(group==1) )
    n2 <- length( which(group==2) )
    s1 <- var( Xi[group==1] )
    s2 <- var( Xi[group==2] )

    low <- s1/s2*1/qf( 1-alpha/2, n1-1,n2-1)
    upp <- s1/s2*qf( 1-alpha/2, n2-1,n1-1)

    list(low=low,upp=upp)
  }

ci3 <- ci.variance( X2, Y, 0.05 )


########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
#### Example 7.3.3
## Exit poll
poll = read.table( "http://astro.temple.edu/~zhaozhg/Stat8003/data/ExitPoll1.txt", header=TRUE )
x= poll[1:2, 1:2 ]

n1 <- 109+138
n2 <- 152+112

p1.hat <- 109/n1
p2.hat <- 152/n2

center <- p1.hat-p2.hat
moe <- sqrt( p1.hat*(1-p1.hat)/n1 + p2.hat*(1-p2.hat)/n2 )
ci.prop <- list(low= center - qnorm(1-alpha/2)*moe, upp=center+qnorm(1-alpha/2)*moe )

p1.tilde <- 110/(n1+2)
p2.tilde <- 153/(n2+2)

center2 <- p1.tilde-p2.tilde
moe2 <- sqrt( p1.tilde*(1-p1.tilde)/(n1+2) + p2.tilde*(1-p2.tilde)/(n2+2) )
ci.prop.agresti <- list(low= center2 - qnorm(1-alpha/2)*moe2, upp=center+qnorm(1-alpha/2)*moe2 )
