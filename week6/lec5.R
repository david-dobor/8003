########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
## EM algorithm
X <- faithful$waiting
em = function(X,s) {
  ## W is the data set
  ## s is the parameter vector, where
  ### s[1] : pi0
  ### s[2] : mu0
  ### s[4]: sigma0^2
  ### s[3]: mu1
  ### s[5]: sigma1^2

  ## Estimate pi_0^{t+1}
  T0.all = s[1]*dnorm(X, s[2], sqrt(s[4]))/(s[1]*dnorm(X, s[2], sqrt(s[4])) + (1-s[1])*dnorm(X, s[3], sqrt(s[5])) )
  s[1] = mean(T0.all)

  ## Estimate mu0 and mu1
  s[2] = sum(T0.all*X) / sum(T0.all)
  s[3] = sum((1-T0.all)*X) / sum(1-T0.all)

  ## Estiamte sigma0^2 and sigma1^2
  s[4] = sum(T0.all*(X-s[2])^2) / sum(T0.all)
  s[5] = sum((1-T0.all)*(X-s[3])^2) / sum(1-T0.all)
  s
}


s.old <- c(0.5, 40, 90, 16, 16)
s.new <- s.old
delta <- 0.0001
Delta <- 1
ITR=1

while( Delta> delta ){
  s.new = em(X,s.old)
  Delta= sum( (s.new-s.old)^2 )
  Delta= max( abs(s.new-s.old) )
  ITR=ITR+1
  s.old=s.new
  print( paste(ITR, "-th iteration: pi0=", s.new[1], ", mu0=", s.new[2], ",sigma0=", sqrt( s.new[4]), ",mu1=", s.new[3], ",sigma1=", sqrt( s.new[5]) )  )
}
print( paste("Final estiamte: pi0=", s.new[1], ", mu0=", s.new[2], ",sigma0=", sqrt( s.new[4]), ",mu1=", s.new[3], ",sigma1=", sqrt( s.new[5]) )  )


pi0 <- s.new[1]
pi1 <- 1-pi0
mu0 <- s.new[2]
sigma0 <- sqrt( s.new[4] )
mu1 <- s.new[3]
sigma1 <- sqrt( s.new[5] )


## Plot the histogram, density estimator and the em algorithm.
xaxis <- seq( 40, 100, 0.1 )
ds <- density(X)
hist( X, freq=F, br=40,  main="Waiting time", xlab="Waiting time", ylab="density", cex.main=2, cex.lab=1.5)
points( ds$x, ds$y, 'l', col='red', lwd=3)
points(  xaxis, pi0*dnorm( xaxis, mu0, sigma0 )+ pi1*dnorm( xaxis, mu1, sigma1), 'l', col='green', lwd=3)
legend(50, 0.05, c("density", "EM algorithm"), lty=c(1, 1), col=c('red', 'green'), lwd=c(3,3), cex=2 )


########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
## Kernel density estimation
x <- c(1,2,3)
xaxis <- seq(0, 4, 0.01)
h <- 0.4
plot( xaxis, dnorm( xaxis, x[1], h), 'l', col='red', ylim=c(0,1.1) )
points( xaxis, dnorm( xaxis, x[2], h), 'l', col='green' )
points( xaxis, dnorm( xaxis, x[3], h), 'l', col='blue' )
points( xaxis, dnorm(xaxis, x[1],h)+dnorm(xaxis,x[2],h )+ dnorm(xaxis,x[3],h), 'l', color='black')





###############
## KDE for the faithful dataset


X <- faithful$eruptions
n <- length(X)
h <- 1.06 * sqrt( var(X) ) / (n^(1/5))
##########################
## Assume the uniform kernel
xaxis <- seq( min(X), max(X), 0.01 )
funif.hat <- xaxis
for( i in 1:length( xaxis) )
  {
    funif.hat[i] <- sum( abs( xaxis[i] - X)/h <= 1 )/(2*n*h)
  }

### Assume the normal kernel
fnorm.hat <- xaxis
for( i in 1:length( xaxis ) )
  {
    fnorm.hat[i] <- mean( dnorm( xaxis[i]-X, 0, h ))
  }


postscript( "figure/gyser_kde_unif_norm.eps", horizontal=FALSE)


hist( X, freq=F, br=40,  main="Eruptions", xlab="Eruptions", ylab="density", cex.main=2, cex.lab=1.5, ylim=c(0, 1.2) )
points( xaxis, funif.hat, 'l', col='red')
points(xaxis, fnorm.hat, 'l', col='green')

dev.off()

########################################################################################################################################################
########################################################################################################################################################
## Effect of the bandwidth
h.small=0.05
fnorm.small.h.hat <- xaxis
for( i in 1:length( xaxis ) )
  {
    fnorm.small.h.hat[i] <- mean( dnorm( xaxis[i]-X, 0, h.small ))
  }

h.large=1
fnorm.large.h.hat <- xaxis
for( i in 1:length( xaxis ) )
  {
    fnorm.large.h.hat[i] <- mean( dnorm( xaxis[i]-X, 0, h.large ))
  }

postscript("figure/geyser_kde_small_h.eps", horizontal=FALSE)
plot( xaxis, fnorm.small.h.hat, main=paste("h=",h.small, sep=""), xlab="Eruptions", ylab="density", cex.main=2, cex.lab=1.5, 'l' )
dev.off()

postscript("figure/geyser_kde_optimal_h.eps", horizontal=FALSE)
plot( xaxis, fnorm.hat, main=paste("h=",h, sep=""), xlab="Eruptions", ylab="density", cex.main=2, cex.lab=1.5, 'l' )
dev.off()


postscript("figure/geyser_kde_large_h.eps", horizontal=FALSE)
plot( xaxis, fnorm.large.h.hat, main=paste("h=",h.large, sep=""), xlab="Eruptions", ylab="density", cex.main=2, cex.lab=1.5, 'l' )
dev.off()


## Choose the bandwidth according tothe maximum likelihood cross validation
library(kedd)
h.cv <- h.mlcv(X)$h
fnorm.cv.hat <- xaxis
for( i in 1:length( xaxis ) )
  {
    fnorm.cv.hat[i] <- mean( dnorm( xaxis[i]-X, 0, h.cv ))
  }

postscript("figure/geyser_kde_cv_h.eps", horizontal=FALSE)
hist( X, freq=F, br=40,  main="Eruptions", xlab="Eruptions", ylab="density", cex.main=2, cex.lab=1.5, ylim=c(0, 1.2) )
points( xaxis, fnorm.hat, main=paste("h=",h, sep=""), xlab="Eruptions", ylab="density", cex.main=2, cex.lab=1.5, 'l', col='red', lwd=3 )
points( xaxis, fnorm.cv.hat, main=paste("h=",h.cv, sep=""), xlab="Eruptions", ylab="density", cex.main=2, cex.lab=1.5, 'l', col='green',lwd=3 )
legend(2.5, 1.0, c("Silverman", "Cross-validation"), lty=c(1, 1), col=c('red', 'green'), lwd=c(3,3), cex=2 )
dev.off()

res <- density( X, bw=h.cv, kernel="gaussian", from=min(X), to=max(X) )
plot(res)
points(xaxis, fnorm.cv.hat, 'l', col='green' )


########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
## Empirical cdf
X <- faithful$waiting
xaxis <- seq( min(X), max(X), 0.01)
emp.cdf <- xaxis
EM.cdf <- xaxis

for(i in 1:length( xaxis ) )
  {
    emp.cdf[i] <- mean( X <= xaxis[i] )
    EM.cdf[i] <- pi0 * pnorm( xaxis[i], mu0, sigma0 ) + pi1 * pnorm( xaxis[i], mu1, sigma1 )
  }
## Recall the EM estiamtes

postscript("figure/emp_cdf.eps", horizontal=FALSE)
plot( xaxis, emp.cdf, main="Waiting", xlab="Waiting", ylab="cdf", cex.main=2, cex.lab=1.5,'l', lwd=3, col='red' )
points( xaxis, EM.cdf, 'l', col='green', lwd=3  )
legend(50, 0.8, c("Empirical cdf", "cdf based on EM"), lty=c(1, 1), col=c('red', 'green'), lwd=c(3,3), cex=2 )
dev.off()

