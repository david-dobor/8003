pvalue <- read.csv("http://astro.temple.edu/~zhaozhg/Stat8003/data/pvalue.csv", header=T)
X.value <- pvalue[,2]
X.group <- pvalue[,1]

## EM algorithm

em = function(X,s) {
  ## W is the data set
  ## s is the parameter vector, where
  ### s[1] : pi0
  ### s[2] : beta

  ## Estimate pi_0^{t+1}
  T0.all = s[1]/(s[1] + (1-s[1])*(s[2]*(1-X)^(s[2]-1)))
  s[1] = mean(T0.all)

  ## Estimate beta^{t+1}
  s[2] = (-1) * sum(1-T0.all) / sum((1-T0.all)*log(1-X))

  s
}


s.old <- c(0.5, 10)
s.new <- s.old
delta <- 0.0001
Delta <- 1
ITR=1

while( Delta> delta ){
  s.new = em(X.value,s.old)
##  Delta= sum( (s.new-s.old)^2 )
  Delta= max( abs(s.new-s.old) )
  ITR=ITR+1
  s.old=s.new
  print( paste(ITR, "-th iteration: pi0=", s.new[1], ", beta=", s.new[2] )  )
}
print( paste("Final estimate: pi0=", s.new[1], ", beta=", s.new[2] )  )


pi0 <- s.new[1]
pi1 <- 1-pi0
beta <- s.new[2]

##Find the local fdr and compare it with the data

greater_than_half = function(x){
    if( x > 0.5)
        0
    else
        1
}

X.estimate <- pi0 / (pi0 + pi1*beta*(1 - X.value)^(beta - 1))
Z.guess <- sapply(X.estimate,greater_than_half)

falsely_classed1 <- sum(abs(Z.guess - X.group))

print(paste("Number of falsely classed : ", falsely_classed1))


##################
##Part 2

##### xaxis is used for plotting. the normal density estimate will be given at these points:
xaxis <- seq( min(X.value), max(X.value), 0.0001 )
fnorm.hat <- xaxis
for( i in 1:length( xaxis ) )
{
    fnorm.hat[i] <- mean( dnorm( xaxis[i]-X.value, 0, h ))
}

value.of.fnorm.hat.at.x = function(x) {
    index = which.min(abs(x - xaxis)) #find the closest element in the xaxis array
    fnorm.hat[index]  #return the value of fnorm.hat at that element
}


# now we are ready to estimate f at any x, including the x_i:
f.at.x_i <- vector(mode = "numeric", length = length(X.value))
for( i in 1:length( f.at.x_i ) )
{
    f.at.x_i[i] <- value.of.fnorm.hat.at.x(X.value[i])
}

jpeg('plot_prob2_a.jpg')
plot(X.value, f.at.x_i, main="Gaussian Kernel Density Estimate", xlab="x", ylab="density", cex = .3)
dev.off()


pi0 <- 0.7
n <- length( X.value )
h <- 1.06 * sqrt( var(X.value) ) / (n^(1/5))

k_estimate = function(x){
    1/(h) * mean( dnorm( (x - X.value)/h, 0, h))
}

X.kdestimate <- pi0 / sapply(X.value,k_estimate)

## jpeg('plot_prob2_a2.jpg')
## plot(X.value, sapply(X.value,k_estimate), main="Gaussian Kernel Density Estimate", xlab="x", ylab="density", cex = .3)
## dev.off()


Z.guess.kde <- sapply(X.kdestimate,greater_than_half)

falsely_classed2 <- sum(abs(Z.guess.kde - X.group))

print(paste("Number of falsely classed for Silverman's h : ", falsely_classed2))

library(kedd)

h <- h.mlcv(X.value)$h

X.kdestimate.cv <- pi0 / sapply(X.value,k_estimate)
Z.guess.kde.cv <- sapply(X.kdestimate.cv,greater_than_half)

falsely_classed3 <- sum(abs(Z.guess.kde.cv - X.group))

print(paste("Number of falsely classed for cross validated h : ", falsely_classed3))

print(paste("Falsely classed data counts: Trial 1 : ", falsely_classed1, " Trial 2 : ", falsely_classed2, " Trial 3 : ", falsely_classed3))
