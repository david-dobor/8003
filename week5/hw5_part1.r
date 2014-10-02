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

fdr_score <- pi0 / (pi0 + pi1*beta*(1 - X.value)^(beta - 1))
Z.guess <- sapply(fdr_score,greater_than_half)

falsely_classed <- sum(abs(Z.guess - X.group))
