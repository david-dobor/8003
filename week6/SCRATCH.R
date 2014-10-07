pvalue <- read.csv("http://astro.temple.edu/~zhaozhg/Stat8003/data/pvalue.csv", header=T)
X.value <- pvalue[,2]
X.group <- pvalue[,1]


##Part 2
xaxis <- seq( min(X.value), max(X.value), 0.001 )

pi0 <- 0.7
n <- length( X.value )
h <- 1.06 * sqrt( var(X.value) ) / (n^(1/5))

k_estimate = function(x){
    1/(h) * mean( dnorm( (x - X.value)/h, 0, h))
}


greater_than_half = function(x){
    if( x > 0.5)
        0
    else
        1
}


X.kdestimate <- pi0 / sapply(X.value,k_estimate)
Z.guess.kde <- sapply(X.kdestimate,greater_than_half)

falsely_classed2 <- sum(abs(Z.guess.kde - X.group))
