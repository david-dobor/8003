#####  get this data:
pvalue <- read.csv("http://astro.temple.edu/~zhaozhg/Stat8003/data/pvalue.csv", header=T)
X.value <- pvalue[,2]
X.group <- pvalue[,1]

##### the parameters we'll use:
n <- length(X.value)
h <- 1.06 * sqrt( var(X.value)) / (n^(1/5))    #silverman's h

##### xaxis is used for plotting. the normal density estimate will be given at these points:
xaxis <- seq( min(X.value), max(X.value), 0.0001 )
fnorm.hat <- xaxis
for( i in 1:length( xaxis ) )
{
    fnorm.hat[i] <- mean( dnorm( xaxis[i]-X.value, 0, h ))
}

#### The following function called value.of.fnorm.hat.at.x does 2 things:
# 1. finds the closest element to a given x in the xaxis array. 
# 2. returns the value of f.hat at that xaxis.
# COMMENT: Ideally, we would like to evaluate some function f at any given x. However, we only have values of f at the elements of xaxis (xaxis is an array of elements that we generate to plot f - in our case f is fnorm.hat). To evaluate f at any x, not just at an xaxis, we do as follows. We first find the closest element in xaxis to x. We then return the value of f at that xaxis instead of at x. this is close enough to the value we want:
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

# jpeg('plot_prob2_a.jpg')
plot(X.value, f.at.x_i, main="Gaussian Kernel Density Estimate", xlab="x", ylab="density", cex = .3)
# dev.off()


###### part b) estimate the local fdr score
pi0 = 0.7
fdr_score <- vector(mode = "numeric", length = length(X.value))
for( i in 1:length( X.value)) 
{
    fdr_score[i] <- pi0 / f.at.x_i[i]
}

###### part c) calculate total number of falsely classified data
greater_than_half = function(x){
    if( x > 0.5)
        0
    else
        1
}
Z.guess <- sapply(fdr_score,greater_than_half)

falsely_classed <- sum(abs(Z.guess - X.group))


###################################################################
##### part d) leave-one-out cross-validation using the kedd library
library(kedd)
h.cv <- h.mlcv(X.value)$h


### Now repeat parts (a) - (c) using h.cv instead of h:

xaxis <- seq( min(X.value), max(X.value), 0.0001 ) #xaxis used for plotting
## fnorm.hat.cv is the normal kernel density estimate using the new h.cv:
fnorm.hat.cv <- xaxis
for( i in 1:length( xaxis ) )
{
    fnorm.hat.cv[i] <- mean( dnorm( xaxis[i]-X.value, 0, h.cv ))
}

## to estimate the fdr score, we again need the value of f at given data, 
## so we write the followin function, as before (this time without comments):
value.of.fnorm.hat.cv.at.x = function(x) {
    index = which.min(abs(x - xaxis)) #find the closest element in the xaxis array
    fnorm.hat.cv[index]  #return the value of fnorm.hat at that element
}

## before we compute the fdr score, we estimate f at the given data:
f.cv.at.x_i <- vector(mode = "numeric", length = length(X.value))
for( i in 1:length( f.at.x_i ) )
{
    f.cv.at.x_i[i] <- value.of.fnorm.hat.cv.at.x(X.value[i])
}

## finally, we compute the new fdr score
pi0 = 0.7
fdr_score_cv <- vector(mode = "numeric", length = length(X.value))
for( i in 1:length( X.value)) 
{
    fdr_score_cv[i] <- pi0 / f.cv.at.x_i[i]
}

## and, finally-finally, we compute the number of falsely classified data:
Z.guess.cv <- sapply(fdr_score_cv, greater_than_half)

falsely_classed_cv <- sum(abs(Z.guess.cv - X.group))

falsely_classed_cv

