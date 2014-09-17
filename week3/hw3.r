
######### Andrew's code 


# #To install package limma:
# source("http://bioconductor.org/biocLite.R")
# biocLite("limma")

#To load Stat8003 data
gdp.all <- read.csv("http://astro.temple.edu/~zhaozhg/Stat8003/data/GDP_Per_Capita.csv")

gdp <- gdp.all[,62]
gdp[ is.na(gdp) ] <- NULL

#*******************************************************
plot( density( gdp, from=0 ), col='green', xlab="gdp", ylab="density", main="HW#3", cex.lab=1.5, cex=2)
x=c(1:max(gdp))


#Problem 3.3 - Assume the data follows a gamma distribution
#Derive the estimators of alpha and beta using MOM

m1 <- mean(gdp)
m2 <- mean(gdp^2)

beta.hat <- (m2 - m1^2) / m1
alpha.hat <- m1 / beta.hat

points( x, dgamma( x, shape=alpha.hat, scale=beta.hat ), 'l', col='red' )


#Problem 3.4

# logalpha.hat <- limma::trigammaInverse(m2 - m1)
# logbeta.hat <- exp(m1 - digamma(logalpha.hat)
