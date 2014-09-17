
######### Andrew's code with minor midifications #########


# #To install package limma:
# source("http://bioconductor.org/biocLite.R")
# biocLite("limma")

#To load Stat8003 data
gdp.all <- read.csv("http://astro.temple.edu/~zhaozhg/Stat8003/data/GDP_Per_Capita.csv")

gdp <- gdp.all[,62]
gdp[ is.na(gdp) ] <- NULL

#*******************************************************
jpeg('./gdpfit.jpg')
plot( density( gdp, from=0 ), col='green', xlab="GDP", ylab="Density", main="GDP Data", cex.lab=1.5, cex=2)
x=c(1:max(gdp))


#Problem 3.3 - Assume the data follows a gamma distribution
#Derive the estimators of alpha and beta using MOM

m1 <- mean(gdp)
m2 <- mean(gdp^2)

beta.hat <- (m2 - m1^2) / m1
alpha.hat <- m1 / beta.hat

points( x, dgamma( x, shape=alpha.hat, scale=beta.hat ), 'l', col='red' )
legend(50000, 3e-05, c("density", "fitted density"), lty=c(1, 1), col=c('green', 'red'), cex=2 )
dev.off()

#Problem 3.4

new.alpha.hat <- limma::trigammaInverse(m2 - m1^2)
new.beta.hat <- exp(m1 - digamma(new.alpha.hat))
new.log.of.beta <- m1 - digamma(new.alpha.hat)
m1 - digamma(new.alpha.hat)
