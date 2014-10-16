#qustion 2
#load the data
har <- read.table("http://astro.temple.edu/~zhaozhg/Stat8003/data/har1.csv", sep=",", header=TRUE)

n1 <- length( har$Pre )
n2 <- length( har$Post )
mean1 <- mean( har$Pre )
mean2 <- mean( har$Post )
s1 <- var( har$Pre )
s2 <- var( har$Post )

# first test if the variances are equal using the F-statistic
F.stat <- s1/s2
if( F.stat >1 ){
    pvalue <- 1-pf( abs( F.stat ), n1-1, n2-1 )
}else{
    pvalue <- 1-pf( abs( 1/F.stat ), n2-1, n1-1 )
}

F.stat
pvalue

## at this point we accept that the variances are equal. Proceed with pooled T-test

# s.pool <- ( (n1-1)*s1 + (n2-1)*s2 )/( n1+n2-2 )
# t.stat <- (mean1-mean2)/(s.pool*sqrt(1/n1+1/n2) )

t.test( har$Post, har$Pre, var.equal=TRUE, , alternative="less", paired=TRUE)


