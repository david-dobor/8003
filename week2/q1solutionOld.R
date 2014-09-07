#### Solutions to Homework 2 ####

## Problem 1
x <- 0:3   # support of r.v X
p_x <- c(0.25, 0.125, 0.125, 0.5) # pmf of r.v X
# to get the cdf, can use the partial sums of pdf
F_x <- cumsum(p_x)

# sum(p_x) == 1  #quick check that it's a true pmf

#write my own plotting function to get the basic look of a step function:
plotMyCdf <- function(x, F_x ){
    plot(x,F_x, type='n', xlim=c(-1, length(F_x) + 1) )
    segments(x[-length(x)], F_x[-length(F_x)], x[-1], F_x[-length(F_x)], lwd=3)
    segments(c(-1, x[]), c(0, F_x[]), c(x[], x[length(x)]+2), c(0, F_x[]), lwd=3, col="blue")
    points(x, F_x, pch=19, col="blue")

    title(expression(paste("The CDF of ", X)))

}


#now plot ans save it:
png(filename="./hw2q1_cdf.png")
plotMyCdf(x, F_x)
dev.off()








