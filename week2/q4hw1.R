########## Solutions to Homework 2 ##########
             #### Problem 4 ####
         ## @author: David Dobor ##

### first, simply plot the density: pdf 3*x^2*1(0,1).
library(ggplot2)
x <- 0:100/100
y <- 3*x^2
data <- data.frame(x,y)
g_densty <- ggplot(data, aes(x=data$x,y=data$y))
g_densty <- g_densty + geom_line()
# add line segments to the left of 0 and to the right of 1 on the x axis:
g_densty <- g_densty + geom_segment(mapping=(aes(x=1, y=0, xend=1.5, yend=0))) 
g_densty <- g_densty + geom_segment(mapping=(aes(x=-0.5, y=0, xend=0, yend=0)))
# add the title and axes labels
g_densty <- g_densty +
            xlab("X") +
            ylab("f(X)") +
            ggtitle("Probablity Density Function of X")
print(g_densty)
# save the plots
ggsave("./density.png", dpi=300)

### next, generate random samples from the distr with pdf 3*x^2*1(0,1).
### To generate these guys, first compute the cdf and find its inverse.
### Then generate uniform r.v.s and transform them with the inverse cdf.
### cdf(x) = u = x^3 => inverse_cdf(u) = u^(1/3) 
n <- 10000
my_rands <- runif(n)^(1/3)
hist(my_rands)
g_hist <- ggplot()
g_hist <- g_hist + geom_histogram(aes(x=my_rands),  fill="slategray2", color="grey60",size=0.2)
g_hist <- g_hist + coord_cartesian(xlim=c(-0.5,1.5))
# add the title and axes labels
g_hist <- g_hist +
    xlab("X") +
    ylab("Frequency") +
    ggtitle("Histogram of 10, 000 random variates of X")
print(g_hist)

### finally, save the plots
ggsave("./histogram.png", dpi=300)


require(ggthemes)
#g_hist <- g_hist + theme_economist()
#ggsave("./histogram_economoist.png", dpi=300)

# g_densty <- g_densty + theme_economist()
# print(g_densty)
