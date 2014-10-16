# problem 2 c) 
library(ggplot2)

xaxis <- seq(0,5, 0.01)
power <- 1 - pnorm(1.96 - xaxis) + pnorm(-1.96 - xaxis)
data <- data.frame(xaxis, power)

g.power <- ggplot(data, aes(x=data$xaxis, y=data$power))
g.power <- g.power + geom_line( size=1.3 )

# add the title and axes labels
g.power <- g.power  +
    xlab("Absolute Change in Mean, Standardized") +
    ylab("Power of Test") +
    ggtitle("Power Calculation") 


print(g.power)

ggsave("./power_plot_2c.png", dpi=300)



# xaxis <- seq(0,5, 0.01)
# power <- 1 - pnorm(1.96 - xaxis) + pnorm(-1.96 - xaxis)
# plot( xaxis, power, 'l', main="Power Calculation", xlab="absolute change in mean, standardized", ylab="Power", cex.main=1.5, cex.lab=1.5, col='blue',lwd=3 ,xlim=c(0, 5))
# xaxis <- -xaxis
# lines(xaxis, power, col='blue', lwd=3)

