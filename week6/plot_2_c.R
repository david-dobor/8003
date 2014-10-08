library(ggplot2)


## Plotting the 10*log(Gamma) - Gamma
x <- seq(-1, 65, length=1000)
Y <- dgamma(xaxis, shape=10, rate=1)
y <- 10 * log(Y) - Y

# put data in a data frame for ggplot
data <- data.frame(x,y)

# now plot the test statistic
g.test.stat <- ggplot(data, aes(x=data$x,y=data$y) )
g.test.stat <- g.test.stat + geom_line(col="blue", lwd=1.3)

# add the dashed horizontal line 
g.test.stat <- g.test.stat + geom_hline(yintercept=-300) 

# add the vertical lines for x_0 and x_1 (the region of part b, prob 2)
g.test.stat <- g.test.stat + annotate(linetype="dashed", "segment", x=.4, xend=.4, y=-400, yend=-300) +
    annotate(linetype="dashed","segment", x=53, xend=53, y=-400, yend=-300)

# add arrows for the rejection region
library(grid)
g.test.stat <- g.test.stat + annotate("segment", x= 40, xend = 57, y=-90, yend=-375, color="green", size=0.9, arrow=arrow())+
    annotate("segment", x= 35, xend = 0, y=-90, yend=-375, color="green", size=0.9, arrow=arrow()) +
    annotate("text", x=42, y=-70, parse=TRUE, label="These~~Areas~~Should~~Add~~Up~~to~0.05") +
    annotate("text", x=30, y=-290, parse=TRUE, label="Constant~~Level~~k")

# add the title and axes labels
g.test.stat <- g.test.stat +
    xlab("Sum of Observations") +
    ylab("Test Statistic Value") +
    ggtitle("Cut-Off Region for the 0.05 significance level") +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) 


print(g.test.stat)
ggsave("./alpha_cut_off.png", dpi=300)
