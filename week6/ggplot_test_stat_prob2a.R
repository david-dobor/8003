# plots the test statistic for problem 2, hwk 6
library(ggplot2)

# put the data to plot in a data frame (cause that's what ggplot2 wants)
x <- seq(0, 60, .01)
y <- x * exp(-0.1*x)  # the peak is exaggerated for better looks; this should really be x * exp(-x)
data <- data.frame(x,y)

# now plot the test statistic
g.test.stat <- ggplot(data, aes(x=data$x,y=data$y))
g.test.stat <- g.test.stat + geom_line()

# add the horizontal line (x axis) and another line just above it
g.test.stat <- g.test.stat + geom_hline(yintercept=0) + geom_hline(linetype="dashed", yintercept=2)

# add the vertical lines for x_0 and x_1 (the region of part b, prob 2)
g.test.stat <- g.test.stat + annotate(linetype="dashed","segment", x=2.6, xend=2.6, y=0, yend=2) +
                             annotate(linetype="dashed","segment", x=25.4, xend=25.4, y=0, yend=2)

# add a shaded rectangle
g.test.stat <- g.test.stat + annotate("rect", xmin=2.61, xmax=25.39, ymin=-1.9, ymax=4.6, alpha=.1)

# add some text annotation
g.test.stat <- g.test.stat + annotate("text", x=2.6, y=-0.3, parse=TRUE, label="x[0]") +
                             annotate("text", x=25.5, y=-0.3, parse=TRUE, label="x[1]") +
                             annotate("text", x= 20, y = 3.4, parse=TRUE, label="bar(X) * e^-bar(X)")


# add arrows for the rejection region
library(grid)
g.test.stat <- g.test.stat + annotate("segment", x= 25.5, xend = 40, y=.2, yend=.2, color="blue", size=1.5, arrow=arrow()) +
    annotate("segment", x= 2.48, xend = -1.5, y=.2, yend=.2, color="blue", size=1.5, arrow=arrow()) +
    annotate("text", x=40, y=-1, parse=TRUE, label="Reject~~Null~~Outside") +
    annotate("text", x=40, y=-1.5, parse=TRUE, label="of~~the~~Shaded~~Region") 

# add the title and axes labels
g.test.stat <- g.test.stat +
    xlab("Average X") +
    ylab("Test Statistic Value") +
    ylim(-2,4.6) +
    ggtitle("The Rejection Region") +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) 


print(g.test.stat)
ggsave("./rejection_region_plot.png", dpi=300)