########## Solutions to Homework 2 ##########
             #### Problem 4 ####
         ## @author: David Dobor ##

## Problem 1
x <- 0:3                          # given support of r.v X
p_x <- c(0.25, 0.125, 0.125, 0.5) # and given its pmf
F_x <- cumsum(p_x)                # compute the cdf of X

#### create a data frame that stores coordinats of the points to plot
coords <- data.frame(x=numeric(), y=numeric()) 
coords <- rbind(coords, c(-2, 0))
coords <- rbind(coords, c(x[1], 0))
for (i in 2:length(x)-1) {
    coords <- rbind(coords, c(x[i], F_x[i]))
    coords <- rbind(coords, c(x[i+1], F_x[i]))
}
coords <- rbind(coords, c(x[length(x)], F_x[length(x)]))
coords <- rbind(coords, c(x[length(x)] + 2, F_x[length(x)]))   


#### now plot the cdf 
g <- ggplot()

odd_inds <- seq(1, nrow(coords), by=2)   #odd indecies- start line segments 
even_inds <- seq(2, nrow(coords), by=2) #even indecies - end line segments

# add the line segments to the plot
g <- g + geom_segment(data=coords, mapping=aes(x=coords[odd_inds,1], y=coords[odd_inds,2], xend=coords[even_inds,1], yend=coords[even_inds,2]))

# add the white circles indicating the points of discontinuity
df <- data.frame(c(0, 1, 2, 3), c(0, F_x[1], F_x[2], F_x[3]))
colnames(df) <- c("x", "y")
g <- g + geom_point(data=df, mapping=aes(x=x, y=y), size=4, shape=21, fill="white")

# add the title and axes labels
g <- g +
    xlab("X") +
    ylab("F(X)") +
    ggtitle("Cmulative Distribution of X")

# set where tick marks appear on the axes
g <- g + scale_y_continuous(breaks=c(0, 0.25, 0.375, 0.5, 1))
g <- g + scale_x_discrete(breaks=c(0, 1, 2, 3))
g <- g + coord_cartesian(xlim=c(-1.5,4.5))

# add a theme for (arguably) better looks
require(ggthemes)
# g <- g + theme_gdocs() 
# ggsave("./cdf_plot_gdocs.png")

# g <- g + theme_economist() #+ scale_color_economist()
# ggsave("./cdf_plot_econ.png")

#g <- g + theme_wsj()
#ggsave("./cdf_plot_wsj.png", width=3.5, height=3.16, dpi=300)
#ggsave("./cdf_plot_wsj.png")

# g <- g + theme_solarized()
# ggsave("./cdf_plot_solarized.png", dpi=300)

# g <- g + theme_igray() 
# ggsave("./cdf_plot_igray.png", dpi=300)


# show the graph
print(g)
ggsave("./cdf_plot.png", dpi=300)
