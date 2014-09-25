
# part e) of problem 3 from hw4

# beta_0 = 15.0429
# beta_1 = -0.2322 

# 
beta_0 = 9.0211846741774
beta_1 = -0.154296115305373
temp <- 31  # temperature during launch
y = beta_0 + beta_1 * temp
p = exp(y) / (1 + exp(y))
p


### part f), same problem

######## plotting the data 
beta_0 = 9.0211846741774
beta_1 = -0.154296115305373
shuttle <- read.csv("http://astro.temple.edu/~zhaozhg/Stat8003/data/shuttle.txt")
mean( shuttle$ndo /2 )

###### see what temperature against number of damaged rings looks like:
plot( shuttle$temp, shuttle$ndo)


####### plot probability against temperature
y <- beta_0 + beta_1 * shuttle$temp
p = exp(y) / (1 + exp(y))
plot( shuttle$temp, p)

# now plot the same for the needed temperature range:
temp.range <- c(30:90)
y <- beta_0 + beta_1 * temp.range
p = exp(y) / (1 + exp(y))

jpeg('./prob_vs_temp_plot_challenger.jpg')
plot( temp.range, p, xlab="Temperature", ylab="Failure Probability", main="Challenger Data")

dev.off()
