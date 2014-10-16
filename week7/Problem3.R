# Problem 3, hwk7.
# @author David Dobor
# <insert link to the problem text>

#############################################################################
# function test.hyps - tests p hypotheses on a batch of 50 random normals 
#                      at alpha level of significance each. Each null hypothesis
#                      is whether the mean of these 50 numbers is zero. 
#
# INPUT:  p     - the number of hypotheses to test 
#         alpha - the significance level at which to test the hypothesis
# OUTPUT: a vector of length p (test.results) containing 1 in the i-th
#         position if hypothesis i was rejected and 0 if it was accepted

test.hyps <- function(p, alpha) {
    cutoff <- qnorm(1-alpha/2) / sqrt(50)  #=0.2771808 when alpha=0.05
    test.result <- rep(0,p)  # initialize the results to zeros
    for (h in 1:p){ # for each hypothesis to test
        X.bar <- mean( rnorm(50) )  # generate 50 standard normals
        if ( abs(X.bar) >= cutoff )  {
            # print("REJECT NULL")
            test.result[h] = 1   # put a 1 if it's a false rejection
        } else{
            # print("accept null")
        }  
    }
    return(test.result)
}
##############################################################################


################################## part (a) ##################################
alpha = 0.05
test.result <- test.hyps(1, alpha)  # test 1 hypothesis on a batch of 50 normals
#test.result 

n.sim = 1000                   # repeat the test this many times
sim.results <- rep(0,n.sim)    # store each result in this vector
for (i in 1:n.sim) {
    test.result <- test.hyps(1, alpha)
    sim.results[i] = test.result 
}
mean(sim.results)     # example run: 0.044 - the fraction of rejections out 
                      #              of n.sim (out of 1000)

################################## part (b) ##################################
p = 10
alpha = 0.05
test.result <- test.hyps(p, alpha)     # test 10 hypothesis on a batch of 50 normals
#test.result 

n.sim = 1000                              # repeat the tests this many times
sim.results <- matrix(0, nrow=n.sim, ncol=p)  # store each result in matrix                                    
for (i in 1:n.sim) {
    test.result <- test.hyps(p, alpha)  
    sim.results[i,] = test.result 
}

#now, summing the rows we can see how many hypotheses got rejected for each simulation
rejections <- rowSums(sim.results)
num.zero.rejections <- sum(rejections == 0)
#family wise error rate:
FWER <- 1 - num.zero.rejections/n.sim
FWER   # output from an example run: 0.421


################################## part (c) ##################################
# the only difference here from part (b) is the number of hypotheses tested
p = 100
alpha = 0.05
test.result <- test.hyps(p, alpha) # test 100 hypothesis on a batch of 50 normals
#test.result 

n.sim = 1000                              # repeat the tests this many times
sim.results <- matrix(0, nrow=n.sim, ncol=p)  # store each result in matrix                                    
for (i in 1:n.sim) {
    test.result <- test.hyps(p, alpha) 
    sim.results[i,] = test.result 
}

#now, summing the rows we can see how many hypotheses got rejected for each simulation
rejections <- rowSums(sim.results)
num.zero.rejections <- sum(rejections == 0)
#family wise error rate:
FWER <- 1 - num.zero.rejections/n.sim
FWER   # output from an example run: 0.995

################################## part (d) ##################################
# here we just set alpha sinificance level to something much smaller so we can
# control FWER
p = 100
alpha = 0.05/p
test.result <- test.hyps(p, alpha) # test 100 hypothesis on a batch of 50 normals
#test.result 

n.sim = 1000                              # repeat the tests this many times
sim.results <- matrix(0, nrow=n.sim, ncol=p)  # store each result in matrix                                    
for (i in 1:n.sim) {
    test.result <- test.hyps(p, alpha) 
    sim.results[i,] = test.result 
}

#now, summing the rows we can see how many hypotheses got rejected for each simulation
rejections <- rowSums(sim.results)
num.zero.rejections <- sum(rejections == 0)
#family wise error rate:
FWER <- 1 - num.zero.rejections/n.sim
FWER   # output from an example run: 0.045
