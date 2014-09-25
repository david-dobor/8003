## Newton Raphson algorithm for the solve MLE of shuttle problem.

shuttle <- read.csv("http://astro.temple.edu/~zhaozhg/Stat8003/data/shuttle.txt")

## Initial value
beta0 <- 15
beta1 <- -0.23

theta.old <- matrix( c(beta0,beta1), 2, 1 ) ## This is a vector for our betas
theta.new <- theta.old
delta <- 0.0001
Delta <- 1
itr <- 1

while(Delta > delta )
  {
    theta.old <- theta.new

    a <- (-2) * sum( ( exp(theta.old[1] + theta.old[2]*shuttle$temp ) / ((1 + exp(theta.old[1] + theta.old[2]*shuttle$temp ))^2 )))
    b <- (-2) * sum( ( shuttle$temp *  exp(theta.old[1] + theta.old[2]*shuttle$temp ) / ((1 + exp(theta.old[1] + theta.old[2]*shuttle$temp ))^2 )))
    c <- (-2) * sum( ( shuttle$temp *  exp(theta.old[1] + theta.old[2]*shuttle$temp ) / ((1 + exp(theta.old[1] + theta.old[2]*shuttle$temp ))^2 )))
    d <- (-2) * sum( ( (shuttle$temp*shuttle$temp) *  exp(theta.old[1] + theta.old[2]*shuttle$temp ) / ((1 + exp(theta.old[1] + theta.old[2]*shuttle$temp ))^2 )))
    
    Jacobian <- matrix( c( a, c, b, d ), 2, 2) ### Calculate the Jacobian matrix

    JacobianInv <- 1/(a*d - c*b) * matrix( c( d, -c, -b, a ), 2, 2)
    
    f.value <- matrix( c(
        sum( shuttle$ndo ) - 2*sum( (exp( theta.old[1] + theta.old[2]*shuttle$temp ) / (1 + exp( theta.old[1] + theta.old[2]*shuttle$temp)))),
        sum( shuttle$ndo*shuttle$temp ) - 2*sum( (shuttle$temp*exp( theta.old[1] + theta.old[2]*shuttle$temp ) / (1 + exp( theta.old[1] + theta.old[2]*shuttle$temp))))), 2, 1) ### calculate the value of the f function
    
    theta.new <- theta.old - solve( Jacobian ) %*% f.value ### Newton raphson updates
#    theta.new <- theta.old - JacobianInv %*% f.value ### Newton raphson updates
    
    Delta <- sum( (theta.new-theta.old)^2 )
    print( paste("Iter:", itr,", beta0= ", theta.new[1], ",beta1=", theta.new[2] , sep=" ") )
    itr <- itr+1
  }
beta0.mle <- theta.new[1]
beta1.mle <- theta.new[2]

