library(TeachingDemos)

## Posterior
set.seed(2)
N <- 10000
theta.post <- rbeta( N, 438, 544 )

## postscript("theta_post_draw.eps",horizontal=FALSE)
hist(theta.post,br=100 )
## dev.off()

## postscript("theta_logit.eps",horizontal=FALSE)
hist(log(theta.post/(1-theta.post)),br=100 )
## dev.off()

## postscript("theta_odds.eps",horizontal=FALSE)
hist(theta.post/(1-theta.post),br=100 )
## dev.off()

## Inference of theta
median( theta.post )
quantile( theta.post, c(0.025, 0.975) ) ## Equal-tail interval
emp.hpd(theta.post, conf=0.95) ##  HPD interval
mean( theta.post < 0.485 )


## Inference of logit(theta)
median( log( theta.post/(1-theta.post) ) )
quantile( log( theta.post/(1-theta.post) ), c(0.025, 0.975) )
emp.hpd( log( theta.post/(1-theta.post) ), conf=0.95 )

## Inference of theta/(1-theta)
median( ( theta.post/(1-theta.post) ) )
quantile( ( theta.post/(1-theta.post) ), c(0.025, 0.975) )
emp.hpd(  ( theta.post/(1-theta.post) ), conf=0.95 )
