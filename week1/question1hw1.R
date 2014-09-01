# Solution to Question1, Homework 1

#given matrix A:
col1 <- c(1,2,3)
A <- cbind(col1, 2*col1, 3*col1)
#and matrix B:
B <- matrix((c(2, 1, -2, 0, 3, 1)), ncol = 2, nrow = 3)

#compute the following prodcts:
A %*% B
t(B) %*% A

