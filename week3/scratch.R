# Problem # 1
E.X <- 1*.4 + 2*.6    # first moment of X
E.Y <- 2*.4 + 3*.6    # first moment of Y

m.2.x <- 1*.4 + 4*.6  # second moment of X
m.2.y <- 4*.4 + 9*.6  # second moment of Y

VarX <- m.2.x - (E.X)^2
VarY <- m.2.y - (E.Y)^2

E.X.Y <- 1*2*.4 + 2*3*.3
Cov <- E.X.Y - E.X*E.Y
Cov/(sqrt(VarX*VarY))
sqrt(4)
