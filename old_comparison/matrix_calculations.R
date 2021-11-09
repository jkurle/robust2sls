# trying out different ways of matrix calculations in R
# check whether can reduce the number of dependencies
library(waldo)

set.seed(40)
dx2 <- 2
S <- matrix(stats::runif(dx2^2)*2-1, ncol=dx2)
S2 <- S %*% t(S) # symm
S <- expm::expm(1/2 * expm::logm(S2)) # symm
S_1 <- expm::sqrtm(S2)
S_2 <- pracma::sqrtm(S2)
compare(S, S_1) # only tiny differences
compare(S, S_2$B) # also tiny differences

# rank: all packages plus base package can do it
Matrix::rankMatrix(S)[1] # is 2 as should be
pracma::Rank(S) # 2
matrixcalc::matrix.rank(S) # 2
qr(S)$rank # 2

A <- matrix(c(1,1,1,1), 2, 2)
Matrix::rankMatrix(A)[1] # 1
pracma::Rank(A) # 1
matrixcalc::matrix.rank(A) # 1
qr(A)$rank # 1

# positive definiteness
matrixcalc::is.positive.definite(S) # TRUE
pracma::isposdef(S) # TRUE
eigen(S)$values
pos <- eigen(S)$values > 0
all(pos) # TRUE

test <- matrix(c(1,0,0,0.0000000000001),2,2)
#depends on tolerance
matrixcalc::is.positive.definite(test) # FALSE
pracma::isposdef(test) # TRUE
all(eigen(S)$values > 0) # TRUE
E <- matrix( c( 1, 2, 0, 2, 1, 2, 0, 2, 1 ), nrow=3, byrow=TRUE )
all(eigen(E)$values > 0) # FALSE (as should be)

# invert matrix
Sinv1 <- pracma::inv(S)
Sinv2 <- solve(S)
compare(Sinv1, Sinv2) # same
Sinv3 <- matrixcalc::matrix.inverse(S)
compare(Sinv1, Sinv3) # same
Sinv4 <- Matrix::chol2inv(S)
compare(Sinv1, Sinv4) # quite different
Sinv4 %*% S # is not identity matrix
Sinv1 %*% S # is identity matrix


