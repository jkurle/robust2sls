# trying out different ways of matrix calculations in R
# check whether can reduce the number of dependencies
library(waldo)

# power of matrix: expm, pracma
set.seed(40)
dx2 <- 2
S <- matrix(stats::runif(dx2^2)*2-1, ncol=dx2)
S2 <- S %*% t(S) # symm
S <- expm::expm(1/2 * expm::logm(S2)) # symm
S_1 <- expm::sqrtm(S2)
S_2 <- pracma::sqrtm(S2)
compare(S, S_1) # only tiny differences
compare(S, S_2$B) # also tiny differences

# rank: base, pracma, matrixcalc, Matrix
Matrix::rankMatrix(S)[1] # is 2 as should be
pracma::Rank(S) # 2
matrixcalc::matrix.rank(S) # 2
qr(S)$rank # 2

A <- matrix(c(1,1,1,1), 2, 2)
Matrix::rankMatrix(A)[1] # 1
pracma::Rank(A) # 1
matrixcalc::matrix.rank(A) # 1
qr(A)$rank # 1

# positive definiteness: base, matrixcalc, pracma
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

# invert matrix: base, pracma, matrixcalc, Matrix
Sinv1 <- pracma::inv(S)
Sinv2 <- solve(S)
compare(Sinv1, Sinv2) # same
Sinv3 <- matrixcalc::matrix.inverse(S)
compare(Sinv1, Sinv3) # same
Sinv4 <- Matrix::chol2inv(Matrix::chol(S))
compare(Sinv1, Sinv4) # only slight difference but maybe even better?
Sinv4 %*% S # is identity matrix
Sinv1 %*% S # is basically identity matrix

# infos as of 9 Nov 2021
# pracma: v2.3.3, last updated 23 Jan 2021
# matrixcalc: v1.0-5, last updated 28 Jul 2021
# expm: v0.999-6, last update 13 Jan 2021
# Matrix: v1.3-4, last update 1 Jun 2021

# both expm and Matrix have developers from R core team in their dev team
# Matrix even has "Priority: recommended" (so recommendation to have installed)
# unfortunately, Matrix cannot do all of the tasks
# instead use pracma for all
