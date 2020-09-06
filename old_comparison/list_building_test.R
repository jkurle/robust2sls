
res <- list(m0 = 1:5, m1 = 6:10, m2 = 11:15, m3 = 16:20, m4 = 21:25)
stdres <- list(m0 = (1:5)/2, m1 = (6:10)/2, m2 = (11:15)/2, m3 = (16:20)/2, m4 = (21:25)/2)

output <- list(res = res, stdres = stdres)
output2 <- list("res" = res, "stdres" = stdres)
identical(output, output2) # TRUE; can create name via character string

iteration <- "m0"
res <- list(iteration = 1:5) # does not work, cannot name via a variable

# res <- list(eval(iteration) = 1:5) # error
# res <- list(substitute(iteration) = 1:5) # error

# how update a list that grows with each iteration?
res <- list()
res[[iteration]] <- 1:5 # has m0 so at least this seems to work
iteration <- "m1"
res[[iteration]] <- 6:10 # has new element m1 so can grow the list in this way

# can use the output from selection() to update the lists?

out <- list(cons = list(), model = list(), res = list(), stdres = list(),
            sel = list(), type = list())

iteration <- "m0"
update_info0 <- list(res = 1:5, stdres = 6:10, sel = logical(length=5),
                     type = c(-1,1,1,0,1))
out$res[[iteration]] <- update_info0$res
out$stdres[[iteration]] <- update_info0$stdres
out$sel[[iteration]] <- update_info0$sel
out$type[[iteration]] <- update_info0$type

iteration <- "m1"
update_info1 <- list(res = (1:5)/2, stdres = (6:10)/2, sel = !logical(length=5),
                     type = c(-1,0,1,0,0))
out$res[[iteration]] <- update_info1$res
out$stdres[[iteration]] <- update_info1$stdres
out$sel[[iteration]] <- update_info1$sel
out$type[[iteration]] <- update_info1$type

# capture call test
fprint <- function(x) {
  a <- sys.call()
  print(x)
  return(a)
}

# test seq_len()

l <- list()
iteration <- 0

for (i in 1:iteration) {
  l[i] <- i
}

for (i in seq_len(iteration)) {
  l[i] <- i
}


