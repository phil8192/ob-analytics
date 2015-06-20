# Needleman-Wunsch optimal matching algorithm
#  
# http://en.wikipedia.org/wiki/Needleman%E2%80%93Wunsch_algorithm
#
# hard to vectorise f-matrix since cells are interdependent.
# hard to vectorise (iterative) backtrace step
# todo: rcpp. (http://adv-r.had.co.nz/Rcpp.html)

s.matrix <- function(a, b, filter=function(f1, f2) ifelse(f1 == f2, 1, -1)) {
  sapply(b, function(b) filter(b, a))
}

f.matrix <- function(s.matrix, gap=-1) {
  s.len <- nrow(s.matrix)
  q.len <- ncol(s.matrix)
  f <- matrix(0, nrow=s.len+1, ncol=q.len+1)
  f[, 1] <- (0:s.len)*gap
  f[1, ] <- (0:q.len)*gap
  for(i in 2:(s.len+1)) {
    for (j in 2:(q.len+1)) {
      f[i, j] <- max(f[i-1, j-1]+s.matrix[i-1, j-1], f[i-1, j]+gap, 
          f[i, j-1]+gap)
    }
  }
  f   
}

backtrace <- function(f.matrix, s.matrix, gap=-1) {
  res <- NULL
  i <- nrow(s.matrix)+1
  j <- ncol(s.matrix)+1
  while(i>1 || j>1) {
    if(i>1 && j>1 && f.matrix[i, j] == f.matrix[i-1, j-1] + 
        s.matrix[i-1, j-1]) {
      i <- i-1
      j <- j-1
      res <- rbind(c(i, j), res)         
    } else if(i>1 && f.matrix[i, j] == f.matrix[i-1, j]+gap) {
      i <- i-1
    } else if(j>1 && f.matrix[i, j] == f.matrix[i, j-1]+gap) {
      j <- j-1
    }
  }
  colnames(res) <- c("a", "b")
  res 
}

align.s <- function(s.matrix) {
  backtrace(f.matrix(s.matrix), s.matrix)  
}

# align vector a with b.
# returns a matrix representing an index alignment between a and b.
# unmatched indices are excluded.
align <- function(a, b) {
  align.s(s.matrix(a, b))
}

