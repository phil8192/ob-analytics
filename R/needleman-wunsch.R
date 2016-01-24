## Copyright (C) 2015,2016 Philip Stubbings <phil@parasec.net>
## Licensed under the GPL v2 license. See LICENSE.md for full terms.

##' Similarity matrix. 
##'
##' Construct a similarity matrix between 2 vectors.
##' 
##' @param a Vector a. 
##' @param b Vector b.
##' @param filter A similarity function. Default: 1 if equal, -1 otherwise.
##' @return A similarity matrix.
##' @author phil
##' @keywords internal
sMatrix <- function(a, b, filter=function(f1, f2) ifelse(f1 == f2, 1, -1)) {
  sapply(b, function(b) filter(b, a))
}

##' Align 2 sequences.
##'
##' Verbatim implementation of Needleman-Wunsch matching algorithm.
##'
##' @param s.matrix Similarity matrix. See sMatrix(...).
##' @param gap Penality assigned to a gap (missing or extra value).
##' @return 2 column matrix. First column = similarity matrix rows
##' (first sequence), Second column = similarity matrix columns
##' (second sequence). Each row maps aligned indexs from each sequence:
##'
##' \preformatted{
##'   > a <- c(2,4,5)
##'   > b <- 1:5
##'   > alignS(sMatrix(a,b))
##'        a b
##'   [1,] 1 2
##'   [2,] 2 4
##'   [3,] 3 5
##' }
##' 
##' @author phil
##' @examples
##' \dontrun{
##'
##' a <- c(2,4,5)
##' b <- 1:5
##' alignS(sMatrix(a, b))
##'
##' }
##' @references \url{https://en.wikipedia.org/wiki/Needleman-Wunsch_algorithm}
##' @keywords internal
alignS <- function(s.matrix, gap=-1) {
  f.matrix <- (function() {
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
  })()
  backtrace <- function() {
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
    
  backtrace()  
}
