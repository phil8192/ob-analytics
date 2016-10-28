## Copyright (C) 2015,2016 Philip Stubbings <phil@parasec.net>
## Licensed under the GPL v2 license. See LICENSE.md for full terms.

vectorDiff <- function(v) c(0, tail(v, -1) - head(v, -1))

reverseMatrix <- function(m) m[rev(1:nrow(m)), ]

norml <- function(v, minv=min(v), maxv=max(v)) (v-minv)/(maxv-minv)

toZoo <- function(v) zoo(v[, -which(colnames(v) == "timestamp")], v$timestamp)

intervalSumBreaks <- function(v, breaks) {
  cs <- cumsum(v)
  intervals <- cs[breaks]
  c(head(intervals, 1), tail(intervals, -1) - head(intervals, -1)) 
}

vwap <- function(price, volume) as.numeric(price %*% volume / sum(volume))

intervalVwap <- function(price, volume, breaks) 
  intervalSumBreaks(price*volume, breaks) /
      intervalSumBreaks(volume, breaks)

# similarly, can define a cumulative definition of price.level.density:
intervalPriceLevelGaps <- 
  function(volume, breaks) intervalSumBreaks(ifelse(volume == 0, 1, 0), 
      breaks)

