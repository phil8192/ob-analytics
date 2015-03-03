vector.diff <- function(v) c(0, tail(v, -1) - head(v, -1))
reverse.matrix <- function(m) m[rev(1:nrow(m)), ]
norml <- function(v, minv=min(v), maxv=max(v)) (v-minv)/(maxv-minv)
to.zoo <- function(v) zoo(v[, -which(colnames(v) == "timestamp")], v$timestamp)

interval.sum.breaks <- function(v, breaks) {
  cs <- cumsum(v)
  intervals <- cs[breaks]
  c(head(intervals, 1), tail(intervals, -1)-head(intervals, -1)) 
}

vwap <- function(price, volume) as.numeric(price %*% volume / sum(volume))

interval.vwap <- function(price, volume, breaks) 
  interval.sum.breaks(price*volume, breaks)/interval.sum.breaks(volume, breaks)

# similarly, can define a cumulative definition of price.level.density:
interval.price.level.gaps <- 
  function(volume, breaks) interval.sum.breaks(ifelse(volume == 0, 1, 0), 
      breaks)

flatten.matrix <- function(m) 
  data.frame(timestamp=rep(m[, 1], 
             times=ncol(m)-1), 
             label=rep(names(m[, -1]), 
             each=nrow(m)), 
             val=unlist(m[, -1], 
             use.names=F))

