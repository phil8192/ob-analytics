vector.diff <- function(v) c(0, tail(v, -1) - head(v, -1))

reverse.matrix <- function(m) m[rev(1:nrow(m)), ]

norml <- function(v, minv=min(v), maxv=max(v)) (v - minv) / (maxv - minv)

to.zoo <- function(v) zoo(v[, -which(colnames(v) == "timestamp")], v$timestamp)

interval_sum_breaks <- function(v, breaks) {
  cs <- cumsum(v)
  intervals <- cs[breaks]
  c(head(intervals, 1), tail(intervals, -1) - head(intervals, -1)) 
}

vwap <- function(price, volume) as.numeric(price %*% volume / sum(volume))

interval.vwap <- function(price, volume, breaks) 
  interval_sum_breaks(price * volume, breaks) /
      interval_sum_breaks(volume, breaks)

# similarly, can define a cumulative definition of price.level.density:
interval_price_level_gaps <- 
  function(volume, breaks) interval_sum_breaks(ifelse(volume == 0, 1, 0), 
      breaks)

# logs to console in form of:
#     calling function => msg
logger <- function(msg) {
  caller <- head(sys.call(-1), 1)
  cat(paste0("    ", caller, " => ", msg, "\n"))
}
