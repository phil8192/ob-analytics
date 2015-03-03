as.bps <- function(pct) as.integer(round(pct*10000))
as.pct <- function(bps) bps/10000
vector.diff <- function(v) c(0, tail(v, -1) - head(v, -1))
reverse.matrix <- function(m) m[rev(1:nrow(m)), ]
# 0:1
norml <- function(v, minv=min(v), maxv=max(v)) (v-minv)/(maxv-minv)
# x:y
norml.range <- function(v, from, to, minv=min(v), maxv=max(v)) (((to-from)*(v-minv))/(maxv-minv))+from
# mean 0, sd 1.
standardise <- function(v, m=mean(v), s=sd(v)) (v-m)/s


to.zoo <- function(v) zoo(v[, -which(colnames(v) == "timestamp")], v$timestamp)

# return a vector containing the sym of values between n.break intervals.
# for example, given a vector v <- 1:1000
# intervalus.sum(v, 4) = 31375  93875 156375 218875, where 218875 = sum(v[751:1000).
# such that, sum(interval.sum(v,4)) == sum(v)
interval.sum <- function(v, n.breaks) {
  breaks <- ceiling(cumsum(rep(length(v)/n.breaks, n.breaks)))
  interval.sum.breaks(v, breaks)
}

interval.sum.breaks <- function(v, breaks) {
  cs <- cumsum(v)
  intervals <- cs[breaks]
  c(head(intervals, 1), tail(intervals, -1)-head(intervals, -1)) 
}

# sum((price*volume))/sum(volume)
vwap <- function(price, volume) as.numeric(price %*% volume / sum(volume))

# note that for:
# length(price) == length(volume), and for 5 points:
# all(cumsum((price*volume))/cumsum(volume) == sapply(1:5, function(i) vwap(price[1:i], volume[1:i]))) == T
# as such:
cum.vwap <- function(price, volume) cumsum((price*volume))/cumsum(volume)
interval.vwap <- function(price, volume, breaks) interval.sum.breaks(price*volume, breaks)/interval.sum.breaks(volume, breaks)

# for a range of price levels, for example 100 cents between 1 and 2 dollars, 
# how many "gaps" exist? 
price.level.gaps <- function(volume) length(which(volume == 0))

# similarly, can define a cumulative definition of price.level.density:
cum.price.level.gaps <- function(volume) cumsum(ifelse(volume == 0, 1, 0))
interval.price.level.gaps <- function(volume, breaks) interval.sum.breaks(ifelse(volume == 0, 1, 0), breaks)

# pacman impacts
pacman.impacts <- function(events) {
  pacmen <- events[events$type == "pacman", ]
  tapply(pacmen$price, pacmen$id, function(price.updates) {
    price.updates <- tail(price.updates, -1)
    (tail(price.updates, 1)-head(price.updates, 1))/head(price.updates, 1)
  })
}

# assuming timestamp is first column, flatten matrix into
# timestamp, label, val; where label = colnames excluding timestamp, of matrix.
# e.g.,
# > m
#                      timestamp bid.vwap25bps bid.vwap50bps
# 367365 2014-10-10 23:59:58.329      35924.24      35858.10
# 367366 2014-10-10 23:59:58.345      35924.24      35858.10
# 367367 2014-10-10 23:59:58.355      35924.24      35858.10
# 367368 2014-10-10 23:59:58.634      35924.24      35858.10
# 367369 2014-10-10 23:59:59.566      35924.24      35858.10
# 367370 2014-10-10 23:59:59.578      35924.24      35857.81
#
# > flatten.matrix(m)
#                  timestamp         label      val
# 1  2014-10-10 23:59:58.329 bid.vwap25bps 35924.24
# 2  2014-10-10 23:59:58.345 bid.vwap25bps 35924.24
# 3  2014-10-10 23:59:58.355 bid.vwap25bps 35924.24
# 4  2014-10-10 23:59:58.634 bid.vwap25bps 35924.24
# 5  2014-10-10 23:59:59.566 bid.vwap25bps 35924.24
# 6  2014-10-10 23:59:59.578 bid.vwap25bps 35924.24
# 7  2014-10-10 23:59:58.329 bid.vwap50bps 35858.10
# 8  2014-10-10 23:59:58.345 bid.vwap50bps 35858.10
# 9  2014-10-10 23:59:58.355 bid.vwap50bps 35858.10
# 10 2014-10-10 23:59:58.634 bid.vwap50bps 35858.10
# 11 2014-10-10 23:59:59.566 bid.vwap50bps 35858.10
# 12 2014-10-10 23:59:59.578 bid.vwap50bps 35857.81
flatten.matrix <- function(m) {
  data.frame(timestamp=rep(m[, 1], times=ncol(m)-1), label=rep(names(m[, -1]), each=nrow(m)), val=unlist(m[, -1], use.names=F))
} 

# todo:
impact <- function(order.id) {
  order.data <- events[events$id == order.id, ]
  matching.events <- order.data$matching.event 
  matching.events <- matching.events[!is.na(matching.events)]
  matching.orders <- events[events$event.id %in% matching.events, ]

}


# time buckets

between <- function(m, from, to) m[m$timestamp >= from & m$timestamp < to, ] 

time.bins <- function(date.s="1970-01-01", secs=60, tz="UTC") {
  from <- as.POSIXct(paste(date.s, "00:00:00.000"), tz=tz)
  to <- as.POSIXct(paste(date.s, "23:59:59.999"), tz=tz)
  by <- paste(secs, "secs")
  seq(from, to, by)  
}

# usage:
# seconds.apply(sell.impacts, 3600, function(bin) with(bin, list(impacts=nrow(bin), hits=sum(hits), vol=sum(vol)))) 
# seconds.apply(trades, 3600, function(bin) with(bin, list(open=head(price,1), high=max(price), low=min(price), close=tail(price,1))))
seconds.apply <- function(m, secs=60, fun) {
  date.s <- format(head(m$timestamp, 1), "%Y-%m-%d")
  bins <- time.bins(date.s, secs)
  res <- lapply(bins, function(tp) fun(between(m, tp, tp+secs)))  
  data.frame(timestamp=trunc(bins+(secs-0.001), "secs"), do.call("rbind.data.frame", res))
}


