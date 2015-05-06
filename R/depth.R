# calculate price level volume (depth)

price.level.volume <- function(events) {
  logger("calculating price level volume from bid event deltas...")
  bids <- events[events$direction == "bid", ]
  depth.bid <- directional.price.level.volume(bids)
  logger("calculating price level volume from ask event deltas...")
  asks <- events[events$direction == "ask", ]
  depth.ask <- directional.price.level.volume(asks)
  depth.data <- rbind(depth.bid, depth.ask)
  depth.data[order(depth.data$timestamp), ]
}

directional.price.level.volume <- function(events) {
  cols <- c("event.id", "id", "timestamp", "exchange.timestamp", "price", 
      "volume", "direction", "action")
  added.volume <- events[(events$action == "created" 
      | (events$action == "changed" & events$fill == 0)) 
      & events$type != "pacman" & events$type != "market", cols]
  cancelled.volume <- events[(events$action == "deleted" 
      & events$volume > 0) & events$type != "pacman" & events$type != "market", 
      cols]
  cancelled.volume$volume <- -cancelled.volume$volume
  # remove deletes with no previous add.
  cancelled.volume <- cancelled.volume[cancelled.volume$id %in% 
      added.volume$id, ]
  filled.volume <- events[events$fill > 0 & events$type != "pacman" 
      & events$type != "market",
      c("event.id", "id", "timestamp", "exchange.timestamp", "price", "fill", 
      "direction", "action")]
  filled.volume$fill <- -filled.volume$fill
  # remove fills with no previous add.
  filled.volume <- filled.volume[filled.volume$id %in% added.volume$id, ]
  colnames(filled.volume) <- cols
  volume.deltas <- rbind(added.volume, cancelled.volume, filled.volume)

  volume.deltas <- volume.deltas[order(volume.deltas$price, 
      volume.deltas$timestamp), ]
  # ^^-- so price level deltas are now in order and order life-cycles can 
  # overlap..

  cum.volume <- unlist(tapply(volume.deltas$volume, volume.deltas$price, 
      function(volume) cumsum(volume)), use.names=F)

  # this can happen with missing data...
  cum.volume <- ifelse(cum.volume < 0, 0, cum.volume)

  cbind(volume.deltas[, c("timestamp", "price")], volume=cum.volume, 
      side=volume.deltas$direction)
}

# given price level volume events, (output of price.level.volume function), 
# truncate to seconds, fill in gaps between volume events (like a run length 
# encoding). very very slow. 
depth.levels <- function(price.level.events, 
    from=as.POSIXct("1970-01-01 00:00:00.000", tz="UTC"), 
    progress=function(i) logger(paste("processing", i))) {

  # align volume events to ticks
  price.series <- function(price.events, x.ticks) {
    price.events.zoo <- zoo(price.events[, c("price", "volume")], 
        price.events$timestamp)
    filled <- na.locf(price.events.zoo, xout=x.ticks)
    filled <- filled[index(filled) >= first(price.events$timestamp)]
    # optional....
    filled <- filled[filled$volume > 0, ]
    data.frame(timestamp=index(filled), filled, row.names=NULL)
  }

  if(!is.null(price.level.events) && nrow(price.level.events) > 0) {
    y.level <- sort(unique(price.level.events$price))
    x.range <- trunc(range(price.level.events$timestamp), "secs")
    x.ticks <- seq(x.range[1], x.range[2], by="1 sec")

    price.level.events$timestamp <- trunc(price.level.events$timestamp, "secs")
    price.level.events <- price.level.events[!duplicated(price.level.events[, 
        c("timestamp", "price")], fromLast=T), ]

    do.call(rbind, lapply(y.level, function(pl) {
      progress(pl)
      ps <- price.level.events[price.level.events$price == pl, ]
      ps <- price.series(ps, x.ticks)
      ps[ps$timestamp >= from, ]
    }))
  }
}

# <ref: plot.price.levels.faster
# depth level changes between a range.
# timestamp of last depth level change < begining of range shifted forward to 
# edge of begining.
filter.depth <- function(depth, from, to) {
  logger(paste("filter depth between", from, "and", to))
  pre <- depth[depth$timestamp <= from, ]
  logger(paste("got", nrow(pre), "previous deltas"))
  pre <- pre[order(pre$price, pre$timestamp), ]
  logger(paste("ordered", nrow(pre), "previous deltas"))
  # last update for each price level <= from. this becomes the starting point 
  # for all updates within the range.
  pre <- pre[!duplicated(pre$price, fromLast=T) & pre$volume > 0, ] 
  logger(paste("extracted", nrow(pre), "previously updated deltas"))
  # clamp range
  if(nrow(pre) > 0) {
    pre$timestamp <- as.POSIXct(sapply(pre$timestamp, function(r) max(from, r)), 
        origin="1970-01-01", tz="UTC") 
    logger("clamped range.")
  }
  mid <- depth[depth$timestamp > from & depth$timestamp < to, ]
  logger(paste("got", nrow(mid), "in range deltas"))
  range <- rbind(pre, mid)
  logger(paste("appended range now contains", nrow(range), "deltas"))
  # close off loose ends.
  price.levels <- unique(range$price)
  # last side of each price level:
  range <- range[order(range$price, range$timestamp), ]
  last.sides <- range[!duplicated(range$price, fromLast=T), "side"]
  range <- rbind(range, data.frame(timestamp=to, price=price.levels, volume=0, 
      side=last.sides))
  # ensure it is in order
  range <- range[order(range$price, range$timestamp), ]
  logger(paste("closed range. depth filtering resulted in", 
      length(unique(range$price)), "price levels."))
  range
}

# todo: this is dog slow. needs some work.

# pre-process order depth metrics.
# returns a matrix of the form:
# [timestamp, best bid price, best bid volume, volume at 20 percentile 
# increments below best bid in 25bps bins: best.bid:(best.bid*0.95). vwap in 20 
# percentile bins, #price gaps in 20 percentile increments.. , same repeated for 
# ask, except: best.ask:(best.ask*1.05). ]
depth.metrics <- function(depth) {
  pb <- txtProgressBar(1, nrow(depth), 0, style=3)
  pct.names <- function(pct.name) paste0(pct.name, seq(from=25, to=500, by=25), 
      "bps")
  ordered.depth <- depth[order(depth$timestamp), ]
  ordered.depth$price <- as.integer(round(100*ordered.depth$price))
  depth.matrix <- cbind(ordered.depth$price, ordered.depth$volume, 
      ifelse(ordered.depth$side == "bid", 0, 1))
  metrics <- matrix(0, ncol=124, nrow=nrow(ordered.depth), 
      dimnames=list(1:nrow(ordered.depth), 
      c("best.bid.price", "best.bid.vol", 
       pct.names("bid.vol"), pct.names("bid.vwap"), pct.names("bid.gap"),
       "best.ask.price", "best.ask.vol", 
       pct.names("ask.vol"), pct.names("ask.vwap"), pct.names("ask.gap"))))
  # the volume state for all price level depths. (updated in loop)
  asks.state <- integer(length=1000000)
  asks.state[1000000] <- 1 # trick (so there is an initial best ask)
  bids.state <- integer(length=1000000)
  bids.state[1] <- 1 # trick
  # initial best bid/ask
  best.ask <- max(ordered.depth[ordered.depth$side=="ask", ]$price)
  best.bid <- min(ordered.depth[ordered.depth$side=="bid", ]$price)
  best.ask.vol <- 0
  best.bid.vol <- 0
  for(i in 1:(nrow(ordered.depth))) {  
    depth.row <- depth.matrix[i, ]
    price <- depth.row[1]
    volume <- depth.row[2]
    side <- depth.row[3]
    if(side > 0) { # ask
      if(price > best.bid) {
        asks.state[price] <- volume
        if(volume > 0) {
          if(price < best.ask) {
            best.ask <- price
            best.ask.vol <- volume
          } else if(price == best.ask) {
            best.ask.vol <- volume
          }
        } else {
          if(price == best.ask) {
            best.ask <- head(which(asks.state > 0), 1)
            best.ask.vol <- asks.state[best.ask]
          }
        }
        price.range <- best.ask:round(1.05*best.ask)
        volume.range <- asks.state[price.range]
        breaks <- ceiling(cumsum(rep(length(price.range)/20, 20)))
        metrics[i, 63] <- best.ask
        metrics[i, 64] <- best.ask.vol
        metrics[i, 65:84] <- interval.sum.breaks(volume.range, breaks)
        metrics[i, 85:104] <- interval.vwap(price.range, volume.range, breaks)
        metrics[i, 105:124] <- interval.price.level.gaps(volume.range, breaks)
        # copy last bid data (no need to re-calculate it)
        if(i > 1) metrics[i, 1:62] <- metrics[i-1, 1:62]
      } else {
        # copy last data (no change)
        if(i > 1) metrics[i, ] <- metrics[i-1, ]
      }
    } else {
      if(price < best.ask) {
        bids.state[price] <- volume
        if(volume > 0) {
          if(price > best.bid) {
            best.bid <- price
          } else if(price == best.bid) {
            best.bid.vol <- volume
          }
        } else {
          if(price == best.bid) {
            best.bid <- tail(which(bids.state > 0), 1)
            best.bid.vol <- bids.state[best.bid]
          }
        }
        price.range <- best.bid:round(0.95*best.bid)
        volume.range <- bids.state[price.range]
        breaks <- ceiling(cumsum(rep(length(price.range)/20, 20)))
        metrics[i, 1] <- best.bid
        metrics[i, 2] <- best.bid.vol
        metrics[i, 3:22] <- interval.sum.breaks(volume.range, breaks)
        metrics[i, 23:42] <- interval.vwap(price.range, volume.range, breaks)
        metrics[i, 43:62] <- interval.price.level.gaps(volume.range, breaks)
        # copy last ask data (no need to re-calculate it)
        if(i > 1) metrics[i, 63:124] <- metrics[i-1, 63:124]
      } else {
        # copy last data (no change)
        if(i > 1) metrics[i, ] <- metrics[i-1, ]
      }
    }
    setTxtProgressBar(pb, i)
  }
  cbind(timestamp=ordered.depth$timestamp, data.frame(metrics))
}

