##' Calculate price level volume (depth).
##'
##' Given a data.frame of limit order events, this function will calculate
##' the cumulative volume for each price level over time. Changes in volume
##' at a price level can occur when an a new order is added to the queue,
##' updated (partial fill) or deleted (execution or removal). The resulting
##' time series is of the form:
##'     [timestamp, price, volume, side]
##' where timestamp = (local) time at which liquidity changed
##'           price = price level at which liquidity changed
##'          volume = amount of volume remaining at this price level
##'            side = the (current) side of the price level in the order book.
##' 
##' @param events Limit order events.
##' @return Time series of liquidity for each price level in the order book.
##' @author phil
price.level.volume <- function(events) {
  directional.price.level.volume <- function(dir.events) {
    cols <- c("event.id", "id", "timestamp", "exchange.timestamp", "price", 
        "volume", "direction", "action")
    added.volume <- dir.events[(dir.events$action == "created" 
        | (dir.events$action == "changed" & dir.events$fill == 0)) 
        & dir.events$type != "pacman" & dir.events$type != "market", cols]
    cancelled.volume <- dir.events[(dir.events$action == "deleted" 
        & dir.events$volume > 0) & dir.events$type != "pacman"
        & dir.events$type != "market", cols]
    cancelled.volume$volume <- -cancelled.volume$volume
    # remove deletes with no previous add.
    cancelled.volume <- cancelled.volume[cancelled.volume$id %in% 
        added.volume$id, ]
    filled.volume <- dir.events[dir.events$fill > 0
        & dir.events$type != "pacman" 
        & dir.events$type != "market",
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
    
  logger("calculating price level volume from bid event deltas...")
  bids <- events[events$direction == "bid", ]
  depth.bid <- directional.price.level.volume(bids)
  logger("calculating price level volume from ask event deltas...")
  asks <- events[events$direction == "ask", ]
  depth.ask <- directional.price.level.volume(asks)
  depth.data <- rbind(depth.bid, depth.ask)
    
  depth.data[order(depth.data$timestamp), ]
}

##' Calculate order book summary statistics/metrics.
##'
##' This function calculates various summary statistics describing the state of
##' the limit order book after every event. The metrics are intended to quantify
##' the "shape" of the order book through time. Currenly the following metrics
##' are calculated:
##'
##'   [timestamp,
##'    best.bid.price, best.bid.vol,
##'    bid.vol25:500bps, bid.vwap25:500bps, bid.gap25:500bps,
##'    best.ask.price, best.ask.vol,
##'    ask.vol25:500bps, ask.vwap25:500bps, ask.gap25:500bps]
##'
##' where timestamp = time of order book state change
##'  best.bid.price = current best bid price
##'  best.bid.vol = current amount of volume at the best bid
##'  bid.vol25:500bps = amount of volume available > -25bps and <= best bid
##'                     until > 500bps <= 475bps.
##'  bid.vwap25:500bps = VWAP > -25bps and <= best bid
##'                     until > 500bps <= 475bps.
##'  bid.gap25:500bps = number of vacant price levels > -25bps and <= best bid
##'                     until > 500bps <= 475bps. 0 = all available price levels
##'                     occupied (maxium density).
##'    ... the same pattern is then repeated for the ask side.
##'
##' TODO: just use mean diff for gap/density summary.
##' TODO: very inneficient implementation: vectorise or use rcpp.
##' TODO: additional summary statistics.
##' 
##' @param depth Price level cumulative depth calculated by price.level.volume()
##' @return data.frame containing order book summary statistics.
##' @author phil
depth.metrics <- function(depth) {
  pb <- txtProgressBar(1, nrow(depth), 0, style=3)
  pct.names <- function(pct.name) paste0(pct.name, seq(from=25, to=500, by=25), 
      "bps")
  ordered.depth <- depth[order(depth$timestamp), ]
  ordered.depth$price <- as.integer(round(100 * ordered.depth$price))
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
  best.ask <- max(ordered.depth[ordered.depth$side == "ask", ]$price)
  best.bid <- min(ordered.depth[ordered.depth$side == "bid", ]$price)
  best.ask.vol <- 0
  best.bid.vol <- 0
  for(i in 1:(nrow(ordered.depth))) {
    depth.row <- depth.matrix[i, ]
    price <- depth.row[1]
    volume <- depth.row[2]
    side <- depth.row[3]
    # ask
    if(side > 0) {
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
        price.range <- best.ask:round(1.05 * best.ask)
        volume.range <- asks.state[price.range]
        breaks <- ceiling(cumsum(rep(length(price.range) / 20, 20)))
        metrics[i, 63] <- best.ask
        metrics[i, 64] <- best.ask.vol
        metrics[i, 65:84] <- interval.sum.breaks(volume.range, breaks)
        metrics[i, 85:104] <- interval.vwap(price.range, volume.range, breaks)
        metrics[i, 105:124] <- interval.price.level.gaps(volume.range, breaks)
        # copy last bid data (no need to re-calculate it)
        if(i > 1) metrics[i, 1:62] <- metrics[i - 1, 1:62]
      } else {
        # copy last data (no change)
        if(i > 1) metrics[i, ] <- metrics[i - 1, ]
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
        price.range <- best.bid:round(0.95 * best.bid)
        volume.range <- bids.state[price.range]
        breaks <- ceiling(cumsum(rep(length(price.range) / 20, 20)))
        metrics[i, 1] <- best.bid
        metrics[i, 2] <- best.bid.vol
        metrics[i, 3:22] <- interval.sum.breaks(volume.range, breaks)
        metrics[i, 23:42] <- interval.vwap(price.range, volume.range, breaks)
        metrics[i, 43:62] <- interval.price.level.gaps(volume.range, breaks)
        # copy last ask data (no need to re-calculate it)
        if(i > 1) metrics[i, 63:124] <- metrics[i - 1, 63:124]
      } else {
        # copy last data (no change)
        if(i > 1) metrics[i, ] <- metrics[i - 1, ]
      }
    }
    setTxtProgressBar(pb, i)
  }
  cbind(timestamp=ordered.depth$timestamp, data.frame(metrics))
}
