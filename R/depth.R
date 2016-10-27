## Copyright (C) 2015,2016 Philip Stubbings <phil@parasec.net>
## Licensed under the GPL v2 license. See LICENSE.md for full terms.

##' Calculate priceLevelVolume (depth).
##'
##' Given a data.frame of limit order events, this function will calculate
##' the cumulative volume for each price level over time. Changes in volume
##' at a price level can occur when an a new order is added to the queue,
##' updated (partial fill) or deleted (execution or removal). The resulting
##' time series is of the form:
##'
##' \describe{
##'   \item{timestamp}{local time at which volume changed}
##'   \item{price}{price level at which volume changed}
##'   \item{volume}{amount of volume now remaining at this price level}
##'   \item{side}{the current side of the price level in the order book}
##' }
##'
##' @param events Limit order events.
##' @return Time series of liquidity for each price level in the order book.
##' @author phil
##' @keywords internal
priceLevelVolume <- function(events) {
  directionalPriceLevelVolume <- function(dir.events) {
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
    
  logger("calculating priceLevelVolume from bid event deltas...")
  bids <- events[events$direction == "bid", ]
  depth.bid <- directionalPriceLevelVolume(bids)
  logger("calculating priceLevelVolume from ask event deltas...")
  asks <- events[events$direction == "ask", ]
  depth.ask <- directionalPriceLevelVolume(asks)
  depth.data <- rbind(depth.bid, depth.ask)
    
  depth.data[order(depth.data$timestamp), ]
}

##' Filter price level volume.
##'
##' Given depth data calculated by \code{\link{priceLevelVolume}}, filter
##' between a specified time range. The resulting data will contain price level
##' volume which is active only within the specified time range.
##'
##' For price levels with volume > 0 before the time range starts, timestamps
##' will be set to the supplied \code{from} parameter.
##'
##' For volume > 0 after the time range ends, timestamps will be set to the
##' supplied \code{to} parameter and volume set to 0.
##'
##' For example, the following data taken from \code{\link{priceLevelVolume}}
##' for price level 243.29 shows the available volume through time at that
##' price level between \code{00:52:37.686} and \code{03:28:49.621}.
##' 
##' \tabular{rrrr}{
##'   timestamp               \tab price  \tab volume    \tab side \cr
##'   2015-05-01 00:52:37.686 \tab 243.29 \tab 911500000 \tab ask  \cr
##;   2015-05-01 01:00:33.111 \tab 243.29 \tab 0         \tab ask  \cr
##'   2015-05-01 01:00:36.243 \tab 243.29 \tab 862200000 \tab ask  \cr
##'   2015-05-01 02:45:43.052 \tab 243.29 \tab 0         \tab ask  \cr
##'   2015-05-01 02:52:24.063 \tab 243.29 \tab 614700000 \tab ask  \cr
##'   2015-05-01 02:52:51.413 \tab 243.29 \tab 0         \tab ask  \cr
##'   2015-05-01 02:53:13.904 \tab 243.29 \tab 952300000 \tab ask  \cr
##'   2015-05-01 03:28:49.621 \tab 243.29 \tab 0         \tab ask}
##'
##' applying \code{filterDepth} to this data for a time range beteen
##' \code{02:45} and \code{03:00} will result in the following:
##' 
##' \tabular{rrrr}{
##'   timestamp               \tab price  \tab volume    \tab side \cr
##'   2015-05-01 02:45:00.000 \tab 243.29 \tab 862200000 \tab ask  \cr
##'   2015-05-01 02:45:43.052 \tab 243.29 \tab 0         \tab ask  \cr
##'   2015-05-01 02:52:24.063 \tab 243.29 \tab 614700000 \tab ask  \cr
##'   2015-05-01 02:52:51.413 \tab 243.29 \tab 0         \tab ask  \cr
##'   2015-05-01 02:53:13.904 \tab 243.29 \tab 952300000 \tab ask  \cr
##'   2015-05-01 03:00:00.000 \tab 243.29 \tab 0         \tab ask}
##'
##' Note that the timestamps at the begining and end of the table have been
##' \emph{clamped} to the specified range and the volume set to 0 at the end.
##' 
##' @param d \code{\link{depth}} data.
##' @param from Beginning of range.
##' @param to End of range.
##' @return Filtered depth data.
##' @author phil
##' @examples
##'
##' # obtain price level volume for a 15 minute window.
##' filtered <- with(lob.data, filterDepth(depth,
##'     from=as.POSIXct("2015-05-01 02:45:00.000", tz="UTC"),
##'     to=as.POSIXct("2015-05-01 03:00:00.000", tz="UTC")))
##'
##' # top 5 most active price levels during this 15 minute window.
##' head(sort(tapply(filtered$volume, filtered$price, length),
##'     decreasing=TRUE), 5)
##'
##' # extract available volume for price level 233.78, then plot it.
##' level.233.78 <- filtered[filtered$price == 233.78, c("timestamp", "volume")]
##' plotTimeSeries(level.233.78$timestamp, level.233.78$volume*10^-8)
##' 
##' @export filterDepth
filterDepth <- function(d, from, to) {

  # 1. get all active price levels before start of range.  
  logger(paste("filterDepth between", from, "and", to))
  pre <- d[d$timestamp <= from, ]
  logger(paste("got", nrow(pre), "previous deltas"))
  pre <- pre[order(pre$price, pre$timestamp), ]
  logger(paste("ordered", nrow(pre), "previous deltas"))

  # last update for each price level <= from. this becomes the starting point 
  # for all updates within the range.
  pre <- pre[!duplicated(pre$price, fromLast=T) & pre$volume > 0, ] 
  logger(paste("extracted", nrow(pre), "previously updated deltas"))

  # clamp range (reset timestamp to from if price level active before start of
  # range.
  if(nrow(pre) > 0) {
    pre$timestamp <- as.POSIXct(sapply(pre$timestamp, function(r) {
      max(from, r)
    }), origin="1970-01-01", tz="UTC") 
    logger("clamped range.")
  }

  # 2. add all volume change within the range.
  mid <- d[d$timestamp > from & d$timestamp < to, ]
  logger(paste("got", nrow(mid), "in range deltas"))
  range <- rbind(pre, mid)
  logger(paste("appended range now contains", nrow(range), "deltas"))

  # 3. at the end of the range, set all price level volume to 0.
  open.ends <- data.frame(timestamp=to,
      range[(!duplicated(range$price, fromLast=T)) & range$volume > 0, -1])
  open.ends$volume <- 0

  # combine pre, mid and open.ends. ensure it is in order.  
  range <- rbind(range, open.ends)
  range <- range[order(range$price, range$timestamp), ]
  logger(paste("closed range. depth filtering resulted in", 
      length(unique(range$price)), "price levels."))

  range
}

##' Calculate order book summary statistics/metrics.
##'
##' This function calculates various summary statistics describing the state of
##' the limit order book after every event. The metrics are intended to quantify
##' the "shape" of the order book through time. Currenly the following metrics
##' are calculated:
##'
##' \preformatted{
##'   [timestamp,
##'    best.bid.price, best.bid.vol, bid.vol25:500bps,
##'    best.ask.price, best.ask.vol, ask.vol25:500bps,]
##'
##' where timestamp = time of order book state change
##'  best.bid.price = current best bid price
##'  best.bid.vol = current amount of volume at the best bid
##'  bid.vol25:500bps = amount of volume available > -25bps and <= best bid
##'                     until > 500bps <= 475bps.
##'    ... the same pattern is then repeated for the ask side.
##' }
##'
##' @param depth Price level cumulative depth calculated by priceLevelVolume()
##' @return data.frame containing order book summary statistics.
##' @author phil
##' @keywords internal
depthMetrics <- function(depth) {
  pb <- txtProgressBar(1, nrow(depth), 0, style=3)
  pctNames <- function(pct.name) paste0(pct.name, seq(from=25, to=500, by=25),
      "bps")
  ordered.depth <- depth[order(depth$timestamp), ]
  ordered.depth$price <- as.integer(round(100 * ordered.depth$price))
  depth.matrix <- cbind(ordered.depth$price, ordered.depth$volume, 
      ifelse(ordered.depth$side == "bid", 0, 1))

  metrics <- matrix(0, ncol=44, nrow=nrow(ordered.depth), 
      dimnames=list(1:nrow(ordered.depth),
          c("best.bid.price", "best.bid.vol", pctNames("bid.vol"),
            "best.ask.price", "best.ask.vol", pctNames("ask.vol"))))

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
        metrics[i, 23] <- best.ask
        metrics[i, 24] <- best.ask.vol
        metrics[i, 25:44] <- intervalSumBreaks(volume.range, breaks)
        # copy last bid data (no need to re-calculate it)
        if(i > 1) metrics[i, 1:22] <- metrics[i - 1, 1:22]
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
        metrics[i, 3:22] <- intervalSumBreaks(volume.range, breaks)
        # copy last ask data (no need to re-calculate it)
        if(i > 1) metrics[i, 23:44] <- metrics[i - 1, 23:44]
      } else {
        # copy last data (no change)
        if(i > 1) metrics[i, ] <- metrics[i - 1, ]
      }
    }
    setTxtProgressBar(pb, i)
  }

  # back into $  
  res <- cbind(timestamp=ordered.depth$timestamp, data.frame(metrics))
  keys <- c("best.bid.price", "best.ask.price")
  res[, keys] <- round(0.01*res[, keys], 2)

  res

}

##' Get the spread.
##'
##' Extracts the spread from the depth summary, removing any points in which a
##' change to bid/ask price/volume did not occur.
##'
##' The spread (best bid and ask price) will change following a market order or
##' upon the addition/cancellation of a limit order at, or within, the range of
##' the current best bid/ask. A change to the spread that is \emph{not} the
##' result of a market order (an impact/market shock) is known as a
##' \emph{quote}.
##'
##' The following table shows a market spread betwen \code{05:03:22.546} and
##' \code{05:04:42.957}. During this time, the best ask price and volume changes
##' whilst the best bid price and volume remains static.
##' 
##' \tabular{rrrrr}{
##'   timestamp    \tab bid.price \tab bid.vol  \tab ask.price \tab ask.vol \cr
##'   05:03:22.546 \tab 235.45    \tab 16235931 \tab 235.72    \tab 39375160 \cr
##'   05:03:24.990 \tab 235.45    \tab 16235931 \tab 235.72    \tab 21211607 \cr
##'   05:03:25.450 \tab 235.45    \tab 16235931 \tab 235.71    \tab 39375160 \cr
##'   05:04:15.477 \tab 235.45    \tab 16235931 \tab 235.72    \tab 39058160 \cr
##'   05:04:16.670 \tab 235.45    \tab 16235931 \tab 235.71    \tab 39058160 \cr
##'   05:04:42.957 \tab 235.45    \tab 16235931 \tab 235.71    \tab 77019160}
##' 
##' @param depth.summary \code{\link{depth.summary}} data.
##' @return Bid/Ask spread quote data.
##' @author phil
##' @examples
##'
##' # get the last 25 quotes (changes to the spread).
##' with(lob.data, tail(getSpread(depth.summary), 25))
##'
##' @export getSpread
getSpread <- function(depth.summary) {
  spread <- depth.summary[, c("timestamp",
                              "best.bid.price", "best.bid.vol",
                              "best.ask.price", "best.ask.vol")]

  changes <- (diff(spread$best.bid.price) != 0
            | diff(spread$best.bid.vol)   != 0
            | diff(spread$best.ask.price) != 0
            | diff(spread$best.ask.vol)   != 0)
    
  spread[c(T, changes), ]   
}

