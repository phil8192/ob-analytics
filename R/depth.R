# calculate price level volume (depth)

price_level_volume <- function(events) {
  logger("calculating price level volume from bid event deltas...")
  bids <- events[events$direction == "bid", ]
  depth_bid <- directional_price_level_volume(bids)
  logger("calculating price level volume from ask event deltas...")
  asks <- events[events$direction == "ask", ]
  depth_ask <- directional_price_level_volume(asks)
  depth_data <- rbind(depth_bid, depth_ask)
  depth_data[order(depth_data$timestamp), ]
}

directional_price_level_volume <- function(events) {
  cols <- c("event.id", "id", "timestamp", "exchange.timestamp", "price", 
      "volume", "direction", "action")
  added_volume <- events[(events$action == "created" 
      | (events$action == "changed" & events$fill == 0)) 
      & events$type != "pacman" & events$type != "market", cols]
  cancelled_volume <- events[(events$action == "deleted" 
      & events$volume > 0) & events$type != "pacman" & events$type != "market", 
      cols]
  cancelled_volume$volume <- -cancelled_volume$volume
  # remove deletes with no previous add.
  cancelled_volume <- cancelled_volume[cancelled_volume$id %in% 
      added_volume$id, ]
  filled_volume <- events[events$fill > 0 & events$type != "pacman" 
      & events$type != "market",
      c("event.id", "id", "timestamp", "exchange.timestamp", "price", "fill", 
      "direction", "action")]
  filled_volume$fill <- -filled_volume$fill
  # remove fills with no previous add.
  filled_volume <- filled_volume[filled_volume$id %in% added_volume$id, ]
  colnames(filled_volume) <- cols
  volume_deltas <- rbind(added_volume, cancelled_volume, filled_volume)

  volume_deltas <- volume_deltas[order(volume_deltas$price, 
      volume_deltas$timestamp), ]
  # ^^-- so price level deltas are now in order and order life-cycles can 
  # overlap..

  cum_volume <- unlist(tapply(volume_deltas$volume, volume_deltas$price, 
      function(volume) cumsum(volume)), use_names=F)

  # this can happen with missing data...
  cum_volume <- ifelse(cum_volume < 0, 0, cum_volume)

  cbind(volume_deltas[, c("timestamp", "price")], volume=cum_volume, 
      side=volume_deltas$direction)
}

# <ref: plot.price.levels.faster
# depth level changes between a range.
# timestamp of last depth level change < begining of range shifted forward to 
# edge of begining.
filter_depth <- function(depth, from, to) {
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
  price_levels <- unique(range$price)
  # last side of each price level:
  range <- range[order(range$price, range$timestamp), ]
  last_sides <- range[!duplicated(range$price, fromLast=T), "side"]
  range <- rbind(range, data.frame(timestamp=to, price=price_levels, volume=0, 
      side=last_sides))
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
depth_metrics <- function(depth) {
  pb <- txtProgressBar(1, nrow(depth), 0, style=3)
  pct_names <- function(pct_name) paste0(pct_name, seq(from=25, to=500, by=25), 
      "bps")
  ordered_depth <- depth[order(depth$timestamp), ]
  ordered_depth$price <- as.integer(round(100*ordered_depth$price))
  depth_matrix <- cbind(ordered_depth$price, ordered_depth$volume, 
      ifelse(ordered_depth$side == "bid", 0, 1))
  metrics <- matrix(0, ncol=124, nrow=nrow(ordered_depth), 
      dimnames=list(1:nrow(ordered_depth), 
      c("best.bid.price", "best.bid.vol", 
       pct.names("bid.vol"), pct.names("bid.vwap"), pct.names("bid.gap"),
       "best.ask.price", "best.ask.vol", 
       pct.names("ask.vol"), pct.names("ask.vwap"), pct.names("ask.gap"))))
  # the volume state for all price level depths. (updated in loop)
  asks_state <- integer(length=1000000)
  asks_state[1000000] <- 1 # trick (so there is an initial best ask)
  bids_state <- integer(length=1000000)
  bids_state[1] <- 1 # trick
  # initial best bid/ask
  best_ask <- max(ordered_depth[ordered_depth$side=="ask", ]$price)
  best_bid <- min(ordered_depth[ordered_depth$side=="bid", ]$price)
  best_ask_vol <- 0
  best_bid_vol <- 0
  for(i in 1:(nrow(ordered_depth))) {  
    depth_row <- depth_matrix[i, ]
    price <- depth_row[1]
    volume <- depth_row[2]
    side <- depth_row[3]
    if(side > 0) { # ask
      if(price > best_bid) {
        asks_state[price] <- volume
        if(volume > 0) {
          if(price < best_ask) {
            best_ask <- price
            best_ask_vol <- volume
          } else if(price == best_ask) {
            best_ask_vol <- volume
          }
        } else {
          if(price == best_ask) {
            best_ask <- head(which(asks_state > 0), 1)
            best_ask_vol <- asks_state[best_ask]
          }
        }
        price_range <- best_ask:round(1.05*best_ask)
        volume_range <- asks_state[price_range]
        breaks <- ceiling(cumsum(rep(length(price_range)/20, 20)))
        metrics[i, 63] <- best_ask
        metrics[i, 64] <- best_ask_vol
        metrics[i, 65:84] <- interval_sum_breaks(volume_range, breaks)
        metrics[i, 85:104] <- interval_vwap(price_range, volume_range, breaks)
        metrics[i, 105:124] <- interval_price_level_gaps(volume_range, breaks)
        # copy last bid data (no need to re-calculate it)
        if(i > 1) metrics[i, 1:62] <- metrics[i-1, 1:62]
      } else {
        # copy last data (no change)
        if(i > 1) metrics[i, ] <- metrics[i-1, ]
      }
    } else {
      if(price < best_ask) {
        bids_state[price] <- volume
        if(volume > 0) {
          if(price > best_bid) {
            best_bid <- price
          } else if(price == best_bid) {
            best_bid_vol <- volume
          }
        } else {
          if(price == best_bid) {
            best_bid <- tail(which(bids_state > 0), 1)
            best_bid_vol <- bids_state[best_bid]
          }
        }
        price_range <- best_bid:round(0.95*best_bid)
        volume_range <- bids_state[price_range]
        breaks <- ceiling(cumsum(rep(length(price_range)/20, 20)))
        metrics[i, 1] <- best_bid
        metrics[i, 2] <- best_bid_vol
        metrics[i, 3:22] <- interval_sum_breaks(volume_range, breaks)
        metrics[i, 23:42] <- interval_vwap(price_range, volume_range, breaks)
        metrics[i, 43:62] <- interval_price_level_gaps(volume_range, breaks)
        # copy last ask data (no need to re-calculate it)
        if(i > 1) metrics[i, 63:124] <- metrics[i-1, 63:124]
      } else {
        # copy last data (no change)
        if(i > 1) metrics[i, ] <- metrics[i-1, ]
      }
    }
    setTxtProgressBar(pb, i)
  }
  cbind(timestamp=ordered_depth$timestamp, data.frame(metrics))
}
