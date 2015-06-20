# infer trades from event data.
# (event contains maker/taker)

# determine t&s with maker/taker match.
# timestamp = min(maker timestamp, taker timestamp) (first time we learned of this trade)
# price = maker price (taker limit price can cross the book.)
# volume = traded volume
# direction = buy or sell (side of the aggressor/taker)
# maker event id
# taker event id
match.trades <- function(events) {

  logger(paste("inferring trades from", nrow(events), "events..."))

  # trades with matching maker/taker.
  # align them by event id.
  matching.bids <- events[events$direction == "bid" & 
      !is.na(events$matching.event), ]
  matching.bids <- matching.bids[order(matching.bids$event.id), ]
  matching.asks <- events[events$direction == "ask" & 
      !is.na(events$matching.event), ]
  matching.asks <- matching.asks[order(matching.asks$matching.event), ]
  stopifnot(all(matching.bids$event.id - matching.asks$matching.event == 0))
    
  # makers/takers. (bid is maker if it comes first.
  # coming first is determined by exchange timestamp and if == then falls back
  # to order id.
  bid.exchange.ts <- matching.bids$exchange.timestamp
  ask.exchange.ts <- matching.asks$exchange.timestamp
  bid.maker <- bid.exchange.ts < ask.exchange.ts | 
      ((bid.exchange.ts == ask.exchange.ts)) &
      (matching.bids$id < matching.asks$id)

  bid.local.ts <- matching.bids$timestamp
  ask.local.ts <- matching.asks$timestamp
  # t&s timestamp is the first observation in the 2 matching trades.   
  timestamp <- as.POSIXct(ifelse(bid.local.ts <= ask.local.ts,
                                 bid.local.ts, ask.local.ts), 
      origin="1970-01-01", tz="UTC")

  # the price at which the earlier timestamp. 
  price <- ifelse(bid.maker, matching.bids$price, matching.asks$price)

  # volume is either side of trade
  volume <- matching.bids$fill

  # if bid is maker, trade was initiated by a seller.
  direction <- factor(ifelse(bid.maker, "sell", "buy"))

  # finally, maker+taker id.
  maker.event.id <- ifelse(bid.maker, matching.bids$event.id, 
      matching.asks$event.id)
  taker.event.id <- ifelse(bid.maker, matching.asks$event.id, 
      matching.bids$event.id)

  # return timestamp ordered series.
  combined <- data.frame(timestamp, price, volume, direction, maker.event.id, 
      taker.event.id)
  trades <- combined[order(timestamp), ]

  jumps <- length(which(abs(diff(trades$price))>5))
  if(jumps>0)
    warning(paste(format(head(events$timestamp,1),"%D"),":",jumps,"jumps >$5"))

  trades

}

