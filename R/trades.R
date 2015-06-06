
# infer trades from event data.
# (event contains maker/taker)

# determine t&s with maker/taker match.
# timestamp = min(maker timestamp, taker timestamp) (first time we learned of this trade)
# price = maker price (taker limit price can cross the book.)
# volume = traded volume
# direction = buy or sell (side of the aggressor/taker
# maker event id
# taker event id
match.trades <- function(events) {

  print(paste("inferring trades from", nrow(events), "events..."))

  # trades with matching maker/taker.
  # align them by event id.
  matching.bids <- events[events$direction == "bid" & !is.na(events$matching.event), ]
  matching.bids <- matching.bids[order(matching.bids$event.id), ]
  matching.asks <- events[events$direction == "ask" & !is.na(events$matching.event), ]
  matching.asks <- matching.asks[order(matching.asks$matching.event), ]
  stopifnot(all(matching.bids$event.id - matching.asks$matching.event == 0))

  # makers/takers.
  # maker = exchange.timestamp < counterpart exchangetime.stamp
  if(all(matching.bids$exchange.timestamp != matching.asks$exchange.timestamp))
    warning("some bid timestamps == ask timestamps.")
  bid.maker <- matching.bids$exchange.timestamp <= matching.asks$exchange.timestamp

  # t&s timestamp is the first observation in the 2 matching trades.   
  timestamp <- as.POSIXct(ifelse(matching.bids$timestamp < matching.asks$timestamp, matching.bids$timestamp, matching.asks$timestamp), origin="1970-01-01", tz="UTC")

  # the price at which the trade occurred is the maker price.
  price <- ifelse(bid.maker, matching.bids$price, matching.asks$price)

  # volume is either side of trade
  volume <- matching.bids$fill

  # if bid is maker, trade was initiated by a seller.
  direction <- factor(ifelse(bid.maker, "sell", "buy"))

  # finally, maker+taker id.
  maker.event.id <- ifelse(bid.maker, matching.bids$event.id, matching.asks$event.id)
  taker.event.id <- ifelse(bid.maker, matching.asks$event.id, matching.bids$event.id)

  # return timestamp ordered series.
  combined <- data.frame(timestamp, price, volume, direction, maker.event.id, taker.event.id)
  combined[order(timestamp), ]

}

